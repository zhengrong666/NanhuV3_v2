/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.issue.MemRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{ExuOutput, FuType, HasXSParameter, MicroOp, Redirect, SrcState, SrcType, XSCoreParamsKey}
import xiangshan.backend.execute.exu.ExuType
import xiangshan.backend.execute.fu.FuConfigs
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import utils.XSPerfHistogram
import xiangshan.backend.issue._
import xiangshan.backend.rename.BusyTable
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xiangshan.mem.SqPtr

object MemRsHelper {
  def WbToWkp(in:Valid[ExuOutput], p:Parameters):Valid[WakeUpInfo] = {
    val wkp = Wire(Valid(new WakeUpInfo()(p)))
    wkp.valid := in.valid
    wkp.bits.pdest := in.bits.uop.pdest
    wkp.bits.robPtr := in.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp.bits.destType := MuxCase(SrcType.default, Seq(
      in.bits.uop.ctrl.rfWen -> SrcType.reg,
      in.bits.uop.ctrl.fpWen -> SrcType.fp,
      in.bits.uop.ctrl.vdWen -> SrcType.vec
    ))
    wkp
  }
}

class MemoryReservationStation(implicit p: Parameters) extends LazyModule{
  private val entryNum = p(XSCoreParamsKey).memRsDepth
  private val wbNodeParam = WriteBackSinkParam(name = "Memory RS", sinkType = WriteBackSinkType.memRs)
  private val rsParam = RsParam(name = "Memory RS", RsType.mem, entryNum)
  require(entryNum % rsParam.bankNum == 0)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)
  val dispatchNode = new RsDispatchNode(rsParam)

  lazy val module = new MemoryReservationStationImpl(this, rsParam)
}

class MemoryReservationStationImpl(outer:MemoryReservationStation, param:RsParam) extends LazyModuleImp(outer) with HasXSParameter {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val rawIssue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2._2
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2._1)
  rawIssue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.memTypes.contains(elm0.exuType))))
  private val issue = rawIssue.filterNot(_._2.isSpecialLoad)
  private val specialLoadIssue = rawIssue.filter(_._2.isSpecialLoad)
  require(specialLoadIssue.length == 1)
  println("\nMemory Reservation Issue Ports Config:")
  for ((iss, issuePortIdx) <- rawIssue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
  }

  private val staIssue = issue.filter(_._2.hasSta)
  private val stdIssue = issue.filter(_._2.hasStd)
  private val lduIssue = issue.filter(_._2.hasLoad)

  private val staIssuePortNum = issue.count(_._2.hasSta)
  private val stdIssuePortNum = issue.count(_._2.hasStd)
  private val lduIssuePortNum = issue.count(_._2.hasLoad)

  require(staIssuePortNum == stdIssuePortNum)
  require(staIssue.nonEmpty && staIssue.length <= param.bankNum && (param.bankNum % staIssue.length) == 0)
  require(stdIssue.nonEmpty && stdIssue.length <= param.bankNum && (param.bankNum % stdIssue.length) == 0)
  require(lduIssue.nonEmpty && lduIssue.length <= param.bankNum && (param.bankNum % lduIssue.length) == 0)

  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val mulSpecWakeup = Input(Vec(p(XSCoreParamsKey).exuParameters.mulNum, Valid(new WakeUpInfo)))
    val aluJmpSpecWakeup = Input(Vec(p(XSCoreParamsKey).exuParameters.aluNum + p(XSCoreParamsKey).exuParameters.AluJmpCnt, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Output(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val stLastCompelet = Input(new SqPtr)
    val integerAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val floatingAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val vectorAllocPregs = Vec(coreParams.vectorParameters.vRenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
  })
  require(outer.dispatchNode.in.length == 1)
  private val enq = outer.dispatchNode.in.map(_._1).head

  private val aluJmpSpecWakeup = Wire(Vec(io.aluJmpSpecWakeup.length, Valid(new WakeUpInfo)))
  aluJmpSpecWakeup.zip(io.aluJmpSpecWakeup).foreach({case(a, b) =>
    val flush = b.bits.robPtr.needFlush(io.redirect)
    val canceled = b.bits.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
    a.valid := b.valid && !flush && !canceled
    a.bits := b.bits
    a.bits.lpv.foreach(_ := 0.U)
  })

  private val allWkpIns = wakeup.map(w => (MemRsHelper.WbToWkp(w._1, p), w._2))
  private val wkpToRsBank = allWkpIns.map(_._1) ++ io.mulSpecWakeup ++ aluJmpSpecWakeup
  private val specWkpLen = (io.mulSpecWakeup ++ aluJmpSpecWakeup).length
  private val wbWkpLen = allWkpIns.length
  private val regWkpIdx = allWkpIns.zipWithIndex.filter(_._1._2.writeIntRf).map(_._2) ++ Seq.tabulate(specWkpLen)(_ + wbWkpLen)
  private val fpWkpIdx = allWkpIns.zipWithIndex.filter(_._1._2.writeFpRf).map(_._2) ++ Seq.tabulate(io.mulSpecWakeup.length)(_ + wbWkpLen)
  private val vecWkpIdx = allWkpIns.zipWithIndex.filter(_._1._2.writeVecRf).map(_._2)

  private val stIssuedWires = Wire(Vec(staIssuePortNum, Valid(new RobPtr)))

  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new MemoryReservationBank(entriesNumPerBank, staIssuePortNum, wkpToRsBank.length, regWkpIdx, fpWkpIdx, vecWkpIdx))
    mod.io.redirect := io.redirect
    mod.io.wakeups := wkpToRsBank
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod.io.stIssued := stIssuedWires
    mod.io.stLastCompelet := RegNext(io.stLastCompelet)
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("MemoryAllocateNetwork")))

  private val regWkpIns = wakeup.filter(_._2.writeIntRf).map(_._1).map(MemRsHelper.WbToWkp(_, p))
  private val fpWkpIns = wakeup.filter(_._2.writeFpRf).map(_._1).map(MemRsHelper.WbToWkp(_, p))
  private val vecWkpIns = wakeup.filter(_._2.writeVecRf).map(_._1).map(MemRsHelper.WbToWkp(_, p))

  private val floatingBusyTable = Module(new BusyTable(param.bankNum, (fpWkpIns ++ io.mulSpecWakeup).length, RenameWidth))
  floatingBusyTable.io.allocPregs := io.floatingAllocPregs
  floatingBusyTable.io.wbPregs.zip(fpWkpIns ++ io.mulSpecWakeup).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.fp
    bt.bits := wb.bits.pdest
  })
  private val integerBusyTable = Module(new BusyTable(param.bankNum * 2, regWkpIns.length + io.mulSpecWakeup.length + aluJmpSpecWakeup.length, RenameWidth))
  integerBusyTable.io.allocPregs := io.integerAllocPregs
  integerBusyTable.io.wbPregs.zip(regWkpIns ++ aluJmpSpecWakeup ++ io.mulSpecWakeup).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.reg
    bt.bits := wb.bits.pdest
  })
  private val vectorBusyTable = Module(new BusyTable(param.bankNum * 3, vecWkpIns.length + io.mulSpecWakeup.length, coreParams.vectorParameters.vRenameWidth))
  vectorBusyTable.io.allocPregs := io.vectorAllocPregs
  vectorBusyTable.io.wbPregs.zip(vecWkpIns ++ io.mulSpecWakeup).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.vec
    bt.bits := wb.bits.pdest
  })

  private val staExuCfg = staIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.sta).head
  private val stdExuCfg = stdIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.std).head
  private val lduExuCfg = lduIssue.flatMap(_._2.exuConfigs).filter(_.exuType == ExuType.ldu).head

  private val staSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, staIssuePortNum, staExuCfg, true, true, Some(s"MemoryStaSelectNetwork")))
  private val stdSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, stdIssuePortNum, stdExuCfg, true, true, Some(s"MemoryStdSelectNetwork")))
  private val lduSelectNetwork = Module(new SelectNetwork(param.bankNum, entriesNumPerBank, lduIssuePortNum, lduExuCfg, true, true, Some(s"MemoryLduSelectNetwork")))

  staSelectNetwork.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
    sink := source.io.staSelectInfo
  })
  staSelectNetwork.io.earlyWakeUpCancel := io.earlyWakeUpCancel
  staSelectNetwork.io.redirect := io.redirect

  stdSelectNetwork.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
    sink := source.io.stdSelectInfo
  })
  stdSelectNetwork.io.earlyWakeUpCancel := io.earlyWakeUpCancel
  stdSelectNetwork.io.redirect := io.redirect

  lduSelectNetwork.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
    sink := source.io.lduSelectInfo
  })
  lduSelectNetwork.io.earlyWakeUpCancel := io.earlyWakeUpCancel
  lduSelectNetwork.io.redirect := io.redirect

  private var fpBusyTableReadIdx = 0
  private var intBusyTableReadIdx = 0
  private var vectorBusyTableReadIdx = 0
  allocateNetwork.io.enqFromDispatch.zip(enq).foreach({case(sink, source) =>
    val fdRead = floatingBusyTable.io.read(fpBusyTableReadIdx)
    val baseRead = integerBusyTable.io.read(intBusyTableReadIdx)
    val strdOrIdRead = integerBusyTable.io.read(intBusyTableReadIdx + 1)
    val offRead = vectorBusyTable.io.read(vectorBusyTableReadIdx + 0)
    val vdRead = vectorBusyTable.io.read(vectorBusyTableReadIdx + 1)
    val vmRead = vectorBusyTable.io.read(vectorBusyTableReadIdx + 2)
    val type0 = source.bits.ctrl.srcType(0)
    val type1 = source.bits.ctrl.srcType(1)
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := sink.ready

    baseRead.req := source.bits.psrc(0)
    sink.bits.srcState(0) := baseRead.resp

    strdOrIdRead.req := source.bits.psrc(1)
    fdRead.req := source.bits.psrc(1)
    offRead.req := source.bits.psrc(1)
    sink.bits.srcState(1) := MuxCase(SrcState.rdy, Seq(
      (type1 === SrcType.reg) -> strdOrIdRead.resp,
      (type1 === SrcType.fp) -> fdRead.resp,
      (type1 === SrcType.vec) -> offRead.resp,
    ))

    vdRead.req := source.bits.psrc(2)
    sink.bits.srcState(2) := vdRead.resp

    vmRead.req := source.bits.vm
    sink.bits.vmState := vmRead.resp

    fpBusyTableReadIdx = fpBusyTableReadIdx + 1
    intBusyTableReadIdx = intBusyTableReadIdx + 2
    vectorBusyTableReadIdx = vectorBusyTableReadIdx + 3
    //assert(type0 === SrcType.reg)
    when(source.valid) {
      assert(FuType.memoryTypes.map(_ === source.bits.ctrl.fuType).reduce(_||_))
    }
  })

  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }


  private val specialIssueArbiter = Module(new SelectRespArbiter(param.bankNum, entriesNumPerBank, lduIssuePortNum, false))
  private val loadUops = Wire(Vec(lduIssuePortNum, new MicroOp))
  private val slRes = specialIssueArbiter.io.out
  private val uopSel = RegEnable(specialIssueArbiter.io.chosen, slRes.valid)
  private val uopChosen = Mux1H(uopSel, loadUops)
  rsBankSeq.zip(slRes.bits.bankIdxOH.asBools).foreach({case(b, e) =>
    b.io.specialIssue.valid := slRes.valid && e
    b.io.specialIssue.bits := slRes.bits.entryIdxOH
  })
  private val specialLoadIssueDriver = Module(new MemoryIssuePipeline(param.bankNum, entriesNumPerBank))
  specialLoadIssueDriver.io.redirect := io.redirect
  specialLoadIssueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel
  specialLoadIssueDriver.io.enq.valid := slRes.valid
  slRes.ready := specialLoadIssueDriver.io.enq.ready
  specialLoadIssueDriver.io.enq.bits.selectResp := slRes
  specialLoadIssueDriver.io.enq.bits.uop := uopChosen

  specialLoadIssue.head._1.issue.valid := specialLoadIssueDriver.io.deq.valid
  specialLoadIssue.head._1.issue.bits.uop := specialLoadIssueDriver.io.deq.bits.uop
  specialLoadIssue.head._1.issue.bits.src := DontCare
  specialLoadIssue.head._1.rsIdx.bankIdxOH := specialLoadIssueDriver.io.deq.bits.bankIdxOH
  specialLoadIssue.head._1.rsIdx.entryIdxOH := specialLoadIssueDriver.io.deq.bits.entryIdxOH
  specialLoadIssue.head._1.rsFeedback.isFirstIssue := false.B
  specialLoadIssue.head._1.auxValid := specialLoadIssueDriver.io.deq.valid
  specialLoadIssueDriver.io.deq.ready := specialLoadIssue.head._1.issue.ready

  private val issBankNum = param.bankNum / issue.length

  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new MemoryIssuePipeline(param.bankNum, entriesNumPerBank))
      issueDriver.io.redirect := io.redirect
      issueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel

      val respArbiter = Module(new SelectRespArbiter(param.bankNum, entriesNumPerBank, 3, true))
      respArbiter.io.in(0) <> stdSelectNetwork.io.issueInfo(issuePortIdx)
      respArbiter.io.in(1) <> staSelectNetwork.io.issueInfo(issuePortIdx)
      respArbiter.io.in(2).valid := lduSelectNetwork.io.issueInfo(issuePortIdx).valid
      respArbiter.io.in(2).bits := lduSelectNetwork.io.issueInfo(issuePortIdx).bits
      val scalarLoadSel = lduSelectNetwork.io.issueInfo(issuePortIdx).valid && !lduSelectNetwork.io.issueInfo(issuePortIdx).bits.info.isVector
      val notChosen = !respArbiter.io.in(2).ready
      val notHoldLoad = !(issueDriver.io.hold && issueDriver.io.isLoad)
      specialIssueArbiter.io.in(issuePortIdx).valid := scalarLoadSel && notChosen && notHoldLoad
      specialIssueArbiter.io.in(issuePortIdx).bits := lduSelectNetwork.io.issueInfo(issuePortIdx).bits
      lduSelectNetwork.io.issueInfo(issuePortIdx).ready := specialIssueArbiter.io.in(issuePortIdx).ready | respArbiter.io.in(2).ready
      val selResp = respArbiter.io.out

      def getSlice[T <: Object](in: Seq[T]): Seq[T] = in.slice(issuePortIdx * issBankNum, issuePortIdx * issBankNum + issBankNum)

      val selectedBanks = getSlice(rsBankSeq)
      val bankEns = getSlice(selResp.bits.bankIdxOH.asBools).map(_ && selResp.fire)
      val bankPayloads = Wire(Vec(3, new MicroOp))
      for ((b, en)<- selectedBanks.zip(bankEns)) {
        b.io.loadIssue.valid := en && respArbiter.io.in(2).fire
        b.io.loadIssue.bits := lduSelectNetwork.io.issueInfo(issuePortIdx).bits.entryIdxOH
        b.io.staIssue.valid := en && respArbiter.io.in(1).fire
        b.io.staIssue.bits := staSelectNetwork.io.issueInfo(issuePortIdx).bits.entryIdxOH
        b.io.stdIssue.valid := en && respArbiter.io.in(0).fire
        b.io.stdIssue.bits := stdSelectNetwork.io.issueInfo(issuePortIdx).bits.entryIdxOH
      }
      val stdSel = getSlice(stdSelectNetwork.io.issueInfo(issuePortIdx).bits.bankIdxOH.asBools)
      val staSel = getSlice(staSelectNetwork.io.issueInfo(issuePortIdx).bits.bankIdxOH.asBools)
      val loadSel = getSlice(lduSelectNetwork.io.issueInfo(issuePortIdx).bits.bankIdxOH.asBools)
      val stdSelDelay = stdSel.map(s => RegEnable(s, selResp.valid))
      val staSelDelay = staSel.map(s => RegEnable(s, selResp.valid))
      val loadSelDelay = loadSel.map(s => RegEnable(s, selResp.valid))
      bankPayloads(0) := Mux1H(stdSelDelay, selectedBanks.map(_.io.stdUop))
      bankPayloads(1) := Mux1H(staSelDelay, selectedBanks.map(_.io.staUop))
      bankPayloads(2) := Mux1H(loadSelDelay, selectedBanks.map(_.io.loadUop))
      loadUops(issuePortIdx) := bankPayloads(2)

      val chosenDelay = RegEnable(respArbiter.io.chosen, selResp.valid)
      val selPayload = Mux1H(chosenDelay, bankPayloads)
      stIssuedWires(issuePortIdx).valid := issueDriver.io.deq.fire && issueDriver.io.deq.bits.uop.ctrl.fuType === FuType.stu
      stIssuedWires(issuePortIdx).bits := issueDriver.io.deq.bits.uop.robIdx
      val replayPortSel = selectedBanks.map(_.io.replay)
      val feedbackSeq = Seq(iss._1.rsFeedback.feedbackSlowLoad, iss._1.rsFeedback.feedbackFastLoad, iss._1.rsFeedback.feedbackSlowStore)
      replayPortSel.zipWithIndex.foreach(bankReplays => {
        val bankIdx = bankReplays._2 + issuePortIdx * issBankNum
        bankReplays._1.zip(feedbackSeq).foreach({ case (sink, source) =>
          sink.valid := source.valid && source.bits.rsIdx.bankIdxOH(bankIdx)
          sink.bits.entryIdxOH := source.bits.rsIdx.entryIdxOH
          sink.bits.waitVal := source.bits.sourceType
        })
      })

      val lduIssueInfo = lduSelectNetwork.io.issueInfo(issuePortIdx)
      val earlyWakeupQueue = Module(new WakeupQueue(3))
      earlyWakeupQueue.io.in.valid := lduIssueInfo.fire && lduIssueInfo.bits.info.rfWen
      earlyWakeupQueue.io.earlyWakeUpCancel := io.earlyWakeUpCancel
      earlyWakeupQueue.io.in.bits.robPtr := lduIssueInfo.bits.info.robPtr
      earlyWakeupQueue.io.in.bits.lpv := lduIssueInfo.bits.info.lpv
      earlyWakeupQueue.io.in.bits.pdest := lduIssueInfo.bits.info.pdest
      earlyWakeupQueue.io.in.bits.destType := MuxCase(SrcType.default, Seq(
        lduIssueInfo.bits.info.rfWen -> SrcType.reg,
        lduIssueInfo.bits.info.fpWen -> SrcType.fp,
      ))
      earlyWakeupQueue.io.redirect := io.redirect

      earlyWakeupQueue.io.in.bits.lpv(issuePortIdx) := lduIssueInfo.bits.info.lpv(issuePortIdx) | (1 << (LpvLength - 1)).U
      io.loadEarlyWakeup(issuePortIdx).valid := earlyWakeupQueue.io.out.valid
      io.loadEarlyWakeup(issuePortIdx).bits.robPtr := earlyWakeupQueue.io.out.bits.robPtr
      io.loadEarlyWakeup(issuePortIdx).bits.pdest := earlyWakeupQueue.io.out.bits.pdest
      io.loadEarlyWakeup(issuePortIdx).bits.destType := earlyWakeupQueue.io.out.bits.destType
      io.loadEarlyWakeup(issuePortIdx).bits.lpv := earlyWakeupQueue.io.out.bits.lpv(issuePortIdx)

      selResp.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := selResp.valid
      issueDriver.io.enq.bits.uop := selPayload
      issueDriver.io.enq.bits.selectResp := selResp.bits

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits.uop
      iss._1.issue.bits.src := DontCare
      iss._1.rsIdx.bankIdxOH := issueDriver.io.deq.bits.bankIdxOH
      iss._1.rsIdx.entryIdxOH := issueDriver.io.deq.bits.entryIdxOH
      iss._1.rsFeedback.isFirstIssue := false.B
      iss._1.hold := issueDriver.io.hold
      iss._1.auxValid := issueDriver.io.deq.valid
      issueDriver.io.deq.ready := iss._1.issue.ready
    }
  }

  println("\nMemory Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({ case ((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
  XSPerfHistogram("issue_num", PopCount(issue.map(_._1.issue.fire)), true.B, 0, issue.length, 1)
  XSPerfHistogram("valid_entries_num", PopCount(Cat(allocateNetwork.io.entriesValidBitVecList)), true.B, 0, param.entriesNum, 4)
}

