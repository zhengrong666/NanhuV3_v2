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
import xiangshan.{FuType, HasXSParameter, MicroOp, Redirect, SrcState, SrcType, XSCoreParamsKey}
import xiangshan.backend.execute.exu.ExuType
import xiangshan.backend.execute.fu.FuConfigs
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import utils.XSPerfHistogram
import xiangshan.backend.issue._
import xiangshan.backend.rename.BusyTable
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xiangshan.mem.SqPtr

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
    val aluSpecWakeup = Input(Vec(p(XSCoreParamsKey).exuParameters.aluNum, Valid(new WakeUpInfo)))
    val loadEarlyWakeup = Output(Vec(loadUnitNum, Valid(new EarlyWakeUpInfo)))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val stLastCompelet = Input(new SqPtr)
    val integerAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val floatingAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val vectorAllocPregs = Vec(coreParams.vectorParameters.vRenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
  })
  require(outer.dispatchNode.in.length == 1)
  private val enq = outer.dispatchNode.in.map(_._1).head

  private val wakeupSignals = VecInit(wakeup.map(_._1).map(elm =>{
    val wkp = Wire(Valid(new WakeUpInfo))
    wkp.valid := elm.valid
    wkp.bits.pdest := elm.bits.uop.pdest
    wkp.bits.robPtr := elm.bits.uop.robIdx
    wkp.bits.lpv := 0.U.asTypeOf(wkp.bits.lpv)
    wkp.bits.destType := MuxCase(SrcType.default, Seq(
      elm.bits.uop.ctrl.rfWen -> SrcType.reg,
      elm.bits.uop.ctrl.fpWen -> SrcType.fp,
      elm.bits.uop.ctrl.vdWen -> SrcType.vec
    ))
    wkp
  }))
  private val aluSpecWakeup = Wire(Vec(p(XSCoreParamsKey).exuParameters.aluNum, Valid(new WakeUpInfo)))
  aluSpecWakeup.zip(io.aluSpecWakeup).foreach({case(a, b) =>
    val flush = b.bits.robPtr.needFlush(io.redirect)
    val canceled = b.bits.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
    a.valid := b.valid && !flush && !canceled
    a.bits := b.bits
    a.bits.lpv.foreach(_ := 0.U)
  })

  private val stIssuedWires = Wire(Vec(staIssuePortNum, Valid(new RobPtr)))

  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new MemoryReservationBank(entriesNumPerBank, staIssuePortNum, lduIssuePortNum, (wakeupSignals ++ io.mulSpecWakeup ++ aluSpecWakeup).length))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals ++ io.mulSpecWakeup ++ aluSpecWakeup
    mod.io.loadEarlyWakeup := io.loadEarlyWakeup
    mod.io.earlyWakeUpCancel := io.earlyWakeUpCancel
    mod.io.stIssued := stIssuedWires
    mod.io.stLastCompelet := RegNext(io.stLastCompelet)
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("MemoryAllocateNetwork")))

  private val wakeupFp = wakeupSignals.zip(wakeup.map(_._2)).filter(_._2.writeFpRf).map(_._1)
  private val wakeupInt = wakeupSignals.zip(wakeup.map(_._2)).filter(_._2.writeIntRf).map(_._1)
  private val wakeupVec = wakeupSignals.zip(wakeup.map(_._2)).filter(_._2.writeVecRf).map(_._1)
  private val floatingBusyTable = Module(new BusyTable(param.bankNum, (wakeupFp ++ io.mulSpecWakeup).length, RenameWidth))
  floatingBusyTable.io.allocPregs := io.floatingAllocPregs
  floatingBusyTable.io.wbPregs.zip(wakeupFp ++ io.mulSpecWakeup).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.fp
    bt.bits := wb.bits.pdest
  })
  private val integerBusyTable = Module(new BusyTable(param.bankNum * 2, wakeupInt.length + io.mulSpecWakeup.length + aluSpecWakeup.length, RenameWidth))
  integerBusyTable.io.allocPregs := io.integerAllocPregs
  integerBusyTable.io.wbPregs.zip(wakeupInt ++ aluSpecWakeup ++ io.mulSpecWakeup).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.reg
    bt.bits := wb.bits.pdest
  })
  private val vectorBusyTable = Module(new BusyTable(param.bankNum * 3, wakeupVec.length + io.mulSpecWakeup.length, coreParams.vectorParameters.vRenameWidth))
  vectorBusyTable.io.allocPregs := io.vectorAllocPregs
  vectorBusyTable.io.wbPregs.zip(wakeupVec ++ io.mulSpecWakeup).foreach({ case (bt, wb) =>
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
  private val uopChosen = Mux1H(specialIssueArbiter.io.chosen, loadUops)
  rsBankSeq.zip(slRes.bits.bankIdxOH.asBools).foreach({case(b, e) =>
    b.io.specialIssue.valid := slRes.valid && e
    b.io.specialIssue.bits := slRes.bits.entryIdxOH
  })
  private val specialLoadIssueDriver = Module(new DecoupledPipeline(false, param.bankNum, entriesNumPerBank))
  specialLoadIssueDriver.io.redirect := io.redirect
  specialLoadIssueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel
  specialLoadIssueDriver.io.enq.valid := slRes.valid
  slRes.ready := specialLoadIssueDriver.io.enq.ready
  specialLoadIssueDriver.io.enq.bits.entryIdxOH := slRes.bits.entryIdxOH
  specialLoadIssueDriver.io.enq.bits.bankIdxOH := slRes.bits.bankIdxOH
  specialLoadIssueDriver.io.enq.bits.uop := uopChosen
  specialLoadIssueDriver.io.enq.bits.uop.robIdx := slRes.bits.info.robPtr
  specialLoadIssueDriver.io.enq.bits.uop.ctrl.rfWen := slRes.bits.info.rfWen
  specialLoadIssueDriver.io.enq.bits.uop.ctrl.fpWen := slRes.bits.info.fpWen
  specialLoadIssueDriver.io.enq.bits.uop.pdest := slRes.bits.info.pdest
  specialLoadIssueDriver.io.enq.bits.uop.ctrl.fuType := slRes.bits.info.fuType
  specialLoadIssueDriver.io.enq.bits.uop.lpv := slRes.bits.info.lpv

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
      val isStd =  respArbiter.io.chosen === 1.U

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
      bankPayloads(0) := Mux1H(stdSel, selectedBanks.map(_.io.stdUop))
      bankPayloads(1) := Mux1H(staSel, selectedBanks.map(_.io.staUop))
      bankPayloads(2) := Mux1H(loadSel, selectedBanks.map(_.io.loadUop))
      loadUops(issuePortIdx) := bankPayloads(2)

      val selPayload = Mux1H(respArbiter.io.chosen, bankPayloads)
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

      val finalSelectInfo = selResp
      val payload = WireInit(selPayload)
      val isVec = selPayload.ctrl.isVector
      when(!isVec && isStd){
        payload.ctrl.srcType(0) := selPayload.ctrl.srcType(1)
        payload.psrc(0) := selPayload.psrc(1)
      }

      val issueBundle = Wire(Valid(new MicroOp))
      issueBundle.valid := finalSelectInfo.valid
      issueBundle.bits := payload
      issueBundle.bits.robIdx := finalSelectInfo.bits.info.robPtr
      issueBundle.bits.ctrl.rfWen := finalSelectInfo.bits.info.rfWen
      issueBundle.bits.ctrl.fpWen := finalSelectInfo.bits.info.fpWen
      issueBundle.bits.pdest := finalSelectInfo.bits.info.pdest
      issueBundle.bits.ctrl.fuType := finalSelectInfo.bits.info.fuType
      issueBundle.bits.lpv := finalSelectInfo.bits.info.lpv

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := issueBundle.valid
      issueDriver.io.enq.bits.uop := issueBundle.bits
      issueDriver.io.enq.bits.bankIdxOH := finalSelectInfo.bits.bankIdxOH
      issueDriver.io.enq.bits.entryIdxOH := finalSelectInfo.bits.entryIdxOH

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

