package xiangshan.vector.vexecute.vissue.vrs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{FuType, HasXSParameter, MicroOp, Redirect, SrcState, SrcType, XSCoreParamsKey}
import xiangshan.backend.execute.exu.ExuType
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, ValName}
import xiangshan.backend.issue._
import xiangshan.backend.rename.BusyTable
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xiangshan.vector.vexecute.vissue.VDecoupledPipeline

class VectorReservationStation(implicit p: Parameters) extends LazyModule with HasXSParameter{
  private val entryNum = vectorParameters.vRsDepth
  private val wbNodeParam = WriteBackSinkParam(name = "Vector RS", sinkType = WriteBackSinkType.vecRs)
  private val rsParam = RsParam(name = "Vector RS", RsType.vec, entryNum)
  require(entryNum % rsParam.bankNum == 0)
  val issueNode = new RsIssueNode(rsParam)
  val wakeupNode = new WriteBackSinkNode(wbNodeParam)
  val dispatchNode = new RsDispatchNode(rsParam)

  lazy val module = new VectorReservationStationImpl(this, rsParam)
}

class VectorReservationStationImpl(outer:VectorReservationStation, param:RsParam) extends LazyModuleImp(outer) with HasXSParameter {
  require(param.bankNum == 4)
  require(param.entriesNum % param.bankNum == 0)
  private val issue = outer.issueNode.out.head._1 zip outer.issueNode.out.head._2._2
  private val wbIn = outer.wakeupNode.in.head
  private val wakeup = wbIn._1.zip(wbIn._2._1)
  issue.foreach(elm => elm._2.exuConfigs.foreach(elm0 => require(ExuType.vecTypes.contains(elm0.exuType))))

  private val issueWidth = issue.length
  private val entriesNumPerBank = param.entriesNum / param.bankNum

  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val intAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val fpAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val vecAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
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
      (elm.bits.uop.ctrl.rfWen, SrcType.reg),
      (elm.bits.uop.ctrl.fpWen, SrcType.fp),
      (elm.bits.uop.ctrl.vdWen, SrcType.vec)
    ))
    wkp
  }))
  private val wakeupWidth = wakeupSignals.length
  private val rsBankSeq = Seq.tabulate(param.bankNum)( _ => {
    val mod = Module(new VRSBank(entriesNumPerBank, issueWidth, wakeupWidth, loadUnitNum))
    mod.io.redirect := io.redirect
    mod.io.wakeup := wakeupSignals
    mod
  })
  private val allocateNetwork = Module(new AllocateNetwork(param.bankNum, entriesNumPerBank, Some("VectorAllocateNetwork")))
  private val oiq = Module(new OrderedInstructionQueue(param.bankNum, vectorParameters.vRsOIQDepth))
  oiq.io.redirect := io.redirect

  private val integerBusyTable = Module(new BusyTable(param.bankNum, wakeupWidth))
  integerBusyTable.io.allocPregs := io.intAllocPregs
  integerBusyTable.io.wbPregs.zip(wakeupSignals).foreach({case(bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.reg
    bt.bits := wb.bits.pdest
  })
  private val floatingBusyTable = Module(new BusyTable(param.bankNum, wakeupWidth))
  floatingBusyTable.io.allocPregs := io.fpAllocPregs
  floatingBusyTable.io.wbPregs.zip(wakeupSignals).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.fp
    bt.bits := wb.bits.pdest
  })
  private val vectorBusyTable = Module(new BusyTable(param.bankNum * 4, wakeupWidth))
  integerBusyTable.io.allocPregs := io.vecAllocPregs
  integerBusyTable.io.wbPregs.zip(wakeupSignals).foreach({ case (bt, wb) =>
    bt.valid := wb.valid && wb.bits.destType === SrcType.vec
    bt.bits := wb.bits.pdest
  })

  private val fuTypeList = issue.head._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType)

  private val orderedSelectNetwork = Module(new VRSSelectNetwork(param.bankNum, entriesNumPerBank, issue.length, true, fuTypeList, Some(s"VectorOrderedSelectNetwork")))
  private val unorderedSelectNetwork = Module(new VRSSelectNetwork(param.bankNum, entriesNumPerBank, issue.length, false, fuTypeList, Some(s"VectorUnorderedSelectNetwork")))

  private val selectNetworkSeq = Seq(orderedSelectNetwork, unorderedSelectNetwork)
  selectNetworkSeq.foreach(sn => {
    sn.io.selectInfo.zip(rsBankSeq).foreach({ case (sink, source) =>
      sink := source.io.selectInfo
    })
    sn.io.redirect := io.redirect
  })

  private var intBusyTableReadIdx = 0
  private var fpBusyTableReadIdx = 0
  private var vectorBusyTableReadIdx = 0
  allocateNetwork.io.enqFromDispatch.zip(oiq.io.enq).zip(enq).foreach({case((sink, o_sink), source) =>
    val intReadPort = integerBusyTable.io.read(intBusyTableReadIdx)
    val fpReadPort = floatingBusyTable.io.read(fpBusyTableReadIdx)
    val vecReadPorts = Seq.tabulate(4)(idx => vectorBusyTable.io.read(vectorBusyTableReadIdx + idx))
    intReadPort.req := source.bits.psrc(0)
    fpReadPort.req := source.bits.psrc(0)
    vecReadPorts(0).req := source.bits.psrc(0)
    vecReadPorts(1).req := source.bits.psrc(1)
    vecReadPorts(2).req := source.bits.old_pdest
    vecReadPorts(3).req := source.bits.vm
    sink.valid := source.valid && oiq.io.enqCanAccept
    sink.bits := source.bits
    sink.bits.srcState(0) := MuxCase(SrcState.rdy, Seq(
      (source.bits.ctrl.srcType(0) === SrcType.reg, intReadPort.resp),
      (source.bits.ctrl.srcType(0) === SrcType.fp, fpReadPort.resp),
      (source.bits.ctrl.srcType(0) === SrcType.vec, vecReadPorts(0).resp)
    ))
    sink.bits.srcState(1) := vecReadPorts(1).resp
    sink.bits.oldPdestState := vecReadPorts(2).resp
    sink.bits.vmState := vecReadPorts(3).resp
    source.ready := sink.ready && oiq.io.enqCanAccept

    o_sink.valid := source.valid && sink.ready
    o_sink.bits := source.bits
    intBusyTableReadIdx = intBusyTableReadIdx + 1
    fpBusyTableReadIdx = fpBusyTableReadIdx + 1
    vectorBusyTableReadIdx = vectorBusyTableReadIdx + 4
  })

  for(((fromAllocate, toAllocate), rsBank) <- allocateNetwork.io.enqToRs
    .zip(allocateNetwork.io.entriesValidBitVecList)
    .zip(rsBankSeq)){
    toAllocate := rsBank.io.allocateInfo
    rsBank.io.enq.valid := fromAllocate.valid
    rsBank.io.enq.bits.data := fromAllocate.bits.uop
    rsBank.io.enq.bits.addrOH := fromAllocate.bits.addrOH
  }

  private var orderedPortIdx = 0
  private var unorderedPortIdx = 0
  println("\nVector Reservation Issue Ports Config:")
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new VDecoupledPipeline)
      issueDriver.io.redirect := io.redirect
      val issueArbiter = Module(new VRSIssueArbiter(param.bankNum, entriesNumPerBank))
      issueArbiter.io.orderedIn <> orderedSelectNetwork.io.issueInfo(orderedPortIdx)
      issueArbiter.io.unorderedIn <> unorderedSelectNetwork.io.issueInfo(unorderedPortIdx)
      issueArbiter.io.orderedCtrl := oiq.io.ctrl
      oiq.io.issued := issueArbiter.io.orderedChosen

      orderedPortIdx = orderedPortIdx + 1
      unorderedPortIdx = unorderedPortIdx + 1

      val finalSelectInfo = issueArbiter.io.out
      val rsBankRen = Mux(issueDriver.io.enq.fire, finalSelectInfo.bits.bankIdxOH, 0.U)
      rsBankSeq.zip(rsBankRen.asBools).foreach({ case (rb, ren) =>
        rb.io.issueAddr(issuePortIdx).valid := ren
        rb.io.issueAddr(issuePortIdx).bits := finalSelectInfo.bits.entryIdxOH
      })

      finalSelectInfo.ready := issueDriver.io.enq.ready
      issueDriver.io.enq.valid := finalSelectInfo.valid
      issueDriver.io.enq.bits := Mux1H(rsBankRen, rsBankSeq.map(_.io.issueUop(issuePortIdx).bits))
      issueDriver.io.enq.bits.robIdx := finalSelectInfo.bits.info.robPtr
      issueDriver.io.enq.bits.uopIdx := finalSelectInfo.bits.info.uopIdx
      issueDriver.io.enq.bits.ctrl.fuType := finalSelectInfo.bits.info.fuType

      iss._1.issue.valid := issueDriver.io.deq.valid
      iss._1.issue.bits.uop := issueDriver.io.deq.bits
      iss._1.issue.bits.src := DontCare
      iss._1.rsIdx.bankIdxOH := DontCare
      iss._1.rsIdx.entryIdxOH := DontCare
      issueDriver.io.deq.ready := iss._1.issue.ready
    }
  }
  println("\nVector Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({case((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
}

