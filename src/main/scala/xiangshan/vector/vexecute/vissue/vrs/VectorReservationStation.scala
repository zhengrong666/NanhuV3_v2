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

class VectorReservationStation(implicit p: Parameters) extends LazyModule with HasXSParameter{
  private val entryNum = p(XSCoreParamsKey).intRsDepth
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
  allocateNetwork.io.enqFromDispatch.zip(enq).foreach({case(sink, source) =>
    val intReadPort = integerBusyTable.io.read(intBusyTableReadIdx)
    val fpReadPort = floatingBusyTable.io.read(fpBusyTableReadIdx)
    val vecReadPorts = Seq.tabulate(4)(idx => vectorBusyTable.io.read(vectorBusyTableReadIdx + idx))
    intReadPort.req := source.bits.psrc(0)
    fpReadPort.req := source.bits.psrc(0)
    vecReadPorts(0).req := source.bits.psrc(0)
    vecReadPorts(1).req := source.bits.psrc(1)
    vecReadPorts(2).req := source.bits.old_pdest
    vecReadPorts(3).req := source.bits.vm
    sink.valid := source.valid
    sink.bits := source.bits
    sink.bits.srcState(0) := MuxCase(SrcState.rdy, Seq(
      (source.bits.ctrl.srcType(0) === SrcType.reg, intReadPort.resp),
      (source.bits.ctrl.srcType(0) === SrcType.fp, fpReadPort.resp),
      (source.bits.ctrl.srcType(0) === SrcType.vec, vecReadPorts(0).resp)
    ))
    sink.bits.srcState(1) := vecReadPorts(1).resp
    sink.bits.oldPdestState := vecReadPorts(2).resp
    sink.bits.vmState := vecReadPorts(3).resp
    source.ready := sink.ready
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

  private var aluWkpPortIdx = 0
  private var mulWkpPortIdx = 0
  private var aluPortIdx = 0
  private var mulPortIdx = 0
  private var divPortIdx = 0
  private var jmpPortIdx = 0
  println("\nInteger Reservation Issue Ports Config:")
  for((iss, issuePortIdx) <- issue.zipWithIndex) {
    println(s"Issue Port $issuePortIdx ${iss._2}")
    prefix(iss._2.name + "_" + iss._2.id) {
      val issueDriver = Module(new DecoupledPipeline(false, param.bankNum, entriesNumPerBank))
      issueDriver.io.redirect := io.redirect
      issueDriver.io.earlyWakeUpCancel := io.earlyWakeUpCancel
      val selectRespArbiter = Module(new SelectRespArbiter(param.bankNum, entriesNumPerBank, 2))
      selectRespArbiter.io.in(0) <> aluSelectNetwork.io.issueInfo(aluPortIdx)
      internalAluWakeupSignals(aluWkpPortIdx) := WakeupQueue(aluSelectNetwork.io.issueInfo(aluPortIdx), aluSelectNetwork.cfg.latency, io.redirect, io.earlyWakeUpCancel, p)
      aluPortIdx = aluPortIdx + 1
      aluWkpPortIdx = aluWkpPortIdx + 1
      if (iss._2.isAluMul) {
        selectRespArbiter.io.in(1) <> mulSelectNetwork.io.issueInfo(mulPortIdx)
        internalMulWakeupSignals(mulWkpPortIdx) := WakeupQueue(mulSelectNetwork.io.issueInfo(mulPortIdx), mulSelectNetwork.cfg.latency, io.redirect, io.earlyWakeUpCancel, p)
        mulPortIdx = mulPortIdx + 1
        mulWkpPortIdx = mulWkpPortIdx + 1
      } else if (iss._2.isAluDiv) {
        selectRespArbiter.io.in(1) <> divSelectNetwork.io.issueInfo(divPortIdx)
        divPortIdx = divPortIdx + 1
      } else if(iss._2.isAluJmp){
        selectRespArbiter.io.in(1) <> jmpSelectNetwork.io.issueInfo(jmpPortIdx)
        jmpPortIdx = jmpPortIdx + 1
      } else {
        require(false, "Unknown Exu complex!")
      }
      val finalSelectInfo = selectRespArbiter.io.out
      val rsBankRen = Mux(issueDriver.io.enq.fire, finalSelectInfo.bits.bankIdxOH, 0.U)
      rsBankSeq.zip(rsBankRen.asBools).foreach({ case (rb, ren) =>
        rb.io.issueAddr(issuePortIdx).valid := ren
        rb.io.issueAddr(issuePortIdx).bits := finalSelectInfo.bits.entryIdxOH
      })

      val issueBundle = Wire(Valid(new MicroOp))
      issueBundle.valid := finalSelectInfo.valid
      issueBundle.bits := Mux1H(rsBankRen, rsBankSeq.map(_.io.issueUop(issuePortIdx).bits))
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
      issueDriver.io.deq.ready := iss._1.issue.ready
    }
  }
  println("\nInteger Reservation Wake Up Ports Config:")
  wakeup.zipWithIndex.foreach({case((_, cfg), idx) =>
    println(s"Wake Port $idx ${cfg.name} of ${cfg.complexName} #${cfg.id}")
  })
}

