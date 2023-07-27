package xiangshan.vector.vbackend.vregfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.execute.exu.{ExuConfig, ExuOutputNode, ExuType}
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.{ExuInput, ExuOutput, HasXSParameter, Redirect, SrcType, XSBundle}
import xiangshan.backend.regfile.{RegFileNode, ScalarRfReadPort}
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xiangshan.vector.HasVectorParameters
import xs.utils.SignExt

class VectorRfReadPort(implicit p:Parameters) extends XSBundle{
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Output(UInt(VLEN.W))
}

class VRegfileTop(extraVectorRfReadPort: Int)(implicit p:Parameters) extends LazyModule with HasXSParameter with HasVectorParameters{
  val issueNode = new RegFileNode
  val writebackNode = new WriteBackSinkNode(WriteBackSinkParam("Vector RegFile Top", WriteBackSinkType.vecRegfile))

  lazy val module = new LazyModuleImp(this) {
    val rfReadNum:Int = issueNode.in.length
    val vectorRfReadNum: Int = issueNode.out.count(_._2._2.hasLoad)
    val wakeupNum:Int = writebackNode.in.head._2._1.count(_.willTriggerVrfWkp)

    val io = IO(new Bundle {
      val hartId = Input(UInt(64.W))
      val vectorReads = Vec(extraVectorRfReadPort, new VectorRfReadPort)
      val scalarReads = Vec(rfReadNum, Flipped(new ScalarRfReadPort))
      val wakeups = Output(Vec(wakeupNum, Valid(new ExuOutput)))
      val moveOldValReqs = Input(Vec(loadUnitNum, Valid(new MoveReq)))
      val debug_vec_rat = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
      val redirect = Input(Valid(new Redirect))
    })
    private val wb = writebackNode.in.flatMap(i => i._1.zip(i._2._1))
    wb.zipWithIndex.foreach({ case ((_, cfg), idx) =>
      println(s"port $idx ${cfg.name} #${cfg.id}")
    })
    println("")
    require(issueNode.in.length == 1)
    private val fromRs = issueNode.in.flatMap(i => i._1.zip(i._2._2).map(e => (e._1, e._2, i._2._1)))
    private val toExuMap = issueNode.out.map(i => i._2._2 -> (i._1, i._2._2, i._2._1)).toMap
    private val wbMayWkp = wb.filter(_._2.willTriggerVrfWkp)
    private val wbNotWkp = wb.filterNot(_._2.willTriggerVrfWkp)
    private val readPortsNum = fromRs.length * 4 + extraVectorRfReadPort

    private val vrf = Module(new VRegfile(wbMayWkp.length, wbNotWkp.length, readPortsNum))
    vrf.io.wbWakeup.zip(wbMayWkp).foreach({case(a, b) => a := b._1})
    vrf.io.wbNoWakeup.zip(wbNotWkp).foreach({case(a, b) => a := b._1})
    vrf.io.moveOldValReqs := io.moveOldValReqs
    io.wakeups := vrf.io.wakeups
    vrf.io.readPorts.take(extraVectorRfReadPort).zip(io.vectorReads).foreach({case(rr, ir) =>
      rr.addr := ir.addr
      ir.data := rr.data
    })

    private var vecReadPortIdx = extraVectorRfReadPort
    private var scalarReadPortIdx = 0
    for (in <- fromRs) {
      val out = toExuMap(in._2)
      val bi = in._1
      val bo = out._1
      val exuInBundle = WireInit(bi.issue.bits)
      exuInBundle.src := DontCare
      io.scalarReads(scalarReadPortIdx).addr := bi.issue.bits.uop.psrc(0)
      io.scalarReads(scalarReadPortIdx).isFp := bi.issue.bits.uop.ctrl.srcType(0) === SrcType.fp
      vrf.io.readPorts(vecReadPortIdx).addr := bi.issue.bits.uop.psrc(0)
      vrf.io.readPorts(vecReadPortIdx + 1).addr := bi.issue.bits.uop.psrc(1)
      vrf.io.readPorts(vecReadPortIdx + 2).addr := bi.issue.bits.uop.psrc(2)
      vrf.io.readPorts(vecReadPortIdx + 3).addr := bi.issue.bits.uop.vm

      exuInBundle.src(0) := MuxCase(vrf.io.readPorts(vecReadPortIdx).data, Seq(
        SrcType.isRegOrFp(bi.issue.bits.uop.ctrl.srcType(0)) -> io.scalarReads(scalarReadPortIdx).data,
        SrcType.isVec(bi.issue.bits.uop.ctrl.srcType(0)) -> vrf.io.readPorts(vecReadPortIdx).data,
        SrcType.isImm(bi.issue.bits.uop.ctrl.srcType(0)) -> SignExt(bi.issue.bits.uop.ctrl.imm(4,0), VLEN)
      ))
      exuInBundle.src(1) := vrf.io.readPorts(vecReadPortIdx + 1).data
      exuInBundle.src(2) := vrf.io.readPorts(vecReadPortIdx + 2).data
      exuInBundle.vm := vrf.io.readPorts(vecReadPortIdx + 3).data

      val issValidReg = RegInit(false.B)
      val issDataReg = Reg(new ExuInput())
      val allowPipe = !issValidReg || bo.issue.ready || (issValidReg && issDataReg.uop.robIdx.needFlush(io.redirect))
      bo.issue.valid := issValidReg && !issDataReg.uop.robIdx.needFlush(io.redirect)
      bo.issue.bits := issDataReg
      when(allowPipe){
        issValidReg := bi.issue.valid
      }
      when(bi.issue.fire) {
        issDataReg := exuInBundle
      }
      bo.rsIdx := DontCare
      bi.rsFeedback := DontCare
      bo.hold := false.B

      scalarReadPortIdx = scalarReadPortIdx + 2
      vecReadPortIdx = vecReadPortIdx + 4
    }
  }
}
