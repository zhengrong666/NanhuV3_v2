package xiangshan.vector.vbackend.vregfile

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import freechips.rocketchip.diplomacy.{AdapterNode, LazyModule, LazyModuleImp, ValName}
import xiangshan.backend.execute.exu.{ExuConfig, ExuOutputNode, ExuOutwardImpl, ExuType}
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.{ExuInput, ExuOutput, FuType, HasXSParameter, MicroOp, Redirect, SrcType, XSBundle}
import xiangshan.backend.regfile.{RegFileNode, ScalarRfReadPort}
import xiangshan.backend.writeback.{WriteBackSinkNode, WriteBackSinkParam, WriteBackSinkType}
import xiangshan.vector.HasVectorParameters
import xs.utils.{SignExt, ZeroExt}

class VectorWritebackMergeNode(implicit valName: ValName) extends AdapterNode(ExuOutwardImpl)({p => p.copy(throughVectorRf = true)}, {p => p})

class VectorRfReadPort(implicit p:Parameters) extends XSBundle{
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Output(UInt(VLEN.W))
}

object VRegfileTopUtil{
  def GenWbMask(in:MicroOp, width:Int, elementWise:Boolean, VLEN:Int): UInt = {
    val res = VecInit(Seq.fill(width)(false.B))
    val sew = in.vCsrInfo.vsew
    val w = width - 1
    val ui = if(elementWise) {
      MuxCase(0.U(3.W), Seq(
        (sew === 0.U) -> ZeroExt(in.uopIdx(w, log2Ceil(VLEN / 8)), 3),
        (sew === 1.U) -> ZeroExt(in.uopIdx(w, log2Ceil(VLEN / 16)), 3),
        (sew === 2.U) -> ZeroExt(in.uopIdx(w, log2Ceil(VLEN / 32)), 3),
        (sew === 3.U) -> ZeroExt(in.uopIdx(w, log2Ceil(VLEN / 64)), 3)
      ))
    } else {
      in.uopIdx
    }
    val un = if (elementWise) {
      MuxCase(0.U(3.W), Seq(
        (sew === 0.U) -> ZeroExt(in.uopNum(w, log2Ceil(VLEN / 8)), 3),
        (sew === 1.U) -> ZeroExt(in.uopNum(w, log2Ceil(VLEN / 16)), 3),
        (sew === 2.U) -> ZeroExt(in.uopNum(w, log2Ceil(VLEN / 32)), 3),
        (sew === 3.U) -> ZeroExt(in.uopNum(w, log2Ceil(VLEN / 64)), 3)
      ))
    } else {
      in.uopNum
    }

    for ((r, i) <- res.zipWithIndex){
      when((un === ui && ui > i.U) || ui === i.U){
        r := true.B
      }
    }
    res.asUInt
  }
}

class VRegfileTop(extraVectorRfReadPort: Int)(implicit p:Parameters) extends LazyModule with HasXSParameter with HasVectorParameters{
  val issueNode = new RegFileNode
  val writebackMergeNode = new VectorWritebackMergeNode

  lazy val module = new LazyModuleImp(this) {
    val rfReadNum:Int = issueNode.in.length

    val io = IO(new Bundle {
      val hartId = Input(UInt(64.W))
      val vectorReads = Vec(extraVectorRfReadPort, new VectorRfReadPort)
      val scalarReads = Vec(rfReadNum, Flipped(new ScalarRfReadPort))
      val moveOldValReqs = Input(Vec(loadUnitNum, Valid(new MoveReq)))
      val debug_vec_rat = Input(Vec(32, UInt(PhyRegIdxWidth.W)))
      val redirect = Input(Valid(new Redirect))
    })

    private val fromVectorFu = writebackMergeNode.in.map(e => (e._1, e._2._1))
    private val toWritebackNetwork = writebackMergeNode.out.map(e => (e._1, e._2._1))

    private val wbVFUPair = fromVectorFu.zip(toWritebackNetwork).map(e => {
      require(e._1._2.name == e._2._2.name && e._1._2.id == e._2._2.id)
      require(e._1._2.writeVecRf || e._1._2.exuType == ExuType.sta)
      (e._1._1, e._2._1, e._1._2)
    })
    private val wb = fromVectorFu
    println("Vector Regfile Info:")
    wb.zipWithIndex.foreach({ case ((_, cfg), idx) =>
      println(s"port $idx ${cfg.name} #${cfg.id} need merge: ${cfg.willTriggerVrfWkp}")
    })
    println("")
    require(issueNode.in.length == 1)

    private val wbPairNeedMerge = wbVFUPair.filter(_._3.willTriggerVrfWkp)
    private val wbPairDontNeedMerge = wbVFUPair.filterNot(_._3.willTriggerVrfWkp)

    private val fromRs = issueNode.in.flatMap(i => i._1.zip(i._2._2).map(e => (e._1, e._2, i._2._1)))
    private val toExuMap = issueNode.out.map(i => i._2._2 -> (i._1, i._2._2, i._2._1)).toMap

    private val readPortsNum = fromRs.length * 4 + extraVectorRfReadPort

    private val vrf = Module(new VRegfile(wbPairNeedMerge.length, wbPairDontNeedMerge.length, readPortsNum))

    vrf.io.wbWakeup.zip(vrf.io.wakeups).zip(wbPairNeedMerge).foreach({case((rfwb, rfwkp),(wbin, wbout, cfg)) =>
      if(cfg.exuType == ExuType.ldu){
        val sew = wbin.bits.uop.vCsrInfo.vsew
        val bitsWire = WireInit(wbin.bits)
        bitsWire.data := MuxCase(0.U, Seq(
          (sew === 0.U) -> (wbin.bits.data << (wbin.bits.uop.uopIdx(3, 0) + 3.U))(VLEN - 1, 0),
          (sew === 1.U) -> (wbin.bits.data << (wbin.bits.uop.uopIdx(2, 0) + 4.U))(VLEN - 1, 0),
          (sew === 2.U) -> (wbin.bits.data << (wbin.bits.uop.uopIdx(1, 0) + 5.U))(VLEN - 1, 0),
          (sew === 3.U) -> (wbin.bits.data << (wbin.bits.uop.uopIdx(0) + 6.U))(VLEN - 1, 0)
        ))
        val validReg = RegNext(wbin.valid && wbin.bits.uop.ctrl.vdWen, false.B)
        rfwb.bits := RegEnable(bitsWire, wbin.valid && wbin.bits.uop.ctrl.vdWen)
        rfwb.valid := validReg && !rfwb.bits.uop.robIdx.needFlush(io.redirect)
      }
      rfwb.valid := wbin.valid && wbin.bits.uop.ctrl.vdWen
      rfwb.bits := wbin.bits
      wbout.valid := rfwkp.valid && !rfwkp.bits.uop.robIdx.needFlush(io.redirect)
      wbout.bits := rfwkp.bits
      wbout.bits.wbmask := VRegfileTopUtil.GenWbMask(rfwkp.bits.uop, 7, cfg.exuType == ExuType.ldu || cfg.exuType == ExuType.sta, VLEN)
    })
    vrf.io.wbNoWakeup.zip(wbPairDontNeedMerge).foreach({case(rfwb, (wbin, wbout, cfg)) =>
      rfwb.valid := wbin.valid && wbin.bits.uop.ctrl.vdWen
      rfwb.bits := wbin.bits
      val validCond = wbin.valid
      val validReg = RegNext(validCond, false.B)
      val bitsReg = RegEnable(wbin.bits, wbin.valid && validCond)
      wbout.valid := validReg && !bitsReg.uop.robIdx.needFlush(io.redirect)
      wbout.bits := bitsReg
      wbout.bits.wbmask := VRegfileTopUtil.GenWbMask(bitsReg.uop, 7, cfg.exuType == ExuType.ldu || cfg.exuType == ExuType.sta, VLEN)
    })
    vrf.io.moveOldValReqs := io.moveOldValReqs
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
      bi.issue.ready := allowPipe

      bo.rsIdx := DontCare
      bi.rsFeedback := DontCare
      bo.hold := false.B

      scalarReadPortIdx = scalarReadPortIdx + 1
      vecReadPortIdx = vecReadPortIdx + 4
    }
  }
}
