package xiangshan.vector.vbackend.vregfile

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{AdapterNode, LazyModule, LazyModuleImp, ValName}
import xiangshan.backend.execute.exu.{ExuConfig, ExuOutputNode, ExuOutwardImpl, ExuType}
import xiangshan.{ExuInput, ExuOutput, FuType, HasXSParameter, MicroOp, Redirect, SrcType, XSBundle}
import xiangshan.backend.regfile.{RegFileNode, ScalarRfReadPort}
import xiangshan.vector.HasVectorParameters
import xs.utils.{SignExt, ZeroExt}
import VRegfileTopUtil._

class VectorWritebackMergeNode(implicit valName: ValName) extends AdapterNode(ExuOutwardImpl)({p => p.copy(throughVectorRf = true)}, {p => p})

class VectorRfReadPort(implicit p:Parameters) extends XSBundle{
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Output(UInt(VLEN.W))
}

object VRegfileTopUtil{
  def GenWbMask(in:MicroOp, width:Int, elementWise:Boolean, VLEN:Int): UInt = {
    val res = VecInit(Seq.fill(width)(false.B))
    val sew = in.vCsrInfo.vsew
    val w = in.uopIdx.getWidth - 1
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
      when((un === ui && un < i.U) || ui === i.U){
        r := true.B
      }
    }
    res.asUInt
  }

  def GenLoadVrfMask(in:MicroOp, VLEN:Int):UInt = {
    val width = VLEN / 8
    val vlenShiftBits = log2Ceil(VLEN / 8)
    val sew = in.vCsrInfo.vsew
    val uopIdx = in.uopIdx
    val mask = MuxCase(0.U, Seq(
      (sew === 0.U) -> ("h01".U << Cat(uopIdx(vlenShiftBits - 1, 0), 0.U(0.W))),
      (sew === 1.U) -> ("h03".U << Cat(uopIdx(vlenShiftBits - 2, 0), 0.U(1.W))),
      (sew === 2.U) -> ("h0f".U << Cat(uopIdx(vlenShiftBits - 3, 0), 0.U(2.W))),
      (sew === 3.U) -> ("hff".U << Cat(uopIdx(vlenShiftBits - 4, 0), 0.U(3.W))),
    ))
    mask(width - 1, 0).asUInt
  }
}

class VRegfileTop(extraVectorRfReadPort: Int)(implicit p:Parameters) extends LazyModule with HasXSParameter with HasVectorParameters{
  val issueNode = new RegFileNode
  val writebackMergeNode = new VectorWritebackMergeNode
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
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
    require(issueNode.in.length == 1)

    private val wbPairNeedMerge = wbVFUPair.filter(_._3.willTriggerVrfWkp)
    private val wbPairDontNeedMerge = wbVFUPair.filterNot(_._3.willTriggerVrfWkp).filterNot(_._3.exuType == ExuType.sta)
    private val wbPairStu = wbVFUPair.filter(_._3.exuType == ExuType.sta)

    private val fromRs = issueNode.in.flatMap(i => i._1.zip(i._2._2).map(e => (e._1, e._2, i._2._1)))
    private val toExuMap = issueNode.out.map(i => i._2._2 -> (i._1, i._2._2, i._2._1)).toMap

    private val readPortsNum = fromRs.length * 4 + extraVectorRfReadPort

    private val vrf = Module(new VRegfile(wbPairNeedMerge.length, wbPairDontNeedMerge.length, readPortsNum))

    println("VRF writeback port need merged:")
    wbPairNeedMerge.foreach(e => print(e._3))
    println("\nVRF writeback port not need merged:")
    wbPairDontNeedMerge.foreach(e => print(e._3))
    vrf.io.wbWakeup.zip(vrf.io.wakeups).zip(wbPairNeedMerge).foreach({case((rfwb, rfwkp),(wbin, wbout, cfg)) =>
      if(cfg.exuType == ExuType.ldu){
        val sew = wbin.bits.uop.vCsrInfo.vsew
        val bitsWire = WireInit(wbin.bits)
        bitsWire.data := MuxCase(0.U, Seq(
          (sew === 0.U) -> Cat(Seq.fill(VLEN / 8)(wbin.bits.data(7, 0))),
          (sew === 1.U) -> Cat(Seq.fill(VLEN / 16)(wbin.bits.data(15, 0))),
          (sew === 2.U) -> Cat(Seq.fill(VLEN / 32)(wbin.bits.data(31, 0))),
          (sew === 3.U) -> Cat(Seq.fill(VLEN / 64)(wbin.bits.data(63, 0)))
        ))
        val validCond = wbin.valid && wbin.bits.uop.ctrl.vdWen
        val validReg = RegNext(validCond, false.B)
        val bitsReg = RegEnable(bitsWire, validCond)
        val redirectValidReg = RegNext(wbin.bits.redirectValid && !wbin.bits.redirect.robIdx.needFlush(io.redirect), false.B)
        val redirectBitsReg = RegEnable(wbin.bits.redirect, wbin.bits.redirectValid)

        rfwb.valid := validReg
        rfwb.bits := bitsReg
        rfwb.bits.wakeupMask := GenLoadVrfMask(bitsReg.uop, VLEN)
        rfwb.bits.writeDataMask := Mux(bitsReg.uop.loadStoreEnable, GenLoadVrfMask(bitsReg.uop, VLEN), 0.U)
        rfwb.bits.redirectValid := redirectValidReg
        rfwb.bits.redirect := redirectBitsReg
      } else {
        rfwb.valid := wbin.valid && wbin.bits.uop.ctrl.vdWen && !wbin.bits.uop.robIdx.needFlush(io.redirect)
        rfwb.bits := wbin.bits
      }
      wbout.valid := rfwkp.valid
      wbout.bits := rfwkp.bits
      wbout.bits.redirectValid := rfwkp.bits.redirectValid
      wbout.bits.redirect := rfwkp.bits.redirect
      wbout.bits.wbmask := GenWbMask(rfwkp.bits.uop, 8, cfg.exuType == ExuType.ldu, VLEN)
    })
    vrf.io.wbNoWakeup.zip(wbPairDontNeedMerge).foreach({case(rfwb, (wbin, wbout, cfg)) =>
      rfwb.valid := wbin.valid && wbin.bits.uop.ctrl.vdWen
      rfwb.bits := wbin.bits
      val validCond = wbin.valid
      val validReg = RegNext(validCond && !wbin.bits.uop.robIdx.needFlush(io.redirect), false.B)
      val bitsReg = RegEnable(wbin.bits, wbin.valid && validCond)
      val redirectValidReg = RegNext(wbin.bits.redirectValid && !wbin.bits.redirect.robIdx.needFlush(io.redirect), false.B)
      val redirectBitsReg = RegEnable(wbin.bits.redirect, wbin.bits.redirectValid)
      wbout.valid := validReg
      wbout.bits := bitsReg
      wbout.bits.redirectValid := redirectValidReg
      wbout.bits.redirect := redirectBitsReg
      wbout.bits.wbmask := GenWbMask(bitsReg.uop, 8, false, VLEN)
    })
    wbPairStu.foreach({case(wbin, wbout, _) =>
      val validCond = wbin.valid
      val validReg = RegNext(validCond, false.B)
      val bitsReg = RegEnable(wbin.bits, wbin.valid && validCond)
      val redirectValidReg = RegNext(wbin.bits.redirectValid, false.B)
      val redirectBitsReg = RegEnable(wbin.bits.redirect, wbin.bits.redirectValid)
      wbout.valid := validReg && !bitsReg.uop.robIdx.needFlush(io.redirect)
      wbout.bits := bitsReg
      wbout.bits.redirectValid := redirectValidReg && !redirectBitsReg.robIdx.needFlush(io.redirect)
      wbout.bits.redirect := redirectBitsReg
      wbout.bits.wbmask := VRegfileTopUtil.GenWbMask(bitsReg.uop, 8, true, VLEN)
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
      io.scalarReads(scalarReadPortIdx).addr := bi.specialPsrc
      io.scalarReads(scalarReadPortIdx).isFp := bi.specialPsrcType === SrcType.fp
      vrf.io.readPorts(vecReadPortIdx).addr := bi.issue.bits.uop.psrc(0)
      vrf.io.readPorts(vecReadPortIdx + 1).addr := bi.issue.bits.uop.psrc(1)
      vrf.io.readPorts(vecReadPortIdx + 2).addr := bi.issue.bits.uop.psrc(2)
      vrf.io.readPorts(vecReadPortIdx + 3).addr := bi.issue.bits.uop.vm

      exuInBundle.src(0) := MuxCase(vrf.io.readPorts(vecReadPortIdx).data, Seq(
        SrcType.isRegOrFp(bi.issue.bits.uop.ctrl.srcType(0)) -> RegEnable(io.scalarReads(scalarReadPortIdx).data, bi.specialPsrcRen),
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

    if (env.EnableDifftest || env.AlwaysBasicDiff) {
      val difftestArchVec = DifftestModule(new DiffArchVecRegState, delay = 2)
      difftestArchVec.clock := clock
      difftestArchVec.coreid := io.hartId

      vrf.io.debug.get.zipWithIndex.foreach {
        case (rp, i) => {
          rp.addr := io.debug_vec_rat(i)
          difftestArchVec.value(i*2) := rp.data(VLEN/2-1, 0)
          difftestArchVec.value(i*2+1) := rp.data(VLEN-1, VLEN/2)
        }
      }
    }
  }
}
