package xiangshan.vector.viwaitqueue
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.vector.EewType
import xiangshan.{FuOpType, FuType, LSUOpType, MicroOp, Redirect, SrcType, XSModule}
import xs.utils.LogicShiftRight

class SplitUop(splitNum:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val in = Input(Valid(new MicroOp))
    val current = Input(Vec(splitNum, UInt(log2Ceil(VLEN).W)))
    val vstart = Input(UInt(log2Ceil(VLEN).W))
    val out = Output(Vec(splitNum, Valid(new MicroOp)))
  })
  private val ctrl = io.in.bits.ctrl
  private val vctrl = io.in.bits.vctrl
  private val idxSew = vctrl.eew(1)
  private val memSew = vctrl.eew(0)
  private val nf = io.in.bits.vctrl.nf
  private def LsRenameInfo(idx: UInt, store:Bool): (Bool, UInt, UInt, UInt) = {
    val vlenBytes = VLEN / 8
    val vlenShiftBits = log2Ceil(vlenBytes)
    val idxBits = idx.getWidth
    val idxDivNf = WireInit(idx)
    val idxModNf = WireInit(idx)
    val vdAddend = Wire(UInt(8.W))
    val vs2Addend = Wire(UInt(8.W))
    val shouldRename = Wire(Bool())
    val lFuOpType = Wire(FuOpType())
    val sFuOpType = Wire(FuOpType())
    idxDivNf := idx / nf
    idxModNf := idx % nf
    shouldRename := MuxCase(false.B, Seq(
      (memSew === 0.U) -> (idxDivNf(vlenShiftBits - 1, 0) === 0.U),
      (memSew === 1.U) -> (idxDivNf(vlenShiftBits - 2, 0) === 0.U),
      (memSew === 2.U) -> (idxDivNf(vlenShiftBits - 3, 0) === 0.U),
      (memSew === 3.U) -> (idxDivNf(vlenShiftBits - 4, 0) === 0.U)
    ))
    vdAddend := MuxCase(0.U, Seq(
      (memSew === 0.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits) / nf * nf),
      (memSew === 1.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits - 1) / nf * nf),
      (memSew === 2.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits - 2) / nf * nf),
      (memSew === 3.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits - 3) / nf * nf),
    ))
    vs2Addend := MuxCase(0.U, Seq(
      (idxSew === 0.U) -> idx(idxBits - 1, 4),
      (idxSew === 1.U) -> idx(idxBits - 1, 3),
      (idxSew === 2.U) -> idx(idxBits - 1, 2),
      (idxSew === 3.U) -> idx(idxBits - 1, 1),
    ))
    lFuOpType := MuxCase(LSUOpType.ld, Seq(
      (memSew === 0.U) -> LSUOpType.lbu,
      (memSew === 1.U) -> LSUOpType.lhu,
      (memSew === 2.U) -> LSUOpType.lwu,
      (memSew === 3.U) -> LSUOpType.ld,
    ))
    sFuOpType := MuxCase(LSUOpType.sd, Seq(
      (memSew === 0.U) -> LSUOpType.sb,
      (memSew === 1.U) -> LSUOpType.sh,
      (memSew === 2.U) -> LSUOpType.sw,
      (memSew === 3.U) -> LSUOpType.sd,
    ))
    val fuOpType = Mux(store, sFuOpType, lFuOpType)
    (shouldRename, vdAddend, vs2Addend, fuOpType)
  }
  private def GenAddend(et:UInt, widenOrNarrow:Bool, idx:UInt):UInt = {
    val res = Wire(UInt(3.W))
    res := MuxCase(0.U, Seq(
      (et === EewType.sew)   -> Mux(widenOrNarrow, LogicShiftRight(idx, 1), idx),
      (et === EewType.sewm2) -> idx, //Only appear in narrow or widen instructions
      (et === EewType.sewd2) -> LogicShiftRight(idx, 1),
      (et === EewType.sewd4) -> LogicShiftRight(idx, 2),
      (et === EewType.sewd8) -> LogicShiftRight(idx, 3),
    ))
    res
  }

  io.out.zipWithIndex.foreach({ case (o, idx) =>
    val currentnum = io.current(idx)
    o.valid := io.in.valid && (currentnum < io.in.bits.uopNum)
    o.bits := io.in.bits
    o.bits.uopNum := io.in.bits.uopNum
    o.bits.uopIdx := currentnum
    o.bits.isTail := currentnum >= vctrl.evl || currentnum === (vctrl.evl - 1.U) && io.in.bits.vctrl.tailOffset === 0.U //Only VLS need this
    o.bits.partialTail := currentnum === (vctrl.evl - 1.U) && io.in.bits.vctrl.tailOffset =/= 0.U
    o.bits.isPrestart := currentnum < io.vstart //Only VLS need this

    when(io.in.bits.vctrl.isLs) {
      val (doRn, vda, vs2a, ft) = LsRenameInfo(currentnum, o.bits.ctrl.fuType === FuType.stu)
      o.bits.canRename := doRn
      o.bits.ctrl.ldest := ctrl.ldest + vda
      o.bits.ctrl.lsrc(0) := ctrl.lsrc(0)
      o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + vs2a, ctrl.lsrc(1))
      o.bits.ctrl.fuOpType := ft
    }.otherwise {
      val narrowOrWiden = vctrl.isNarrow | vctrl.isNarrow
      val vs1Addend = GenAddend(vctrl.eewType(0), narrowOrWiden, currentnum)
      val vs2Addend = GenAddend(vctrl.eewType(1), narrowOrWiden, currentnum)
      val vdAddend  = GenAddend(vctrl.eewType(2), narrowOrWiden, currentnum)
      o.bits.canRename := true.B
      o.bits.ctrl.ldest := ctrl.ldest + vdAddend
      o.bits.ctrl.lsrc(0) := ctrl.lsrc(0) + vs1Addend
      o.bits.ctrl.lsrc(1) := ctrl.lsrc(1) + vs2Addend
    }
  })
}

object SplitUop {
  def apply(in:Valid[MicroOp], currentNum:Vec[UInt], vstart:UInt, splitNum:Int)(implicit p: Parameters): Vec[Valid[MicroOp]] = {
    val uopSplitter = Module(new SplitUop(splitNum))
    uopSplitter.io.in := in
    uopSplitter.io.current := currentNum
    uopSplitter.io.vstart := vstart
    uopSplitter.io.out
  }
}

class SplitNetwork(splitNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(new MicroOp))
    val out = Vec(splitNum, Decoupled(new MicroOp))
    val vstart = Input(UInt(7.W))
    val redirect = Input(Valid(new Redirect))
  })

  private val remain = RegInit(0.U(log2Ceil(VLEN + 1).W))
  private val remainWire = WireInit(remain)
  private val leaving = PopCount(io.out.map(_.fire))
  private val remainUpdate = io.out.map(_.fire).reduce(_|_)
  private val uopNum = io.in.bits.uopNum
  private val currentIndices = RegInit(VecInit(Seq.tabulate(splitNum)(_.U(log2Ceil(VLEN + 1).W))))
  private val currentDefaultVal = VecInit(Seq.tabulate(splitNum)(_.U))
  private val currentWires = WireInit(currentIndices)

  when(io.in.bits.robIdx.needFlush(io.redirect) && io.in.valid) {
    remain := 0.U
    currentIndices := currentDefaultVal
  }.elsewhen(remain === 0.U && io.in.valid) {
    remain := uopNum - leaving
    currentIndices.zip(currentDefaultVal).foreach({case(c, rv) => c := rv + leaving})
  }.elsewhen(remainUpdate) {
    remain := remain - leaving
    currentIndices.foreach(c => c := c + leaving)
  }

  when(remain === 0.U && io.in.valid){
    remainWire := uopNum
    currentWires := currentDefaultVal
  }

  io.in.ready := (remainWire === leaving) && !io.redirect.valid

  private val in_v = Wire(Valid(new MicroOp))
  in_v.valid := io.in.valid
  in_v.bits := io.in.bits
  in_v.bits.uopNum := uopNum

  private val out_v = SplitUop(in_v, currentWires, io.vstart, splitNum)

  io.out.zipWithIndex.foreach({ case (o, i) =>
    o.valid := out_v(i).valid
    o.bits := out_v(i).bits
  })

  when(io.in.valid){assert(leaving <= remainWire)}
}
