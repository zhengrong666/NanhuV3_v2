package xiangshan.vector.viwaitqueue
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{MicroOp, Narrow, Redirect, SrcType, Widen, XSModule}

class SplitUop(splitNum:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val in = Input(Valid(new MicroOp))
    val current = Input(Vec(splitNum, UInt(log2Ceil(VLEN).W)))
    val vstart = Input(UInt(log2Ceil(VLEN).W))
    val out = Output(Vec(splitNum, Valid(new MicroOp)))
  })
  private val vl = io.in.bits.vCsrInfo.vl
  private val sew = io.in.bits.vCsrInfo.vsew
  private val narrow = io.in.bits.ctrl.narrow === Narrow.Narrow
  private val narrowToMask = io.in.bits.ctrl.narrow === Narrow.Narrow2
  private val ctrl = io.in.bits.ctrl
  private val nf = io.in.bits.ctrl.NField +& 1.U
  private def LsRenameInfo(idx: UInt): (Bool, UInt) = {
    val vlenBytes = VLEN / 8
    val vlenShiftBits = log2Ceil(vlenBytes)
    val idxBits = idx.getWidth
    val idxDivNf = WireInit(idx)
    val idxModNf = WireInit(idx)
    val vecIdxAddend = WireInit(idx)
    val shouldRename = Wire(Bool())
    idxDivNf := idx / nf
    idxModNf := idx % nf
    shouldRename := MuxCase(false.B, Seq(
      (sew === 0.U) -> (idxDivNf(vlenShiftBits - 1, 0) === 0.U),
      (sew === 1.U) -> (idxDivNf(vlenShiftBits - 2, 0) === 0.U),
      (sew === 2.U) -> (idxDivNf(vlenShiftBits - 3, 0) === 0.U),
      (sew === 3.U) -> (idxDivNf(vlenShiftBits - 4, 0) === 0.U)
    ))
    vecIdxAddend := MuxCase(0.U, Seq(
      (sew === 0.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits) / nf * nf),
      (sew === 1.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits - 1) / nf * nf),
      (sew === 2.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits - 2) / nf * nf),
      (sew === 3.U) -> (idxModNf + idx(idxBits - 1, vlenShiftBits - 3) / nf * nf),
    ))
    (shouldRename, vecIdxAddend)
  }

  io.out.zipWithIndex.foreach({ case (o, idx) =>
    val currentnum = io.current(idx)
    o.valid := io.in.valid && (currentnum < io.in.bits.uopNum)
    o.bits := io.in.bits
    o.bits.uopNum := io.in.bits.uopNum
    o.bits.uopIdx := currentnum
    o.bits.isTail := currentnum >= vl //Only VLS need this
    o.bits.isPrestart := currentnum < io.vstart //Only VLS need this

    when(io.in.bits.ctrl.isVLS) {
      val (doRn, addend) = LsRenameInfo(currentnum)
      o.bits.canRename := doRn
      o.bits.ctrl.ldest := ctrl.ldest + addend
      o.bits.ctrl.lsrc(0) := ctrl.lsrc(0)
      o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + addend, ctrl.lsrc(1))
    }.elsewhen(narrow) {
      val addend = currentnum(6, 1)
      o.bits.canRename := !currentnum(0).asBool
      o.bits.ctrl.ldest := ctrl.ldest + addend
      o.bits.ctrl.lsrc(0) := Mux(ctrl.srcType(0) === SrcType.vec, ctrl.lsrc(0) + addend, ctrl.lsrc(1))
      o.bits.ctrl.lsrc(1) := ctrl.lsrc(1) + addend
    }.elsewhen(narrowToMask) {
      val addend = currentnum
      o.bits.canRename := currentnum === 0.U
      o.bits.ctrl.ldest := ctrl.ldest
      o.bits.ctrl.lsrc(0) := Mux(ctrl.srcType(0) === SrcType.vec, ctrl.lsrc(0) + addend, ctrl.lsrc(0))
      o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + addend, ctrl.lsrc(1))
    }.otherwise {
      val addend = currentnum
      o.bits.canRename := true.B
      o.bits.ctrl.ldest := Mux(ctrl.vdWen, ctrl.ldest + addend, ctrl.ldest)
      o.bits.ctrl.lsrc(0) := Mux(ctrl.srcType(0) === SrcType.vec, ctrl.lsrc(0) + addend, ctrl.lsrc(0))
      o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + addend, ctrl.lsrc(1))
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
  private def GetUopNum(in:MicroOp):UInt = {
    val uopnum = Wire(UInt(8.W))
    val sew = in.vCsrInfo.vsew
    val lmul = MuxCase(0.U, Seq(
      (in.vCsrInfo.vlmul === 0.U) -> 1.U,
      (in.vCsrInfo.vlmul === 1.U) -> 2.U,
      (in.vCsrInfo.vlmul === 2.U) -> 4.U,
      (in.vCsrInfo.vlmul === 3.U) -> 8.U,
    ))
    val lmulWiden = MuxCase(8.U, Seq(
      (in.vCsrInfo.vlmul === 0.U) -> 2.U,
      (in.vCsrInfo.vlmul === 1.U) -> 4.U,
      (in.vCsrInfo.vlmul === 2.U) -> 8.U,
    ))
    val elementInRegGroup = Cat(lmul, 0.U(4.W)) >> sew
    uopnum := Mux(in.ctrl.isVLS, elementInRegGroup, Mux(in.ctrl.widen === Widen.NotWiden, lmul, lmulWiden))
    uopnum
  }

  private val remain = RegInit(0.U(log2Ceil(VLEN + 1).W))
  private val remainWire = WireInit(remain)
  private val leaving = PopCount(io.out.map(_.fire))
  private val remainUpdate = io.out.map(_.fire).reduce(_|_)
  private val uopNum = GetUopNum(io.in.bits)
  private val currentIndices = RegInit(VecInit(Seq.tabulate(splitNum)(_.U(log2Ceil(VLEN).W))))
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

  // private val assertValid = RegInit(false.B)
  // assertValid := io.in.valid
  when(io.in.valid){assert(leaving <= remainWire)}
}
