package xiangshan.vector.viwaitqueue
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{MicroOp, Narrow, Redirect, SrcType, Widen, XSModule}

class SplitNetwork(splitNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(new MicroOp))
    val out = Vec(splitNum, Decoupled(new MicroOp))
    val vstart = Input(UInt(7.W))
    val redirect = Input(Valid(new Redirect))
  })
  private def SplitUop(in:Valid[MicroOp], remain:UInt, vstart:UInt): Vec[Valid[MicroOp]] = {
    val res = Wire(Vec(splitNum, Valid(new MicroOp)))
    val vl = in.bits.vCsrInfo.vl
    val sew = in.bits.vCsrInfo.vsew
    val narrow = io.in.bits.ctrl.narrow === Narrow.Narrow
    val narrowToMask = io.in.bits.ctrl.narrow === Narrow.Narrow2
    val ctrl = in.bits.ctrl
    val nf = in.bits.ctrl.NField +& 1.U

    def LsRenameInfo(idx:UInt):(Bool, UInt) = {
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

    res.zipWithIndex.foreach({case(o , idx) =>
      val currentnum = Wire(UInt(7.W))
      currentnum := in.bits.uopNum - remain + idx.U
      o.valid := io.in.valid && (idx.U < remain)
      o.bits := in.bits
      o.bits.uopNum := in.bits.uopNum
      o.bits.uopIdx := currentnum
      o.bits.isTail := currentnum >= vl //Only VLS need this
      o.bits.isPrestart := currentnum < vstart //Only VLS need this

      when(in.bits.ctrl.isVLS){
        val (doRn, addend) = LsRenameInfo(currentnum)
        o.bits.canRename := doRn
        o.bits.ctrl.ldest := ctrl.ldest + addend
        o.bits.ctrl.lsrc(0) := ctrl.lsrc(0)
        o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + addend, ctrl.lsrc(1))
        o.bits.ctrl.lsrc(2) := ctrl.lsrc(2) + addend
      }.elsewhen(narrow){
        val addend = currentnum(6, 1)
        o.bits.canRename := currentnum(0).asBool
        o.bits.ctrl.ldest := ctrl.ldest + addend
        o.bits.ctrl.lsrc(0) := Mux(ctrl.srcType(0) === SrcType.vec, ctrl.lsrc(0) + addend, ctrl.lsrc(1))
        o.bits.ctrl.lsrc(1) := ctrl.lsrc(1) + addend
        o.bits.ctrl.lsrc(2) := ctrl.lsrc(2) + addend
      }.elsewhen(narrowToMask){
        val addend = currentnum
        o.bits.canRename := currentnum === 0.U
        o.bits.ctrl.ldest := ctrl.ldest
        o.bits.ctrl.lsrc(0) := Mux(ctrl.srcType(0) === SrcType.vec, ctrl.lsrc(0) + addend, ctrl.lsrc(0))
        o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + addend, ctrl.lsrc(1))
        o.bits.ctrl.lsrc(2) := Mux(ctrl.srcType(2) === SrcType.vec, ctrl.lsrc(2) + addend, ctrl.lsrc(2))
      }.otherwise{
        o.bits.canRename := true.B
        o.bits.pdest := in.bits.pdest + idx.U
        o.bits.ctrl.lsrc(0) := Mux(ctrl.srcType(0) === SrcType.vec, ctrl.lsrc(0) + idx.U, ctrl.lsrc(0))
        o.bits.ctrl.lsrc(1) := Mux(ctrl.srcType(1) === SrcType.vec, ctrl.lsrc(1) + idx.U, ctrl.lsrc(1))
        o.bits.ctrl.lsrc(2) := Mux(ctrl.srcType(2) === SrcType.vec, ctrl.lsrc(2) + idx.U, ctrl.lsrc(2))
      }
    })
    res
  }
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

  when(io.in.bits.robIdx.needFlush(io.redirect) && io.in.valid) {
    remain := 0.U
  }.elsewhen(remain === 0.U && io.in.valid) {
    remain := uopNum - leaving
  }.elsewhen(remainUpdate) {
    remain := remain - leaving
  }

  when(remain === 0.U && io.in.valid){
    remainWire := uopNum
  }

  io.in.ready := (remainWire === leaving) && !io.redirect.valid

  private val in_v = Wire(Valid(new MicroOp))
  in_v.valid := io.in.valid
  in_v.bits := io.in.bits
  in_v.bits.uopNum := uopNum

  private val out_v = SplitUop(in_v, remainWire, io.vstart)

  io.out.zipWithIndex.foreach({ case (o, i) =>
    o.valid := out_v(i).valid
    o.bits := out_v(i).bits
  })

  // private val assertValid = RegInit(false.B)
  // assertValid := io.in.valid
  when(io.in.valid){assert(leaving <= remainWire)}
}
