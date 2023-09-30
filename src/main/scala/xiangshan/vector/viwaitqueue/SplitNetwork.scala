package xiangshan.vector.viwaitqueue
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{MicroOp, Narrow, Redirect, Widen, XSModule}

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
    val nf = MuxCase(0.U, Seq(
      (in.bits.ctrl.NField === 0.U) -> 1.U,
      (in.bits.ctrl.NField === 1.U) -> 2.U,
      (in.bits.ctrl.NField === 2.U) -> 3.U,
      (in.bits.ctrl.NField === 3.U) -> 4.U,
      (in.bits.ctrl.NField === 4.U) -> 5.U,
      (in.bits.ctrl.NField === 5.U) -> 6.U,
      (in.bits.ctrl.NField === 6.U) -> 7.U,
      (in.bits.ctrl.NField === 7.U) -> 8.U,
    ))
    def LsShouldRename(idx:UInt):Bool = {
      MuxCase(false.B, Seq(
        (sew === 0.U) -> (idx(3, 0) === 0.U),
        (sew === 1.U) -> (idx(2, 0) === 0.U),
        (sew === 2.U) -> (idx(1, 0) === 0.U),
        (sew === 3.U) -> idx(0).asBool,
      ))
    }

    res.zipWithIndex.foreach({case(o , idx) =>
      val currentnum = in.bits.uopNum - remain + idx.U
      o.valid := io.in.valid && (idx.U < remain)
      o.bits := in.bits
      o.bits.uopNum := in.bits.uopNum
      o.bits.uopIdx := currentnum
      o.bits.isTail := currentnum >= vl //Only VLS need this
      o.bits.isPrestart := currentnum < vstart //Only VLS need this
      o.bits.canRename := true.B
      when(o.bits.ctrl.isSeg){
        o.bits.canRename := currentnum <= in.bits.ctrl.NField
      }.elsewhen(in.bits.ctrl.isVLS){
        o.bits.canRename := LsShouldRename(currentnum)
      }.elsewhen(narrow){
        o.bits.canRename := currentnum(0).asBool
      }.elsewhen(narrowToMask){
        o.bits.canRename := currentnum === 0.U
      }
      val vlsNum = currentnum >> (4.U - sew)
      val tempnum = Mux(in.bits.ctrl.isSeg, currentnum % nf,
        Mux(in.bits.ctrl.isVLS, vlsNum, currentnum))
      o.bits.ctrl.lsrc(0) := in.bits.ctrl.lsrc(0) + tempnum
      o.bits.ctrl.lsrc(1) := in.bits.ctrl.lsrc(1) + tempnum
      o.bits.ctrl.lsrc(2) := in.bits.ctrl.lsrc(2) + tempnum
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
