package xiangshan.vector.vbackend.vexecute.vexu
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.issue.WakeUpInfo
import xiangshan.{MicroOp, Redirect, XSModule}
import xs.utils.LogicShiftRight
class MicroOpShiftQueue(latency:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Input(Valid(new MicroOp))
    val out = Output(Valid(new MicroOp))
    val redirect = Input(Valid(new Redirect))
  })
  require(latency > 0)

  private def DelayInput(in: Valid[MicroOp], l: Int): Valid[MicroOp] = {
    val res = Wire(Valid(new MicroOp))
    val realIn = if (l == 1) in else DelayInput(in, l - 1)
    val resValidReg = RegNext(realIn.valid, false.B)
    val resDataReg = RegEnable(realIn.bits, realIn.valid)
    val shouldBeFlushed = resDataReg.robIdx.needFlush(io.redirect)
    res.valid := resValidReg && !shouldBeFlushed
    res.bits := resDataReg
    res
  }
  io.out := DelayInput(io.in, latency - 1)

  when(io.out.valid) {
    assert(!io.out.bits.robIdx.needFlush(io.redirect))
  }
}
