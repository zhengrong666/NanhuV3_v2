package xiangshan.vector.viwaitqueue
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.rob.RobPtr
import xiangshan.{MicroOp, Redirect, XSModule}
import xiangshan.vector.HasVectorParameters
import xs.utils.ParallelPriorityMux

class SplitNetwork(splitNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Input(Decoupled(new MicroOp))
    val out = Vec(splitNum, Decoupled(new MicroOp))
    val redirect = Input(Valid(new Redirect))
  })
  private def SplitUop(in:Valid[MicroOp], remain:UInt): Vec[Valid[MicroOp]] = {
    val res = Wire(Vec(splitNum, Valid(new MicroOp)))
    //TODO: Fill split logics here
    res.foreach(_.valid := false.B)
    res := DontCare
    res
  }
  private def GetUopNum(in:MicroOp):UInt = {
    //TODO: Fill uopNum gen logics here
    0.U
  }

  private val remain = RegInit(0.U(log2Ceil(VLEN).W))
  private val remainNext = WireInit(remain)
  private val leaving = PopCount(io.out.map(_.fire))
  private val remainUpdate = io.out.map(_.fire).reduce(_|_)
  when(io.in.bits.robIdx.needFlush(io.redirect) && io.in.valid){
    remainNext := 0.U
  }.elsewhen(remain === 0.U && io.in.valid){
    remainNext := GetUopNum(io.in.bits)
  }.elsewhen(remainUpdate){
    remainNext := remain - leaving
  }
  remain := remainNext
  io.in.ready := remainNext === 0.U && !io.redirect.valid

  private val in_v = Wire(Valid(new MicroOp))
  in_v.valid := io.in.valid
  in_v.bits := io.in.bits
  private val out_v = SplitUop(in_v, remainNext)

  io.out.zipWithIndex.foreach({ case (o, i) =>
    o.valid := out_v(i).valid
    o.bits := out_v(i).bits
  })

  when(io.in.valid){assert(leaving <= remain)}
}
