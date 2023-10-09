package xiangshan.vector.viwaitqueue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{MicroOp, Redirect, XSModule}
class DequeuePipeline(PipelineWidth: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val in = Vec(PipelineWidth, Flipped(Decoupled(new MicroOp)))
    val out = Vec(PipelineWidth, Decoupled(new MicroOp))
    val redirect = Input(new Valid(new Redirect))
  })
  assert(PopCount(io.out.map(_.ready)) === PipelineWidth.U || PopCount(io.out.map(_.ready)) === 0.U)

  for((out, in) <- io.out.zip(io.in)){
    val validReg = RegInit(false.B)
    val bitsReg = Reg(new MicroOp)
    val allowPipe = out.ready || !validReg
    in.ready := allowPipe && !io.redirect.valid
    when(validReg && io.redirect.valid && bitsReg.robIdx.needFlush(io.redirect)){
      validReg := false.B
    }.elsewhen(allowPipe){
      validReg := in.valid
    }
    when(in.fire){
      bitsReg := in.bits
    }

    out.valid := validReg && !io.redirect.valid
    out.bits := bitsReg
  }
}
