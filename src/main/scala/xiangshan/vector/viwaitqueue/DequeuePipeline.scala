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
  assert(PopCount(io.in.map(_.ready)) === PipelineWidth.U || PopCount(io.in.map(_.ready)) === 0.U)

  private val pipes = Seq.fill(PipelineWidth)(Reg(Valid(new MicroOp)))
  private val globalValid = RegInit(false.B)

  private val flushed = pipes.head.bits.robIdx.needFlush(io.redirect)
  private val allowPipe = io.out.head.ready || !globalValid || flushed
  io.in.foreach(_.ready := allowPipe)

  private val enqValid = io.in.map(_.valid).reduce(_ | _)
  when(allowPipe) {
    globalValid := enqValid
  }

  io.in.zip(io.out).zip(pipes).foreach({case((in, out), pip) =>
    when(enqValid && allowPipe) {
      pip.valid := in.valid && !in.bits.robIdx.needFlush(io.redirect)
      pip.bits := in.bits
    }
    out.valid := pip.valid && globalValid
    out.bits := pip.bits
  })
}
