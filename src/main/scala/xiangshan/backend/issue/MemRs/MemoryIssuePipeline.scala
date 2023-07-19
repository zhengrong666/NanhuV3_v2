package xiangshan.backend.issue.MemRs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{MicroOp, Redirect, SrcType, XSModule}
import xs.utils.LogicShiftRight

sealed class MemPipelineEntry(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val uop = new MicroOp
  val bankIdxOH: UInt = UInt(bankIdxWidth.W)
  val entryIdxOH: UInt = UInt(entryIdxWidth.W)
}

class MemoryIssuePipeline(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(DecoupledIO(new MemPipelineEntry(bankIdxWidth, entryIdxWidth)))
    val deq = DecoupledIO(new MemPipelineEntry(bankIdxWidth, entryIdxWidth))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val issueFire = Output(Bool())
    val hold = Output(Bool())
  })
  private val hold = RegInit(false.B)
  when(io.enq.fire && io.enq.bits.uop.ctrl.isVector){
    when(io.enq.bits.uop.ctrl.srcType(1) === SrcType.reg || io.enq.bits.uop.ctrl.srcType(1) === SrcType.vec){
      hold := true.B
    }
  }.elsewhen(hold){
    hold := false.B
  }
  io.hold := hold
  io.enq.ready := io.deq.ready && !hold
  io.issueFire := !hold && io.deq.fire

  private val deqValidDriverReg = RegInit(false.B)
  private val deqDataDriverReg = Reg(new MemPipelineEntry(bankIdxWidth, entryIdxWidth))
  private val shouldBeFlushed = deqDataDriverReg.uop.robIdx.needFlush(io.redirect)
  private val shouldBeCanceled = deqDataDriverReg.uop.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)

  when(!hold | shouldBeFlushed | shouldBeCanceled){
    deqValidDriverReg := io.enq.fire
  }
  when(hold){
    deqDataDriverReg.uop.lpv.foreach(l => l := LogicShiftRight(l,1))
  }.elsewhen(io.enq.fire){
    deqDataDriverReg := io.enq.bits
  }

  io.deq.valid := deqValidDriverReg && !shouldBeFlushed && !shouldBeCanceled
  io.deq.bits := deqDataDriverReg
  io.deq.bits.uop.lpv.zip(deqDataDriverReg.uop.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
  when(io.deq.valid){
    assert(!io.deq.bits.uop.robIdx.needFlush(io.redirect))
  }
}
