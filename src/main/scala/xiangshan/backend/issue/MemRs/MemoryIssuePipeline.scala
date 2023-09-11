package xiangshan.backend.issue.MemRs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.issue.SelectResp
import xiangshan.{FuType, MicroOp, Redirect, SrcType, XSModule}
import xs.utils.LogicShiftRight

sealed class MemPipelineEnqBundle(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val selectResp = new SelectResp(bankIdxWidth, entryIdxWidth)
  val uop = new MicroOp
}

sealed class MemPipelineDeqBundle(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val uop = new MicroOp
  val bankIdxOH: UInt = UInt(bankIdxWidth.W)
  val entryIdxOH: UInt = UInt(entryIdxWidth.W)
}

class MemoryIssuePipeline(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(DecoupledIO(new MemPipelineEnqBundle(bankIdxWidth, entryIdxWidth)))
    val deq = DecoupledIO(new MemPipelineDeqBundle(bankIdxWidth, entryIdxWidth))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
    val issueFire = Output(Bool())
    val hold = Output(Bool())
    val isLoad = Output(Bool())
  })
  private val hold = RegInit(false.B)
  when(io.enq.fire && io.enq.bits.selectResp.info.isVector && io.enq.bits.selectResp.info.isSgOrStride){
    hold := true.B
  }.elsewhen(hold){
    hold := false.B
  }
  io.hold := hold

  io.enq.ready := io.deq.ready && !hold
  io.issueFire := !hold && io.deq.fire

  private val deqValidDriverReg = RegInit(false.B)
  private val deqDataDriverReg = Reg(new SelectResp(bankIdxWidth, entryIdxWidth))
  private val shouldBeFlushed = deqDataDriverReg.info.robPtr.needFlush(io.redirect)
  private val shouldBeCanceled = deqDataDriverReg.info.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
  io.isLoad := deqDataDriverReg.info.fuType === FuType.ldu
  when(!hold | shouldBeFlushed | shouldBeCanceled){
    deqValidDriverReg := io.enq.fire
  }
  when(hold){
    deqDataDriverReg.info.lpv.foreach(l => l := LogicShiftRight(l,1))
  }.elsewhen(io.enq.fire){
    deqDataDriverReg := io.enq.bits.selectResp
  }

  io.deq.valid := deqValidDriverReg && !shouldBeCanceled
  io.deq.bits.uop := io.enq.bits.uop
  io.deq.bits.uop.robIdx := deqDataDriverReg.info.robPtr
  io.deq.bits.uop.ctrl.rfWen := deqDataDriverReg.info.rfWen
  io.deq.bits.uop.ctrl.fpWen := deqDataDriverReg.info.fpWen
  io.deq.bits.uop.pdest := deqDataDriverReg.info.pdest
  io.deq.bits.uop.ctrl.fuType := deqDataDriverReg.info.fuType
  io.deq.bits.uop.psrc := deqDataDriverReg.info.psrc
  io.deq.bits.uop.vm := deqDataDriverReg.info.vm
  io.deq.bits.uop.lpv.zip(deqDataDriverReg.info.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
  io.deq.bits.entryIdxOH := deqDataDriverReg.entryIdxOH
  io.deq.bits.bankIdxOH := deqDataDriverReg.bankIdxOH

  private val isVec = io.deq.bits.uop.ctrl.isVector
  private val isStd = deqDataDriverReg.info.fuType === FuType.std
  when(!isVec && isStd) {
    io.deq.bits.uop.ctrl.srcType(0) := io.enq.bits.uop.ctrl.srcType(1)
    io.deq.bits.uop.psrc(0) := deqDataDriverReg.info.psrc(1)
  }
}
