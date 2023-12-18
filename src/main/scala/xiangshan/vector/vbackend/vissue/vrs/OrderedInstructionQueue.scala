package xiangshan.vector.vbackend.vissue.vrs

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.rob.RobPtr
import xiangshan.{MicroOp, Redirect, XSBundle, XSModule}
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, ParallelPriorityMux, UIntToMask}

class OIQEntry(implicit p: Parameters) extends XSBundle{
  val robIdx = new RobPtr
  val uopIdx = UInt(3.W)
  val uopNum = UInt(4.W)
}

class OIQMergeQueue(size:Int, enqNum:Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper{
  val io = IO(new Bundle {
    val enq = Input(Vec(enqNum, Valid(new OIQEntry)))
    val enqCanAccept = Output(Bool())
    val ctrl = Output(Valid(new OIQEntry))
    val issued = Input(Bool())
    val redirect = Input(Valid(new Redirect))
  })

  private class MergeQueuePtr extends CircularQueuePtr[MergeQueuePtr](size)

  private val array = Reg(Vec(size, new OIQEntry))
  private val enqPtr = RegInit(0.U.asTypeOf(new MergeQueuePtr))
  private val deqPtr = RegInit(0.U.asTypeOf(new MergeQueuePtr))

  private val validEntriesNum = distanceBetween(enqPtr, deqPtr)
  private val emptyEntriesNum = size.U - validEntriesNum
  private val enqMask = UIntToMask(enqPtr.value, size)
  private val deqMask = UIntToMask(deqPtr.value, size)
  private val enqXorDeq = enqMask ^ deqMask
  private val validsMask = Mux(deqPtr.value < enqPtr.value || deqPtr === enqPtr, enqXorDeq, (~enqXorDeq).asUInt)
  private val flushVec = Cat(array.map(_.robIdx.needFlush(io.redirect)).reverse)
  private val redirectMask = validsMask & flushVec
  private val flushNum = PopCount(redirectMask)

  private val enqNeedAllocate = Wire(Vec(enqNum, Bool()))
  for(((na, e), idx) <- enqNeedAllocate.zip(io.enq).zipWithIndex){
    val memUopHitMask = Cat(array.map(_.robIdx === e.bits.robIdx && e.valid).reverse)
    val enqUopHitMask = if(idx == 0) 0.U else Cat(io.enq.take(idx).map(p => p.bits.robIdx === e.bits.robIdx && p.valid).reverse)
    val needMerge = (memUopHitMask & validsMask).orR || enqUopHitMask.orR
    na := !needMerge
  }
  dontTouch(enqNeedAllocate)

  private val actualEnqs = WireInit(io.enq)
  for((e, i) <- actualEnqs.zipWithIndex) {
    e.valid := io.enq(i).valid && enqNeedAllocate(i)
    e.bits := io.enq(i).bits
    e.bits.uopIdx := 0.U
  }

  io.enqCanAccept := PopCount(actualEnqs.map(_.valid)) <= emptyEntriesNum && !io.redirect.valid

  private val enqPtrVec = Wire(Vec(enqNum, new MergeQueuePtr))
  enqPtrVec.zipWithIndex.foreach({ case (ptr, idx) =>
    if (idx == 0) {
      ptr := enqPtr
    } else {
      ptr := enqPtr + PopCount(actualEnqs.take(idx).map(_.valid))
    }
  })

  private val update = Wire(Valid(new OIQEntry))
  update.valid := io.issued
  update.bits := io.ctrl.bits
  update.bits.uopIdx := io.ctrl.bits.uopIdx + 1.U

  private val enqPairs = (actualEnqs :+ update).zip(enqPtrVec :+ deqPtr)

  for((mem, addr) <- array.zipWithIndex){
    val valids = enqPairs.map(wreq => wreq._1.valid && wreq._2.value === addr.U)
    val dataSeq = enqPairs.map(_._1.bits)
    val data = Mux1H(valids, dataSeq)
    when(valids.reduce(_ | _)) {
      mem := data
    }
  }

  private val actualEnqNum = PopCount(actualEnqs.map(_.valid))
  when(io.redirect.valid) {
    enqPtr := enqPtr - flushNum
  }.elsewhen(actualEnqNum =/= 0.U) {
    enqPtr := enqPtr + actualEnqNum
  }

  private val deqPtrValOH = UIntToOH(deqPtr.value)
  io.ctrl.valid := (deqPtrValOH & validsMask).orR && !io.redirect.valid
  io.ctrl.bits := Mux1H(deqPtrValOH, array)

  when(io.issued && (io.ctrl.bits.uopIdx === io.ctrl.bits.uopNum - 1.U) && !io.redirect.valid) {
    deqPtr := deqPtr + 1.U
  }

  assert(deqPtr <= enqPtr)
  assert(actualEnqNum <= emptyEntriesNum)

  assert(flushNum <= validEntriesNum)
  private val enqPtrNext = enqPtr - flushNum
  private val enqFlushNextMask = UIntToMask((enqPtr - flushNum).value, size)
  private val flushXorPresentMask = enqFlushNextMask ^ enqMask
  private val enqRollbackMask = Mux(enqPtr.value > enqPtrNext.value || enqPtr === enqPtrNext, flushXorPresentMask, ~flushXorPresentMask)
  when(io.redirect.valid) {
    assert(enqRollbackMask === redirectMask, "Redirect mask should be continuous.")
  }
}

class OIQEnqBuffer(enqNum:Int)(implicit p: Parameters) extends Module with HasCircularQueuePtrHelper{
  private val size = 8
  val io = IO(new Bundle{
    val enq = Input(Vec(enqNum, Valid(new MicroOp)))
    val needAlloc = Input(Vec(enqNum, Bool()))
    val enqCanAccept = Output(Bool())
    val deq = Vec(enqNum, Valid(new OIQEntry))
    val deqCanAccept = Input(Bool())
    val redirect = Input(Valid(new Redirect))
  })
  private class EnqBufferPtr extends CircularQueuePtr[EnqBufferPtr](size)
  private val array = Reg(Vec(size, new OIQEntry))
  private val enqPtr = RegInit(0.U.asTypeOf(new EnqBufferPtr))
  private val deqPtr = RegInit(0.U.asTypeOf(new EnqBufferPtr))

  private val validEntriesNum = distanceBetween(enqPtr, deqPtr)
  private val emptyEntriesNum = size.U - validEntriesNum
  private val enqMask = UIntToMask(enqPtr.value, size)
  private val deqMask = UIntToMask(deqPtr.value, size)
  private val enqXorDeq = enqMask ^ deqMask
  private val validsMask = Mux(deqPtr.value < enqPtr.value || deqPtr === enqPtr, enqXorDeq, (~enqXorDeq).asUInt)
  private val flushVec = Cat(array.map(_.robIdx.needFlush(io.redirect)).reverse)
  private val redirectMask = validsMask & flushVec
  private val flushNum = PopCount(redirectMask)

  io.enqCanAccept := enqNum.U <= emptyEntriesNum && !io.redirect.valid
  private val enqPtrVec = Wire(Vec(enqNum, new EnqBufferPtr))
  enqPtrVec.zipWithIndex.foreach({case(ptr, idx) =>
    if(idx == 0){
      ptr := enqPtr
    } else {
      ptr := enqPtr + PopCount(io.needAlloc.take(idx))
    }
  })

  private val enqPairs = io.enq.zip(enqPtrVec)

  for ((mem, addr) <- array.zipWithIndex) {
    val valids = enqPairs.map(wreq => wreq._1.valid && wreq._2.value === addr.U)
    val dataSeq = enqPairs.map(_._1.bits)
    val data = Mux1H(valids, dataSeq)
    when(valids.reduce(_ | _)) {
      mem.robIdx := data.robIdx
      mem.uopIdx := data.uopIdx
      mem.uopNum := data.uopNum
    }
  }
  private val actualEnqNum = PopCount(io.enq.map(_.valid))
  when(io.redirect.valid) {
    enqPtr := enqPtr - flushNum
  }.elsewhen(actualEnqNum =/= 0.U) {
    enqPtr := enqPtr + actualEnqNum
  }

  for((r, idx) <- io.deq.zipWithIndex){
    val ptrValOH = UIntToOH((deqPtr + idx.U).value)
    r.bits := Mux1H(ptrValOH, array)
    r.valid := (ptrValOH & validsMask).orR && !io.redirect.valid
  }
  private val actualDeqNum = PopCount(io.deq.map(_.valid && io.deqCanAccept))
  when(actualDeqNum =/= 0.U && !io.redirect.valid) {
    deqPtr := deqPtr + actualDeqNum
  }

  assert(deqPtr <= enqPtr)
  assert(actualEnqNum <= emptyEntriesNum)

  assert(flushNum <= validEntriesNum)
  private val enqPtrNext = enqPtr - flushNum
  private val enqFlushNextMask = UIntToMask((enqPtr - flushNum).value, size)
  private val flushXorPresentMask = enqFlushNextMask ^ enqMask
  private val enqRollbackMask = Mux(enqPtr.value > enqPtrNext.value || enqPtr === enqPtrNext, flushXorPresentMask, ~flushXorPresentMask)
  when(io.redirect.valid) {
    assert(enqRollbackMask === redirectMask, "Redirect mask should be continuous.")
  }
}

class OrderedInstructionQueue(enqNum:Int, size:Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper{
  val io = IO(new Bundle{
    val enq = Input(Vec(enqNum, Valid(new MicroOp)))
    val needAlloc = Input(Vec(enqNum, Bool()))
    val enqCanAccept = Output(Bool())
    val ctrl = Output(Valid(new OIQEntry))
    val issued = Input(Bool())
    val redirect = Input(Valid(new Redirect))
  })
  private val enqBuffer = Module(new OIQEnqBuffer(enqNum))
  private val mergeQueue = Module(new OIQMergeQueue(size, enqNum))
  enqBuffer.io.redirect := io.redirect
  mergeQueue.io.redirect := io.redirect

  enqBuffer.io.needAlloc := io.needAlloc
  enqBuffer.io.enq := io.enq
  io.enqCanAccept := enqBuffer.io.enqCanAccept
  mergeQueue.io.enq := enqBuffer.io.deq
  enqBuffer.io.deqCanAccept := mergeQueue.io.enqCanAccept
  io.ctrl := mergeQueue.io.ctrl
  mergeQueue.io.issued := io.issued
}
