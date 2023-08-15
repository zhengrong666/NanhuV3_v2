package xiangshan.vector.vbackend.vissue.vrs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.rob.RobPtr
import xiangshan.{MicroOp, Redirect, XSBundle, XSModule}
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, ParallelPriorityMux, UIntToMask}

class OIQEntry(implicit p: Parameters) extends XSBundle{
  val robIdx = new RobPtr
  val uopIdx = UInt(3.W)
  val uopMax = UInt(3.W)
}

object OIQ {
  def SqueezeEnqueue(in: Seq[Valid[OIQEntry]], p: Parameters): Vec[Valid[OIQEntry]] = {
    val validMatrix = Wire(Vec(in.length + 1, Vec(in.length, Bool())))
    validMatrix.head.zip(in.map(_.valid)).foreach({ case (a, b) => a := b })
    val dst = Wire(Vec(in.length, Valid(new OIQEntry()(p))))
    dst.zipWithIndex.foreach({ case (o, idx) =>
      val validVec = validMatrix(idx).drop(idx)
      val selOH = ParallelPriorityMux(validVec, validVec.indices.map(i => (1 << (i + idx)).U(in.length.W)))
      validMatrix(idx + 1).zip(validMatrix(idx)).zip(selOH.asBools).foreach({ case ((n, p), s) =>
        n := p && (!s)
      })
      o.valid := validVec.reduce(_ | _)
      o.bits := Mux1H(selOH, in.map(_.bits))
    })
    dst
  }
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
  private val validsMask = Mux(deqPtr.value <= enqPtr.value, enqXorDeq, (~enqXorDeq).asUInt)
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

  private val actualEnqs = WireInit(Vec(enqNum, Valid(new OIQEntry)))
  for((e, i) <- actualEnqs.zipWithIndex) {
    e.valid := io.enq(i).valid && enqNeedAllocate(i)
    e.bits <> io.enq(i).bits
    e.bits.uopIdx := 0.U
  }
  // actualEnqs.zip(enqNeedAllocate).foreach({case(e, na) =>
  //   e.valid := e.valid && na
  //   e.bits.uopIdx := 0.U
  // })

  io.enqCanAccept := PopCount(actualEnqs.map(_.valid)) <= emptyEntriesNum && !io.redirect.valid

  private val squeezedEnqs = OIQ.SqueezeEnqueue(actualEnqs, p)

  private val update = Wire(Valid(new OIQEntry))
  update.valid := io.issued
  update.bits := io.ctrl.bits
  update.bits.uopIdx := io.ctrl.bits.uopIdx + 1.U

  private val writeAddrs = squeezedEnqs.indices.map(i => (enqPtr + i.U).value) :+ deqPtr.value
  private val enqPairs = (squeezedEnqs :+ update).zip(writeAddrs)

  for((mem, addr) <- array.zipWithIndex){
    val valids = enqPairs.map(wreq => wreq._1.valid && wreq._2 === addr.U)
    val dataSeq = enqPairs.map(_._1.bits)
    val data = Mux1H(valids, dataSeq)
    when(valids.reduce(_ | _)) {
      mem := data
    }
  }

  private val actualEnqNum = PopCount(squeezedEnqs.map(_.valid))
  when(io.redirect.valid) {
    enqPtr := enqPtr - flushNum
  }.elsewhen(actualEnqNum =/= 0.U) {
    enqPtr := enqPtr + actualEnqNum
  }

  private val deqPtrValOH = UIntToOH(deqPtr.value)
  io.ctrl.valid := (deqPtrValOH & validsMask).orR && !io.redirect.valid
  io.ctrl.bits := Mux1H(deqPtrValOH, array)

  when(io.issued && update.bits.uopIdx === io.ctrl.bits.uopMax && !io.redirect.valid) {
    deqPtr := deqPtr + 1.U
  }

  assert(deqPtr <= enqPtr)
  assert(actualEnqNum <= emptyEntriesNum)
  when(io.enqCanAccept) {
    assert(PopCount(squeezedEnqs.map(_.valid)) === actualEnqNum)
  }
  for (i <- io.enq.indices) {
    when(io.enqCanAccept) {
      assert(Mux(i.U < actualEnqNum, squeezedEnqs(i).valid === true.B, squeezedEnqs(i).valid === false.B))
    }
  }

  assert(flushNum <= validEntriesNum)
  private val enqFlushNextMask = UIntToMask((enqPtr - flushNum).value, size)
  private val flushXorPresentMask = enqFlushNextMask ^ enqMask
  private val enqRollbackMask = Mux(enqPtr.value >= (enqPtr - flushNum).value, flushXorPresentMask, ~flushXorPresentMask)
  when(io.redirect.valid) {
    assert(enqRollbackMask === redirectMask, "Redirect mask should be continuous.")
  }
}

class OIQEnqBuffer(enqNum:Int)(implicit p: Parameters) extends Module with HasCircularQueuePtrHelper{
  private val size = 8
  val io = IO(new Bundle{
    val enq = Input(Vec(enqNum, Valid(new MicroOp)))
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
  private val validsMask = Mux(deqPtr.value <= enqPtr.value, enqXorDeq, (~enqXorDeq).asUInt)
  private val flushVec = Cat(array.map(_.robIdx.needFlush(io.redirect)).reverse)
  private val redirectMask = validsMask & flushVec
  private val flushNum = PopCount(redirectMask)

  io.enqCanAccept := PopCount(io.enq.map(e => e.valid && e.bits.ctrl.isOrder)) <= emptyEntriesNum && !io.redirect.valid

  private val squeezedEnqs = OIQ.SqueezeEnqueue(io.enq.map(e => {
    val res = Wire(Valid(new OIQEntry))
    res.valid := e.valid && e.bits.ctrl.isOrder && io.enqCanAccept
    res.bits.robIdx := e.bits.robIdx
    res.bits.uopIdx := e.bits.uopIdx
    res.bits.uopMax := e.bits.uopNum
    when(res.valid) {
      assert(e.bits.uopIdx <= 7.U)
      assert(e.bits.uopNum <= 7.U)
    }
    res
  }), p)

  private val writeAddrs = squeezedEnqs.indices.map(i => (enqPtr + i.U).value)
  private val enqPairs = squeezedEnqs.zip(writeAddrs)

  for ((mem, addr) <- array.zipWithIndex) {
    val valids = enqPairs.map(wreq => wreq._1.valid && wreq._2 === addr.U)
    val dataSeq = enqPairs.map(_._1.bits)
    val data = Mux1H(valids, dataSeq)
    when(valids.reduce(_ | _)) {
      mem := data
    }
  }
  private val actualEnqNum = PopCount(squeezedEnqs.map(_.valid))
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
  private val actualDeqNum = PopCount(squeezedEnqs.map(_.valid && io.deqCanAccept))
  when(actualDeqNum =/= 0.U && !io.redirect.valid) {
    deqPtr := deqPtr + actualDeqNum
  }

  assert(deqPtr <= enqPtr)
  assert(actualEnqNum <= emptyEntriesNum)
  when(io.enqCanAccept) {
    assert(PopCount(squeezedEnqs.map(_.valid)) === actualEnqNum)
  }
  for (i <- io.enq.indices) {
    when(io.enqCanAccept) {
      assert(Mux(i.U < actualEnqNum, squeezedEnqs(i).valid === true.B, squeezedEnqs(i).valid === false.B))
    }
  }

  assert(flushNum <= validEntriesNum)
  private val enqFlushNextMask = UIntToMask((enqPtr - flushNum).value, size)
  private val flushXorPresentMask = enqFlushNextMask ^ enqMask
  private val enqRollbackMask = Mux(enqPtr.value >= (enqPtr - flushNum).value, flushXorPresentMask, ~flushXorPresentMask)
  when(io.redirect.valid) {
    assert(enqRollbackMask === redirectMask, "Redirect mask should be continuous.")
  }
}

class OrderedInstructionQueue(enqNum:Int, size:Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper{
  val io = IO(new Bundle{
    val enq = Input(Vec(enqNum, Valid(new MicroOp)))
    val enqCanAccept = Output(Bool())
    val ctrl = Output(Valid(new OIQEntry))
    val issued = Input(Bool())
    val redirect = Input(Valid(new Redirect))
  })
  private val enqBuffer = Module(new OIQEnqBuffer(enqNum))
  private val mergeQueue = Module(new OIQMergeQueue(size, enqNum))
  enqBuffer.io.redirect := io.redirect
  mergeQueue.io.redirect := io.redirect
  enqBuffer.io.enq := io.enq
  io.enqCanAccept := enqBuffer.io.enqCanAccept
  mergeQueue.io.enq := enqBuffer.io.deq
  enqBuffer.io.deqCanAccept := mergeQueue.io.enqCanAccept
  io.ctrl := mergeQueue.io.ctrl
  mergeQueue.io.issued := io.issued
}
