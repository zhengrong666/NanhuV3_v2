package xiangshan.vector.vexecute.vissue.vrs
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

class OrderedInstructionQueuePayload(entryNum:Int, enqNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val w = Input(Vec(enqNum + 1, new Bundle {
      val en: Bool = Bool()
      val addrOH: UInt = UInt(entryNum.W)
      val data: OIQEntry = new OIQEntry
    }))
    val r = new Bundle {
      val addrOH: UInt = Input(UInt(entryNum.W))
      val data: OIQEntry = Output(new OIQEntry)
    }
    val redirect = Input(Valid(new Redirect))
    val flushVec = Output(UInt(entryNum.W))
  })

  private val array = Reg(Vec(entryNum, new OIQEntry))
  for (w <- io.w) {
    for ((hit, mem) <- w.addrOH.asBools.zip(array)) {
      when(w.en && hit) {
        mem := w.data
      }
    }
  }

  io.r.data := Mux1H(io.r.addrOH, array)

  private val redirectHits = array.map(_.robIdx.needFlush(io.redirect))
  io.flushVec := Cat(redirectHits.reverse)
}

class OrderedInstructionQueue(enqNum:Int, entryNum:Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper{
  val io = IO(new Bundle{
    val enq = Vec(enqNum, Valid(new MicroOp))
    val enqCanAccept = Output(Bool())
    val ctrl = Output(Valid(new OIQEntry))
    val issued = Input(Bool())
    val redirect = Input(Valid(new Redirect))
  })
  private class OIQQueuePtr extends CircularQueuePtr[OIQQueuePtr](entryNum)

  private val payloadArray = Module(new OrderedInstructionQueuePayload(entryNum, enqNum))
  private val enqPtr = RegInit(0.U.asTypeOf(new OIQQueuePtr))
  private val deqPtr = RegInit(0.U.asTypeOf(new OIQQueuePtr))
  private val validEntriesNum = distanceBetween(enqPtr, deqPtr)
  private val emptyEntriesNum = entryNum.U - validEntriesNum
  io.enqCanAccept := PopCount(io.enq.map(e => e.valid && e.bits.ctrl.isOrder)) <= emptyEntriesNum && !io.redirect.valid

  payloadArray.io.redirect := io.redirect
  private val enqMask = UIntToMask(enqPtr.value, entryNum)
  private val deqMask = UIntToMask(deqPtr.value, entryNum)
  private val enqXorDeq = enqMask ^ deqMask
  private val validsMask = Mux(deqPtr.value <= enqPtr.value, enqXorDeq, (~enqXorDeq).asUInt)
  private val redirectMask = validsMask & payloadArray.io.flushVec
  private val flushNum = PopCount(redirectMask)

  private def SqueezeEnqueue(in: Seq[Valid[OIQEntry]]): Vec[Valid[OIQEntry]] = {
    val validMatrix = Wire(Vec(in.length + 1, Vec(in.length, Bool())))
    validMatrix.head.zip(in.map(_.valid)).foreach({ case (a, b) => a := b })
    val dst = Wire(Vec(in.length, Valid(new OIQEntry)))
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

  private val squeezedEnqs = SqueezeEnqueue(io.enq.map(e => {
    val res = Wire(Valid(new OIQEntry))
    res.valid := e.valid && e.bits.ctrl.isOrder && io.enqCanAccept
    res.bits.robIdx := e.bits.robIdx
    res.bits.uopIdx := e.bits.uopIdx
    res
  }))
  payloadArray.io.w.take(enqNum).zip(squeezedEnqs).zipWithIndex.foreach({case((w, e), i) =>
    w.en := e.valid
    w.addrOH := UIntToOH((enqPtr + i.U).value)
    w.data := e.bits
  })
  payloadArray.io.r.addrOH := UIntToOH(deqPtr.value)
  io.ctrl.valid := deqPtr < enqPtr
  io.ctrl.bits := payloadArray.io.r.data

  payloadArray.io.w.last.en := io.issued
  payloadArray.io.w.last.addrOH := UIntToOH(deqPtr.value)
  payloadArray.io.w.last.data := payloadArray.io.r.data
  payloadArray.io.w.last.data.uopIdx := Mux(io.issued, payloadArray.io.r.data.uopIdx + 1.U, payloadArray.io.r.data)

  private val actualEnqNum = PopCount(squeezedEnqs.map(_.valid))
  when(io.redirect.valid) {
    enqPtr := enqPtr - flushNum
  }.elsewhen(actualEnqNum =/= 0.U) {
    enqPtr := enqPtr + actualEnqNum
  }

  private val shouldDeq = io.issued && payloadArray.io.r.data.uopIdx === payloadArray.io.r.data.uopMax
  when(shouldDeq && !io.redirect.valid) {
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
  private val enqFlushNextMask = UIntToMask((enqPtr - flushNum).value, entryNum)
  private val flushXorPresentMask = enqFlushNextMask ^ enqMask
  private val enqRollbackMask = Mux(enqPtr.value >= (enqPtr - flushNum).value, flushXorPresentMask, ~flushXorPresentMask)
  when(io.redirect.valid) {
    assert(enqRollbackMask === redirectMask, "Redirect mask should be continuous.")
  }
}
