package xiangshan.vector.viwaitqueue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.vector._
import xs.utils._
import xiangshan.backend.dispatch.DispatchQueue
import xiangshan.backend.rob._
import xiangshan.vector.writeback.WbMergeBufferPtr

class NewVIMop(implicit p: Parameters) extends VectorBaseBundle {
  val uop = new MicroOp
  val vtypeRdy = Bool()
}

class NewWqEnqIO(implicit p: Parameters) extends VectorBaseBundle  {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  val needAlloc = Vec(VIDecodeWidth, Input(Bool()))
  val req = Vec(VIDecodeWidth, Flipped(ValidIO(new NewVIMop)))
}

class NewWaitQueue(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    //val hartId = Input(UInt(8.W))
    val vstart = Input(UInt(7.W))
    val vtypeWbData = Flipped(ValidIO(new ExuOutput))
    val robin = Vec(VIDecodeWidth, Flipped(ValidIO(new RobPtr)))
    val mergeId = Vec(VIDecodeWidth, Flipped(DecoupledIO(new WbMergeBufferPtr(VectorMergeBufferDepth))))
    val canRename = Input(Bool())
    val redirect = Input(Valid(new Redirect))
    val enq = new NewWqEnqIO
    val out = Vec(VIRenameWidth, DecoupledIO(new MicroOp))
  })
  private class WqPtr extends CircularQueuePtr[WqPtr](VIWaitQueueWidth)
  private val deqPtr = RegInit(0.U.asTypeOf(new WqPtr))
  private val enqPtr = RegInit(0.U.asTypeOf(new WqPtr))
  private val mergePtr = RegInit(0.U.asTypeOf(new WqPtr))
  private val table = Module(new VIWaitQueueArray)
  private val splitNetwork = Module(new SplitNetwork(VIRenameWidth))

  private val validEntriesNum = distanceBetween(enqPtr, deqPtr)
  private val emptyEntriesNum = VIWaitQueueWidth.U - validEntriesNum

  private val enqMask = UIntToMask(enqPtr.value, VIWaitQueueWidth)
  private val deqMask = UIntToMask(deqPtr.value, VIWaitQueueWidth)
  private val enqXorDeq = enqMask ^ deqMask
  private val validMask = Mux(deqPtr.value <= enqPtr.value, enqXorDeq, (~enqXorDeq).asUInt)
  private val redirectMask = validMask & table.io.flushMask
  private val flushNum = PopCount(redirectMask)

  //Enqueue Logics
  private val allocNum = PopCount(io.enq.needAlloc)
  io.enq.canAccept := allocNum <= emptyEntriesNum
  io.enq.isEmpty := deqPtr === enqPtr
  private val enqAddrDelta = Wire(Vec(VIDecodeWidth, UInt(VIWaitQueueWidth.W)))
  enqAddrDelta.zipWithIndex.foreach({case(d, i) =>
    if(i == 0){
      d := 0.U
    } else {
      d := PopCount(io.enq.needAlloc.take(i))
    }
  })
  table.io.enq.zip(io.enq.req).zip(enqAddrDelta).foreach({case((t, e), d) =>
    t.wen := e.valid && io.enq.canAccept
    t.addr := (enqPtr + d).value
    t.data.uop := e.bits.uop
    t.data.robEnqueued := false.B
    t.data.mergeIdAlloc := false.B
    t.data.vtypeRdy := e.bits.vtypeRdy
  })

  private val enqValids = io.enq.req.map(_.valid && io.enq.canAccept)
  private val enqNum = PopCount(enqValids)
  private val enqPtrNext = WireInit(enqPtr)
  when(io.redirect.valid){
    enqPtrNext := enqPtr - flushNum
  }.elsewhen(enqValids.reduce(_|_)){
    enqPtrNext := enqPtr + enqNum
  }
  enqPtr := enqPtrNext


  //MergeId Allocation Logics
  private val needMergeNum = distanceBetween(enqPtr, mergePtr)

  io.mergeId.zipWithIndex.foreach({case(m, i) =>
    m.ready := needMergeNum >= (i + 1).U
  })

  table.io.vmsIdAllocte.zipWithIndex.foreach({ case(va, i) =>
    va.en := io.mergeId(i).fire
    va.data := io.mergeId(i).bits
    va.addr := (mergePtr + i.U).value
  })

  private val mergeAllocs = io.mergeId.map(_.fire)
  private val mergeAllocNum = PopCount(mergeAllocs)
  when(io.redirect.valid){
    when(enqPtrNext < mergePtr){
      mergePtr := enqPtrNext
    }
  }.elsewhen(mergeAllocs.reduce(_|_)){
    mergePtr := mergePtr + mergeAllocNum
  }

  //Misc entry update logics
  table.io.vtypeWb.valid := io.vtypeWbData.valid
  table.io.vtypeWb.bits.data := io.vtypeWbData.bits.data
  table.io.vtypeWb.bits.uop := io.vtypeWbData.bits.uop
  table.io.robEnq := io.robin
  table.io.redirect := io.redirect

  //Dequeue logics
  table.io.deq.addr := deqPtr.value
  private val deqUop = table.io.deq.data

  private val vstartHold = RegInit(false.B)
  private val hasValid = deqPtr =/= enqPtr
  private val uopRdy = deqUop.vtypeRdy && deqUop.robEnqueued && deqUop.mergeIdAlloc
  splitNetwork.io.redirect := io.redirect
  splitNetwork.io.in.valid := hasValid && !vstartHold && uopRdy
  splitNetwork.io.in.bits := deqUop.uop
  splitNetwork.io.vstart := io.vstart

  private val deqValid = splitNetwork.io.in.fire
  when(deqValid){
    deqPtr := deqPtr + 1.U
  }
  when(deqValid && io.vstart =/= 0.U){
    vstartHold := true.B
  }.elsewhen(io.vstart === 0.U && vstartHold){
    vstartHold := false.B
  }

  splitNetwork.io.out.foreach(_.ready := io.canRename)
  io.out.zip(splitNetwork.io.out).foreach({case(a, b) =>
    a.valid := b.valid
    a.bits := b.bits
  })

}
