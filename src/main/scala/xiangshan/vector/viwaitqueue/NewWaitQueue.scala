package xiangshan.vector.viwaitqueue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.vector._
import xs.utils._
import xiangshan.backend.dispatch.DispatchQueue
import xiangshan.backend.rob._
import xiangshan.vector.writeback.WbMergeBufferPtr
import xiangshan.backend.execute.fu.csr.vcsr._

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
    val vtypeWbData = Flipped(ValidIO(new VtypeWbIO))
    val robin = Vec(VIDecodeWidth, Flipped(ValidIO(new RobPtr)))
    val mergeId = Vec(VIDecodeWidth, Flipped(DecoupledIO(new WbMergeBufferPtr(VectorMergeBufferDepth))))
    val canRename = Input(Bool())
    val redirect = Input(Valid(new Redirect))
    val enq = new NewWqEnqIO
    val out = Vec(VIRenameWidth, DecoupledIO(new MicroOp))
    val vmbInit = Output(Valid(new MicroOp))
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
//  private val allocNum = PopCount(io.enq.needAlloc)
  io.enq.canAccept := emptyEntriesNum >= VIDecodeWidth.U
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
    t.data.state := Mux(e.bits.vtypeRdy, WqState.s_updating, WqState.s_waiting)
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
  table.io.vtypeWb := io.vtypeWbData
  table.io.robEnq := io.robin
  table.io.redirect := io.redirect

  //Dequeue logics
  table.io.deq.addr := deqPtr.value
  private val deqUop = table.io.deq.data

  private val vstartHold = RegInit(false.B)
  private val hasValid = deqPtr =/= enqPtr
  private val uopRdy = deqUop.vtypeRdy && deqUop.robEnqueued && deqUop.mergeIdAlloc && deqUop.state === WqState.s_waiting

  private val splitEnqPipe = Module(new DequeuePipeline(1))
  splitEnqPipe.io.redirect := io.redirect
  splitEnqPipe.io.in(0).bits := deqUop.uop
  splitEnqPipe.io.in(0).valid := hasValid && !vstartHold && uopRdy

  splitNetwork.io.redirect := io.redirect
  splitNetwork.io.in <> splitEnqPipe.io.out(0)
  splitNetwork.io.vstart := io.vstart

  private val deqValid = splitEnqPipe.io.in(0).fire
  when(deqValid){
    deqPtr := deqPtr + 1.U
  }
  when(deqValid && io.vstart =/= 0.U){
    vstartHold := true.B
  }.elsewhen(io.vstart === 0.U && vstartHold){
    vstartHold := false.B
  }

  private val splitPipe = Module(new DequeuePipeline(VIRenameWidth))
  splitPipe.io.redirect := io.redirect
  splitPipe.io.in <> splitNetwork.io.out
  io.out <> splitPipe.io.out

  private val vmbInit = Wire(Valid(new MicroOp))
  vmbInit.valid := deqValid
  vmbInit.bits := deqUop.uop
  private val isLoad = deqUop.uop.ctrl.fuType === FuType.ldu
  private val isNarrowToMask = deqUop.uop.vctrl.isNarrow &&
    deqUop.uop.vctrl.eewType(2) === EewType.const &&
    deqUop.uop.vctrl.eew(2) === EewVal.mask
  private val emul = deqUop.uop.vctrl.emul
  when(deqUop.uop.uopNum =/= 0.U) {
    when(isLoad) {
      vmbInit.bits.uopNum := MuxCase(1.U, Seq(
        (emul === 0.U(3.W)) -> 1.U,
        (emul === 1.U(3.W)) -> 2.U,
        (emul === 2.U(3.W)) -> 4.U,
        (emul === 3.U(3.W)) -> 8.U,
        (emul === 5.U(3.W)) -> 1.U,
        (emul === 6.U(3.W)) -> 1.U,
        (emul === 7.U(3.W)) -> 1.U
      ))
    }.elsewhen(isNarrowToMask) {
      vmbInit.bits.uopNum := 1.U
    }
  }

  io.vmbInit := Pipe(vmbInit)
}
