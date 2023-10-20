package xiangshan.vector.viwaitqueue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import xiangshan.vector._
import xs.utils._
import xiangshan.backend.dispatch.DispatchQueue
import xiangshan.backend.rob._
import xiangshan.vector.writeback.VmbAlloc
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
    val vmbAlloc = Flipped(new VmbAlloc)
    val canRename = Input(Bool())
    val redirect = Input(Valid(new Redirect))
    val enq = new NewWqEnqIO
    val out = Vec(VIRenameWidth, DecoupledIO(new MicroOp))
    val vmbInit = Output(Valid(new MicroOp))
  })
  private class WqPtr extends CircularQueuePtr[WqPtr](VIWaitQueueWidth)
  private val deqPtr = RegInit(0.U.asTypeOf(new WqPtr))
  private val enqPtr = RegInit(0.U.asTypeOf(new WqPtr))
  private val mergePtrVec = RegInit(VecInit(Seq.tabulate(VIDecodeWidth)(i => i.U.asTypeOf(new WqPtr))))
  private val mergePtr = mergePtrVec.head
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
    enqPtr := enqPtrNext
  }.elsewhen(enqValids.reduce(_|_)){
    enqPtrNext := enqPtr + enqNum
    enqPtr := enqPtrNext
  }

  //MergeId Allocation Logics
  private val needMergeNum = distanceBetween(enqPtr, mergePtr)
  io.vmbAlloc.req.zipWithIndex.foreach({case(req, i) =>
    req.valid := i.U < needMergeNum & !io.redirect.valid
    table.io.read(i).addr := mergePtrVec(i).value
    req.bits := table.io.read(i).data.uop.robIdx
  })
  table.io.vmsIdAllocte.zipWithIndex.foreach({ case(va, i) =>
    va.en := io.vmbAlloc.resp(i).valid
    va.data := io.vmbAlloc.resp(i).bits
    va.addr := mergePtrVec(i).value
  })

  private val mergeAllocs = io.vmbAlloc.resp.map(_.valid)
  private val mergeAllocNum = PopCount(mergeAllocs)
  when(io.redirect.valid){
    when(enqPtrNext < mergePtr){
      mergePtrVec.zipWithIndex.foreach({case(ptr, i) => ptr := enqPtrNext + i.U})
    }
  }.elsewhen(mergeAllocs.reduce(_|_)){
    mergePtrVec.foreach(ptr => ptr := ptr + mergeAllocNum)
  }

  //Misc entry update logics
  table.io.vtypeWb.valid := io.vtypeWbData.valid
  table.io.vtypeWb := io.vtypeWbData
  table.io.robEnq := io.robin
  table.io.redirect := io.redirect

  //Dequeue logics
  table.io.deq.addr := deqPtr.value
  private val deqUop = table.io.deq.data
  private val deqHasException = deqUop.uop.cf.exceptionVec.reduce(_|_)
  private val raiseII = deqUop.uop.ctrl.wvstartType === VstartType.hold && io.vstart =/= 0.U

  private val vstartHold = RegInit(false.B)
  private val hasValid = deqPtr =/= enqPtr && !vstartHold
  private val uopRdy = deqUop.vtypeRdy && deqUop.robEnqueued && deqUop.mergeIdAlloc && deqUop.state === WqState.s_waiting
  private val directlyWb = deqHasException || deqUop.uop.uopNum === 0.U || io.vstart >= deqUop.uop.vCsrInfo.vl || raiseII

  private val splitDriver = Module(new DequeuePipeline(1))
  splitDriver.io.redirect := io.redirect
  splitDriver.io.in(0).bits := deqUop.uop
  splitDriver.io.in(0).valid := hasValid && uopRdy && !directlyWb

  splitNetwork.io.redirect := io.redirect
  splitNetwork.io.in.valid := splitDriver.io.out(0).valid
  splitNetwork.io.in.bits := splitDriver.io.out(0).bits
  splitDriver.io.out(0).ready := splitNetwork.io.in.ready
  splitNetwork.io.vstart := RegNextN(io.vstart, 3)

  private val deqValid = hasValid && uopRdy && (splitDriver.io.in(0).ready || directlyWb)
  when(deqValid){
    deqPtr := deqPtr + 1.U
  }
  when(deqValid && !deqHasException && io.vstart =/= 0.U){
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
  vmbInit.bits.uopIdx := 0.U
  private val isNarrowToMask = deqUop.uop.vctrl.isNarrow &&
    deqUop.uop.vctrl.eewType(2) === EewType.const &&
    deqUop.uop.vctrl.eew(2) === EewVal.mask
  when(directlyWb){
    vmbInit.bits.uopNum := 0.U
  }.elsewhen(deqUop.uop.uopNum =/= 0.U) {
    when(isNarrowToMask || (deqUop.uop.vctrl.ff && !deqUop.uop.vctrl.isLs)) {
      vmbInit.bits.uopNum := 1.U
    }
  }
  when(raiseII){
    vmbInit.bits.cf.exceptionVec(illegalInstr) := true.B
  }
  io.vmbInit := Pipe(vmbInit)
}
