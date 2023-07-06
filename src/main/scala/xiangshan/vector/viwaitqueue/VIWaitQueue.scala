package xiangshan.vector.viwaitqueue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.vector._
import xiangshan.vector.vtyperename.VtypeInfo
import xs.utils._
import xiangshan.backend.rob._


class vimop(implicit p: Parameters) extends VectorBaseBundle {
  val MicroOp = new MicroOp
  val state = 3.U(1.W)
}

class WqPtr(implicit p: Parameters) extends CircularQueuePtr[WqPtr](
  p => p(XSCoreParamsKey).RobSize
) with HasCircularQueuePtrHelper {

  def needFlush(redirect: Valid[Redirect]): Bool = {
    val flushItself = redirect.bits.flushItself() && this === redirect.bits.robIdx
    redirect.valid && (flushItself || isAfter(this, redirect.bits.robIdx))
  }

  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR
}

object WqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): WqPtr = {
    val ptr = Wire(new WqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class WqEnqIO(implicit p: Parameters) extends VectorBaseBundle  {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for robIdx gen and walk
  val needAlloc = Vec(VIDecodeWidth, Input(Bool()))
  val req = Vec(VIDecodeWidth, Flipped(DecoupledIO(new MicroOp)))
  val resp = Vec(VIDecodeWidth, Output(new WqPtr))
}

class VIWaitQueue(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {

  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val redirect = Input(Valid(new Redirect))
    val enq = new WqEnqIO
    val vtype = Vec(VIDecodeWidth, Flipped(ValidIO(new VtypeInfo)))
    val robin = Vec(VIDecodeWidth, Flipped(ValidIO(new RobPtr)))
    val WqFull = Output(Bool())
  })

  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  val enqPtrVec = Wire(Vec(VIDecodeWidth, new WqPtr))
  val deqPtr = Wire(new WqPtr)
  val enqPtr = enqPtrVec.head
  val vtypePtr = RegInit(deqPtr)
  val waitPtr = RegInit(deqPtr)

  val allowEnqueue = RegInit(true.B)
  val isEmpty = enqPtr === deqPtr
  val isReplaying = io.redirect.valid && RedirectLevel.flushItself(io.redirect.bits.level)

  /**
    * states of Wq
    */
  val s_valid :: s_wait :: s_busy :: s_invalid :: Nil = Enum(4)


  val WqData = Module(new SyncDataModuleTemplate(new vimop, VIWaitQueueWidth, 1, VIDecodeWidth, "VIWaitqueue", concatData = true))

  /**
    * pointers and counters
    */
  // dequeue pointers
  val isComplete = RegInit(false.B)
  val deqPtr_temp = deqPtr + 1
  val deqPtr_next = Mux(!io.redirect.valid && isComplete, deqPtr, deqPtr_temp)
  deqPtr := deqPtr_next

  // enqueue pointers
  val enqPtrVec_temp = RegInit(VecInit.tabulate(VIDecodeWidth)(_.U.asTypeOf(new WqPtr)))
  val canAccept = allowEnqueue
  val enqNum = Mux(canAccept, PopCount(VecInit(io.enq.req.map(_.valid))), 0.U)
  for ((ptr, i) <- enqPtrVec_temp.zipWithIndex) {
    when(io.redirect.valid) {
      ptr := ptr
    }.otherwise {
      ptr := ptr + enqNum
    }
  }
  enqPtrVec := enqPtrVec_temp


  /**
    * Enqueue
    */
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  allowEnqueue := numValidEntries + enqNum <= (VIWaitQueueWidth - VIDecodeWidth).U
  val allocatePtrVec = VecInit((0 until VIDecodeWidth).map(i => enqPtrVec(PopCount(io.enq.needAlloc.take(i)))))
  io.enq.canAccept := allowEnqueue && !io.redirect.valid
  io.enq.resp := allocatePtrVec
  val canEnqueue = VecInit(io.enq.req.map(_.valid && io.enq.canAccept))


  /**
    * read and write of data modules
    */
  val ReadAddr_next = VecInit(deqPtr_next.value)

  WqData.io.wen := canEnqueue
  WqData.io.waddr := allocatePtrVec.map(_.value)
  WqData.io.wdata.zip(io.enq.req.map(_.bits)).foreach { case (wdata, req) =>
    wdata := req
    wdata.state := s_busy
  }
  WqData.io.raddr := ReadAddr_next
  val WqDataRead = WqData.io.rdata

  /**
    * instruction split (Dequeue)
    */

  val currentdata = WqDataRead(0)
  if (currentdata.state == s_valid) {
    val lmul = currentdata.MicroOp.vCsrInfo.LmulToInt()
    val sew = currentdata.MicroOp.vCsrInfo.SewToInt()
    val dnum = lmul / VIRenameWidth + 1
    val tempout = Vec(dnum,Vec(VIRenameWidth,new MicroOp))
    for (i <- 0 until lmul) {
      val data = currentdata.MicroOp
      data.uopIdx := i.U
      data.uopNum := lmul.U
      data.ctrl.lsrc(0) := data.ctrl.lsrc(0) + i.U
      data.ctrl.lsrc(1) := data.ctrl.lsrc(1) + i.U
      data.ctrl.lsrc(2) := data.ctrl.lsrc(2) + i.U
      data.ctrl.ldest := data.ctrl.ldest + i.U
      val tempnum = i / VIRenameWidth
      tempout(tempnum)(i) := data
    }
    isComplete := true.B
  }

  /**
    * vtype update
    */
  for (i <- 0 until VIDecodeWidth) {
    when (io.vtype(i).valid) {
      val ftq = io.vtype(i).bits.cf.ftqPtr
      val offset = io.vtype(i).bits.cf.ftqOffset
      WqData.io.raddr := vtypePtr.value
      val tempdata = WqData.io.rdata(0)
      if (tempdata.MicroOp.cf.ftqPtr == ftq && tempdata.MicroOp.cf.ftqOffset == offset) {
        WqData.io.waddr := vtypePtr.value
        tempdata.MicroOp.vCsrInfo.vsew := io.vtype(i).bits.ESEW
        tempdata.MicroOp.vCsrInfo.vlmul := io.vtype(i).bits.ELMUL
        tempdata.MicroOp.robIdx := io.vtype(i).bits.robIdx
        if (io.vtype(i).bits.state == 1.U) {
          tempdata.state := tempdata.state - 1.U
        } else {
          tempdata.state := tempdata.state
        }
        WqData.io.wdata := tempdata
        vtypePtr := vtypePtr + 1
      }
    }
  }

  /**
    * robenq update
    */
  for (i <- 0 until VIDecodeWidth) {
    when(io.robin(i).valid) {
      WqData.io.raddr := waitPtr.value
      val tempdata = WqData.io.rdata(0)
      if (tempdata.MicroOp.robIdx == io.robin(i).bits) {
        WqData.io.waddr := waitPtr.value
        tempdata.state := tempdata.state - 1.U
        WqData.io.wdata := tempdata
        vtypePtr := vtypePtr + 1
      }
    }
  }

}