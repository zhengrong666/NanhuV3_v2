package xiangshan.vector.viwaitqueue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.vector._
import xs.utils._
import utils._
import xiangshan.backend.rob._
import xiangshan.vector.writeback.WbMergeBufferPtr


class VIMop(implicit p: Parameters) extends VectorBaseBundle {
  val MicroOp = new MicroOp
  val state = UInt(3.W)
}

class WaitQueueState(implicit p: Parameters) extends VectorBaseBundle {
  val state = UInt(3.W)
  val vtypeEn = Bool()
  val robenqEn = Bool()
  val mergeidEn = Bool()
  val robIdx = new RobPtr
  val vtypeRegIdx = UInt(3.W)
  val vtypeInfo = new VICsrInfo
  val mergeIdx = new WbMergeBufferPtr(VectorMergeBufferDepth)
}

class WqPtr(implicit p: Parameters) extends CircularQueuePtr[WqPtr](
  p => p(VectorParametersKey).vWaitQueueNum
) with HasCircularQueuePtrHelper

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
  val needAlloc = Vec(VIDecodeWidth, Input(Bool()))
  val req = Vec(VIDecodeWidth, Flipped(ValidIO(new VIMop)))
}

class VIWaitQueue(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {

  val io = IO(new Bundle() {
    //val hartId = Input(UInt(8.W))
    val vstart = Input(UInt(7.W))
    val vtypeWbData = Vec(VIDecodeWidth, Flipped(ValidIO(new ExuOutput)))
    val robin = Vec(VIDecodeWidth, Flipped(ValidIO(new RobPtr)))
    val MergeId = Vec(VIDecodeWidth, Flipped(DecoupledIO(new WbMergeBufferPtr(VectorMergeBufferDepth))))
    val canRename = Input(Bool())
    val redirect = Input(Valid(new Redirect))
    val enq = new WqEnqIO
    val out = Vec(VIRenameWidth, DecoupledIO(new MicroOp))
    //val WqFull = Output(Bool())
  })

  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  val enqPtrVec = RegInit(VecInit(Seq.fill(VIDecodeWidth)(0.U.asTypeOf(new WqPtr))))
  val deqPtr = RegInit(0.U.asTypeOf(new WqPtr))
  val enqPtr = enqPtrVec.head
  val mergePtr = RegInit(0.U.asTypeOf(new WqPtr))

  val allowEnqueue = RegInit(true.B)
  val isEmpty = enqPtr === deqPtr
  io.enq.isEmpty := isEmpty
  val isReplaying = io.redirect.valid && !io.canRename

  /**
    * data module and states module of Wq
    */

  val WqData = Module(new SyncDataModuleTemplate(new MicroOp, VIWaitQueueWidth, 1, VIDecodeWidth, "VIWaitqueue", concatData = true))
  val WqStateAraay = RegInit(VecInit(Seq.fill(VIWaitQueueWidth)(0.U.asTypeOf(new WaitQueueState))))

  /**
    * pointers and counters
    */
  // dequeue pointers
  val isComplete = RegInit(false.B)
  isComplete := RegEnable(false.B, isComplete)
  val deqPtr_temp = deqPtr + 1.U
  val deqPtr_next = Mux(isComplete, deqPtr, deqPtr_temp)
  deqPtr := deqPtr_next

  // enqueue pointers
  val enqPtrVec_temp = RegInit(VecInit.tabulate(VIDecodeWidth)(_.U.asTypeOf(new WqPtr)))
  val enqNum = Mux(allowEnqueue, PopCount(VecInit(io.enq.req.map(_.valid))), 0.U)
  for ((ptr, i) <- enqPtrVec_temp.zipWithIndex) {
    when(!isReplaying) {
      ptr := ptr + enqNum
    }
  }
  when(allowEnqueue && !isReplaying) {
    enqPtrVec := enqPtrVec_temp
  }

  when(isReplaying) {
    deqPtr := 0.U.asTypeOf(new WqPtr)
    enqPtrVec := VecInit.tabulate(VIDecodeWidth)(_.U.asTypeOf(new WqPtr))
  }

  /**
    * Enqueue
    * read and write of data modules
    */
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  when(!isReplaying) {
    allowEnqueue := numValidEntries + enqNum <= (VIWaitQueueWidth - VIDecodeWidth).U
  }
  val allocatePtrVec = VecInit((0 until VIDecodeWidth).map(i => enqPtrVec(PopCount(io.enq.needAlloc.take(i)))))
  io.enq.canAccept := allowEnqueue && !isReplaying
  val canEnqueue = VecInit(io.enq.req.map(_.valid && io.enq.canAccept))

  WqData.io.wen := canEnqueue
  WqData.io.waddr := allocatePtrVec.map(_.value)
  WqData.io.wdata.zip(io.enq.req.map(_.bits)).foreach { case (wdata, req) =>
    wdata := req.MicroOp
  }

  for (i <- 0 until VIDecodeWidth) {
    when(io.enq.req(i).valid) {
      val stateidx = allocatePtrVec(i).value
      WqStateAraay(stateidx).robenqEn := false.B
      WqStateAraay(stateidx).mergeidEn := false.B
      WqStateAraay(stateidx).vtypeEn := Mux(io.enq.req(i).bits.state === 1.U, true.B, false.B)
      WqStateAraay(stateidx).robIdx := io.enq.req(i).bits.MicroOp.robIdx
      WqStateAraay(stateidx).vtypeInfo := io.enq.req(i).bits.MicroOp.vCsrInfo
    }
  }


  val ReadAddr_next = VecInit(deqPtr_next.value)

  WqData.io.raddr := ReadAddr_next
  val WqDataRead = WqData.io.rdata.head

  /**
    * instruction split (Dequeue)
    */

  val prestartelement = io.vstart
  val vstartInterrupt = RegNext(Mux(io.vstart === 0.U, false.B, true.B))
  val currentdata = WqDataRead
  val currentstate = WqStateAraay(deqPtr_next.value)
  val deqUop = WireInit(VecInit(Seq.fill(VIRenameWidth)(0.U.asTypeOf(new MicroOp))))
  val isLS = Mux(currentdata.ctrl.isVLS, true.B, false.B)
  val isWiden = Mux(currentdata.ctrl.widen === Widen.NotWiden, false.B, true.B)
  val countnum = RegInit(VecInit.tabulate(VIDecodeWidth)(_.U))
  val lmul = currentstate.vtypeInfo.LmulToInt()
  val vl = currentstate.vtypeInfo.vl
  val sew = currentstate.vtypeInfo.SewToInt()
  val nf = currentdata.ctrl.NFToInt()
  val elementInRegGroup = VLEN >> (sew + 3)
  val elementTotal = lmul * elementInRegGroup
  //val elementWidth = currentdata.MicroOp.vCsrInfo.SewToInt()
  val splitnum = Mux(isLS, elementTotal.U, Mux(isWiden, (lmul << 1).U, lmul.U))
  val tailreg = vl / elementInRegGroup.U
//  val tailidx = vl % elementInRegGroup.U
  val prestartreg = prestartelement / elementInRegGroup.U
//  val prestartIdx = prestartelement % elementInRegGroup.U
  val globalcanSplit = Mux(vstartInterrupt, false.B, WqStateAraay(deqPtr_next.value).vtypeEn && WqStateAraay(deqPtr_next.value).robenqEn && WqStateAraay(deqPtr_next.value).mergeidEn)
  when(globalcanSplit && !isReplaying) {
    for (i <- 0 until VIRenameWidth) {
      val cansplit = Mux(countnum(i) < splitnum, true.B, false.B)
      when(cansplit) {
        deqUop(i) <> currentdata
        deqUop(i).isTail := Mux(countnum(i) === tailreg, true.B, false.B)
        deqUop(i).isPrestart := Mux(vstartInterrupt && (prestartreg === countnum(i)), true.B, false.B)
        deqUop(i).uopIdx := countnum(i)
        deqUop(i).uopNum := splitnum
        deqUop(i).canRename := Mux(deqUop(i).ctrl.isSeg, Mux(countnum(i) > nf.U, false.B, true.B), Mux(isLS, Mux(countnum(i) % elementInRegGroup.U === 0.U, true.B, false.B), true.B))
        val tempnum = Mux(deqUop(i).ctrl.isSeg, countnum(i) % nf.U, Mux(isLS, countnum(i) % elementInRegGroup.U, countnum(i)))
        deqUop(i).ctrl.lsrc(0) := currentdata.ctrl.lsrc(0) + tempnum
        deqUop(i).ctrl.lsrc(1) := currentdata.ctrl.lsrc(1) + tempnum
        deqUop(i).ctrl.lsrc(2) := currentdata.ctrl.lsrc(2) + tempnum
        isComplete := Mux(vstartInterrupt, true.B, Mux(!cansplit, false.B, true.B))
        countnum(i) := countnum(i) + 1.U
      }
      //      if (cansplit == true.B) {
      //        deqUop(i) <> currentdata
      //        deqUop(i).tailMask := Mux(countnum.U === tailreg, 0xffff.U >> ((elementInRegGroup.U - tailidx) * 16.U / elementInRegGroup.U),
      //          Mux(countnum.U > tailreg, 0.U, 0xffff.U))
      //        deqUop(i).preStartMask := Mux(vstartInterrupt && (prestartreg === countnum.U), 0xffff.U << prestartIdx, 0.U)
      //        deqUop(i).uopIdx := countnum.U
      //        deqUop(i).uopNum := splitnum
      //        if (deqUop(i).ctrl.isSeg == true.B) {
      //          val nf = currentdata.ctrl.NFToInt()
      //          val tempnum = countnum % nf
      //          deqUop(i).canRename := Mux(countnum.U > nf.U, false.B, true.B)
      //          deqUop(i).ctrl.lsrc(0) := currentdata.ctrl.lsrc(0) + tempnum.U
      //          deqUop(i).ctrl.lsrc(1) := currentdata.ctrl.lsrc(1) + tempnum.U
      //          deqUop(i).ctrl.lsrc(2) := currentdata.ctrl.lsrc(2) + tempnum.U
      //          deqUop(i).ctrl.ldest := currentdata.ctrl.ldest + tempnum.U
      //        } else if (isLS == true.B) {
      //          val tempnum = countnum % elementInRegGroup
      //          deqUop(i).canRename := Mux(tempnum.U === 0.U, true.B, false.B)
      //          deqUop(i).ctrl.lsrc(0) := currentdata.ctrl.lsrc(0) + tempnum.U
      //          deqUop(i).ctrl.lsrc(1) := currentdata.ctrl.lsrc(1) + tempnum.U
      //          deqUop(i).ctrl.lsrc(2) := currentdata.ctrl.lsrc(2) + tempnum.U
      //          deqUop(i).ctrl.ldest := currentdata.ctrl.ldest + tempnum.U
      //        } else {
      //          deqUop(i).canRename := true.B
      //          deqUop(i).ctrl.lsrc(0) := currentdata.ctrl.lsrc(0) + countnum.U
      //          deqUop(i).ctrl.lsrc(1) := currentdata.ctrl.lsrc(1) + countnum.U
      //          deqUop(i).ctrl.lsrc(2) := currentdata.ctrl.lsrc(2) + countnum.U
      //          deqUop(i).ctrl.ldest := currentdata.ctrl.ldest + countnum.U
      //        }
      //        countnum = countnum + 1
      //      } else {
      //        deqUop(i) := DontCare
      //        countnum = 0
      //        if (vstartInterrupt == true) {
      //          isComplete := false.B
      //          WqData.io.wen := isComplete
      //          WqData.io.waddr := deqPtr_next.value
      //          WqData.io.wdata := currentdata
      //        } else {
      //          isComplete := true.B
      //        }
      //      }
    }
  }

  //To VIRename
  for (i <- 0 until VIRenameWidth) {
    io.out(i).bits := deqUop(i)
    io.out(i).valid := !isReplaying
  }

  /**
    * vtype writeback
    */

  for (i <- 0 until VIDecodeWidth) {
    when(io.vtypeWbData(i).valid) {
      WqStateAraay.zipWithIndex.foreach({ case (o, idx) =>
        val tempdata = o
        tempdata.vtypeEn := true.B
        tempdata.vtypeInfo.vlmul := io.vtypeWbData(i).bits.data(2, 0)
        tempdata.vtypeInfo.vsew := io.vtypeWbData(i).bits.data(5, 3)
        o := Mux(o.vtypeRegIdx === io.vtypeWbData(i).bits.uop.vtypeRegIdx, tempdata, o)
      })
    }
  }

  /**
    * robenq update
    */

  for (i <- 0 until VIDecodeWidth) {
    when(io.robin(i).valid) {
      WqStateAraay.zipWithIndex.foreach({ case (o, idx) =>
        o.robenqEn := Mux(o.robIdx === io.vtypeWbData(i).bits.uop.robIdx, true.B, false.B)
      })
    }
  }

  /**
    * mergeid allocate
    */

  for (i <- 0 until VIDecodeWidth) {
    when(io.MergeId(i).valid && WqStateAraay(mergePtr.value).mergeidEn === false.B) {
      WqStateAraay(mergePtr.value).mergeidEn := true.B
      WqStateAraay(mergePtr.value).mergeIdx := io.MergeId(i).bits
      mergePtr := mergePtr + 1.U
    }
    io.MergeId(i).ready := !((mergePtr + 1.U) === enqPtr)
  }

  /**
    * update pointers
    */

//  val ren = PopCount(canEnqueue).orR
//  when(ren){
//    WqData.io.raddr := waitPtr.value
//    val tempdata_wait = WqData.io.rdata(0)
//    waitPtr := Mux(tempdata_wait.state === s_valid, waitPtr + 1.U, waitPtr)
//
//    WqData.io.raddr := vtypePtr.value
//    val tempdata_vtype = WqData.io.rdata(0)
//    vtypePtr := Mux(tempdata_vtype.state === s_vtypewb, vtypePtr, vtypePtr + 1.U)
//  }

}