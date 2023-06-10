/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package xiangshan.vector.vtyperename

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.rob._
import xs.utils._

class VtypeInfo(implicit p: Parameters) extends CfCtrl{
  val s_invalid :: s_valid :: s_busy :: Nil = Enum(2)
  val robIdx = new RobPtr
  val ESEW = UInt(3.W)
  val ELMUL = UInt(3.W)
  val state = RegInit(s_invalid)
}

class VtypeReg(implicit p: Parameters) extends MicroOp{
  val s_invalid :: s_valid :: s_busy :: Nil = Enum(2)
  val vma = UInt(1.W)
  val vta = UInt(1.W)
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
  val state = RegInit(s_invalid)
}

class VtypePtr(implicit p: Parameters) extends CircularQueuePtr[VtypePtr](
  p => p(XSCoreParamsKey).RobSize
) with HasCircularQueuePtrHelper {

  def needFlush(redirect: Valid[Redirect]): Bool = {
    val flushItself = redirect.bits.flushItself() && this === redirect.bits.robIdx
    redirect.valid && (flushItself || isAfter(this, redirect.bits.robIdx))
  }

  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR
}

object VtypePtr {
  def apply(f: Boolean, v: Int)(implicit p: Parameters): VtypePtr = {
    val ptr = Wire(new VtypePtr)
    ptr.flag := f.B
    ptr.value := v.U
    ptr
  }
}

class VtypeRename(size: Int, enqnum: Int, deqnum: Int)(implicit p: Parameters) extends XSModule with HasPerfEvents with HasCircularQueuePtrHelper {

  val io = IO(new Bundle() {

    val redirect = Flipped(ValidIO(new Redirect))
    val robCommits = Flipped(new RobCommitIO)
    val canAllocate = Vec(enqnum, Output(Bool()))
    val doAllocate = Vec(enqnum, Input(Bool()))
    val in = Vec(enqnum, Flipped(DecoupledIO(new MicroOp)))
    val out = Vec(enqnum, DecoupledIO(new VtypeInfo))
    val deq = Vec(CommitWidth, DecoupledIO(new MicroOp))
    val dqFull = Output(Bool())
    val deqNext = Vec(deqnum, Output(new MicroOp))

  })

  val VtypeRegTable = RegInit(VecInit(Seq.tabulate(size-1)(i => new VtypeReg)))
  class VtypePtr extends CircularQueuePtr[VtypePtr](size)

  // head: first valid entry
  val headPtr = RegInit(VtypePtr(false,0))
  val headPtrNext = Wire(Vec(2 * deqnum, new VtypePtr))
  val headPtrOH = RegInit(1.U(size.W))
  val headPtrOHShift = CircularShift(headPtrOH)
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(VtypePtr(false,size-1))
  val tailPtrOH = RegInit(1.U(size.W))
  val tailPtrOHShift = CircularShift(tailPtrOH)
  val tailPtrOHVec = VecInit.tabulate(enqnum + 1)(tailPtrOHShift.left)

  val s_invalid :: s_valid :: s_busy :: Nil = Enum(2)

  val numAllocate = PopCount(io.doAllocate)
  val headPtrNext = headPtr + numAllocate


  for (i <- 0 until enqnum) {
    when(io.in(i).valid) {
      val tempvtype = VtypeRegTable(tailPtr.value)
      io.out(i).bits.cf := tempvtype.cf
      io.out(i).bits.ctrl := tempvtype.ctrl
      io.out(i).bits.robIdx := tempvtype.robIdx
      io.out(i).bits.state := tempvtype.state
      io.out(i).bits.cf := tempvtype.cf
      io.out(i).valid := true.B
      if (io.in(i).bits.ctrl.isVtype === 1) {
        val tempvtype = new VtypeReg
        tempvtype.robIdx := io.in(i).bits.robIdx
        tempvtype.cf := io.in(i).bits.cf
        tempvtype.ctrl := io.in(i).bits.ctrl
        if (io.in(i).bits.cf.instr(31) === 0) {
          tempvtype.vma := io.in(i).bits.cf.instr(30)
          tempvtype.vta := io.in(i).bits.cf.instr(29)
          tempvtype.vsew := io.in(i).bits.cf.instr(28, 26)
          tempvtype.vlmul := io.in(i).bits.cf.instr(25, 23)
          tempvtype.state := s_valid
        } else if (io.in(i).bits.cf.instr(31,30) === 11) {
          tempvtype.vma := io.in(i).bits.cf.instr(29)
          tempvtype.vta := io.in(i).bits.cf.instr(28)
          tempvtype.vsew := io.in(i).bits.cf.instr(27, 25)
          tempvtype.vlmul := io.in(i).bits.cf.instr(24, 22)
          tempvtype.state := s_valid
        } else {
          tempvtype.state := s_busy
        }
        val freePtr = tailPtr + 1.U
        VtypeRegTable(freePtr.value) := tempvtype
      }
    }
  }


















































  /**
    * Part 1: update states and uops when enqueue, dequeue, commit, redirect/replay
    *
    * uop only changes when a new instruction enqueues.
    *
    * state changes when
    * (1) enqueue: from s_invalid to s_valid or s_busy
    * (2) dequeue: from s_valid to s_invalid
    * (3) writeback: from s_busy to s_valid
    * (4) commit: from s_valid to s_invalid
    * (5) redirect (branch misprediction or exception): from any state to s_invalid (flushed)
    */

  val s_invalid :: s_valid :: s_busy :: Nil = Enum(2)

  // queue data array
  //val dataModule = Module(new SyncDataModuleTemplate(new VtypeReg, size, 2 * deqnum, enqnum, "DispatchQueue"))
  val vtypeRegList = RegInit(VecInit(
    // originally {1, 2, ..., size - 1} are free. Register 0-31 are mapped to x0.
    Seq.tabulate(size - 1)(i => (i + 1).U(PhyRegIdxWidth.W)) :+ 0.U(PhyRegIdxWidth.W)))
  val robIdxEntries = Reg(Vec(size, new RobPtr))
  val stateEntries = RegInit(VecInit(Seq.fill(size)(s_invalid)))

  class VtypePtr extends CircularQueuePtr[VtypePtr](size)

  // head: first valid entry
  val headPtr = RegInit(VecInit((0 until 2 * deqnum).map(_.U.asTypeOf(new VtypePtr))))
  val headPtrNext = Wire(Vec(2 * deqnum, new VtypePtr))
  val headPtrMask = UIntToMask(headPtr(0).value, size)
  val headPtrOH = RegInit(1.U(size.W))
  val headPtrOHShift = CircularShift(headPtrOH)
  val headPtrOHVec = VecInit.tabulate(deqnum + 1)(headPtrOHShift.left)
  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(VecInit((0 until enqnum).map(_.U.asTypeOf(new VtypePtr))))
  val tailPtrMask = UIntToMask(tailPtr(0).value, size)
  val tailPtrOH = RegInit(1.U(size.W))
  val tailPtrOHShift = CircularShift(tailPtrOH)
  val tailPtrOHVec = VecInit.tabulate(enqnum + 1)(tailPtrOHShift.left)
  // valid entries counter
  val validCounter = RegInit(0.U(log2Ceil(size + 1).W))
  val allowEnqueue = RegInit(true.B)

  val isTrueEmpty = !VecInit(stateEntries.map(_ === s_valid)).asUInt.orR
  val canEnqueue = allowEnqueue


  val enqOffset = (0 until enqnum).map(i => PopCount(io.enq.needAlloc.take(i)))
  val enqIndexOH = (0 until enqnum).map(i => tailPtrOHVec(enqOffset(i)))
  for (i <- 0 until size) {
    var enqValid = false.B
    if(io.enq.req(i).bits.ctrl.isVtype === true) {
      enqValid = true.B
    }
    val validVec = io.enq.req.map(_.valid).zip(enqIndexOH).map { case (v, oh) => v && oh(i) }
    when(VecInit(validVec).asUInt.orR && canEnqueue && enqValid) {
      robIdxEntries(i) := Mux1H(validVec, io.enq.req.map(_.bits.robIdx))
      stateEntries(i) := s_valid
    }
  }

  val vtypedata = Vec(enqnum, new VtypeReg)

  for(i <- 0 until enqnum) {
    if(io.enq.req(i).bits.ctrl.isVtype == 1) {
      if (io.enq.req(i).bits.cf.instr(31) == 0) {
        vtypedata(i).robIdx := io.enq.req(i).bits.robIdx
        vtypedata(i).cf := io.enq.req(i).bits.cf
        vtypedata(i).ctrl := io.enq.req(i).bits.ctrl
        vtypedata(i).vma := io.enq.req(i).bits.cf.instr(30)
        vtypedata(i).vta := io.enq.req(i).bits.cf.instr(29)
        vtypedata(i).vsew := io.enq.req(i).bits.cf.instr(28,26)
        vtypedata(i).vlmul := io.enq.req(i).bits.cf.instr(25,23)
      } else if (io.enq.req(i).bits.cf.instr(31, 30) == 11) {
        vtypedata(i).robIdx := io.enq.req(i).bits.robIdx
        vtypedata(i).cf := io.enq.req(i).bits.cf
        vtypedata(i).ctrl := io.enq.req(i).bits.ctrl
        vtypedata(i).vma := io.enq.req(i).bits.cf.instr(29)
        vtypedata(i).vta := io.enq.req(i).bits.cf.instr(28)
        vtypedata(i).vsew := io.enq.req(i).bits.cf.instr(27, 25)
        vtypedata(i).vlmul := io.enq.req(i).bits.cf.instr(24, 22)
      } else {
        vtypedata(i).robIdx := io.enq.req(i).bits.robIdx
        vtypedata(i).cf := io.enq.req(i).bits.cf
        vtypedata(i).ctrl := io.enq.req(i).bits.ctrl
      }
    }
  }

  for (i <- 0 until enqnum) {
    dataModule.io.wen(i) := canEnqueue && io.enq.req(i).valid
    dataModule.io.waddr(i) := tailPtr(enqOffset(i)).value
    dataModule.io.wdata(i) := vtypedata(i)
  }

  // dequeue: from s_valid to s_dispatched
  for (i <- 0 until size) {
    val validVec = io.deq.map(_.fire).zip(headPtrOHVec).map { case (v, oh) => v && oh(i) }
    when(VecInit(validVec).asUInt.orR && !io.redirect.valid) {
      stateEntries(i) := s_invalid
    }
  }

  /**
    * Part 2: update indices
    *
    * tail: (1) enqueue; (2) redirect
    * head: dequeue
    */

  // dequeue
  val currentValidCounter = distanceBetween(tailPtr(0), headPtr(0))
  val numDeqTryMask = Mux(currentValidCounter >= deqnum.U,
    // all deq are valid
    (1 << deqnum).U,
    // only the valid bits are set
    UIntToOH(currentValidCounter, deqnum)
  )
  val deqEnable_n = io.deq.zipWithIndex.map { case (deq, i) =>
    // For dequeue, the first entry should never be s_invalid
    // Otherwise, there should be a redirect and tail walks back
    // in this case, we set numDeq to 0
    if (i == 0) !deq.fire || numDeqTryMask(i)
    // When the state is s_invalid, we set deqEnable_n to false.B because
    // the entry may leave earlier and require to move forward the deqPtr.
    else (!deq.fire && stateEntries(headPtr(i).value) =/= s_invalid) || numDeqTryMask(i)
  } :+ true.B
  val numDeq = PriorityEncoder(deqEnable_n)
  // agreement with reservation station: don't dequeue when redirect.valid
  for (i <- 0 until 2 * deqnum) {
    headPtrNext(i) := Mux(io.redirect.valid, headPtr(i), headPtr(i) + numDeq)
  }
  headPtr := headPtrNext
  headPtrOH := Mux(io.redirect.valid, headPtrOH, ParallelPriorityMux(deqEnable_n, headPtrOHVec))
  XSError(headPtrOH =/= headPtr.head.toOH, p"head: $headPtrOH != UIntToOH(${headPtr.head})")

  // For branch mis-prediction or memory violation replay,
  // we delay updating the indices for one clock cycle.
  // For now, we simply use PopCount to count #instr cancelled.
  val lastCycleMisprediction = RegNext(io.redirect.valid)
  // find the last one's position, starting from headPtr and searching backwards
  val validBitVec = VecInit((0 until size).map(i => stateEntries(i) === s_valid))
  val loValidBitVec = Cat((0 until size).map(i => validBitVec(i) && headPtrMask(i)))
  val hiValidBitVec = Cat((0 until size).map(i => validBitVec(i) && !headPtrMask(i)))
  val flippedFlag = loValidBitVec.orR || validBitVec(size - 1)
  val leadingZeros = PriorityEncoder(Mux(loValidBitVec.orR, loValidBitVec, hiValidBitVec))
  val lastOneIndex = Mux(leadingZeros === 0.U, 0.U, size.U - leadingZeros)
  val walkedTailPtr = Wire(new VtypePtr)
  walkedTailPtr.flag := flippedFlag ^ headPtr(0).flag
  walkedTailPtr.value := lastOneIndex

  // enqueue
  val numEnq = Mux(io.enq.canAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  tailPtr(0) := Mux(io.redirect.valid,
    tailPtr(0),
    Mux(lastCycleMisprediction,
      Mux(isTrueEmpty, headPtr(0), walkedTailPtr),
      tailPtr(0) + numEnq)
  )
  val lastLastCycleMisprediction = RegNext(lastCycleMisprediction)
  for (i <- 1 until enqnum) {
    tailPtr(i) := Mux(io.redirect.valid,
      tailPtr(i),
      Mux(lastLastCycleMisprediction,
        tailPtr(0) + i.U,
        tailPtr(i) + numEnq)
    )
  }
  tailPtrOH := Mux(lastLastCycleMisprediction, tailPtr.head.toOH, tailPtrOHVec(numEnq))
  val tailPtrOHAccurate = !lastCycleMisprediction && !lastLastCycleMisprediction
  XSError(tailPtrOHAccurate && tailPtrOH =/= tailPtr.head.toOH, p"tail: $tailPtrOH != UIntToOH(${tailPtr.head})")

  // update valid counter and allowEnqueue reg
  validCounter := Mux(io.redirect.valid,
    validCounter,
    Mux(lastLastCycleMisprediction,
      currentValidCounter,
      validCounter + numEnq - numDeq)
  )
  allowEnqueue := Mux(currentValidCounter > (size - enqnum).U, false.B, numEnq <= (size - enqnum).U - currentValidCounter)

  /**
    * Part 3: set output valid and data bits
    */
  val deqData = Reg(Vec(deqnum, new VtypeInfo))
  // How to pipeline the data read:
  // T: get the required read data
  for (i <- 0 until deqnum) {
    io.deq(i).bits := deqData(i)
    // Some bits have bad timing in Dispatch but will not be used at Dispatch2
    // They will use the slow path from data module
    io.deq(i).bits.cf := DontCare
    io.deq(i).bits.ctrl.fpu := DontCare
    // do not dequeue when io.redirect valid because it may cause dispatchPtr work improperly
    io.deq(i).valid := Mux1H(headPtrOHVec(i), stateEntries) === s_valid && !lastCycleMisprediction
  }
  // T-1: select data from the following (deqnum + 1 + numEnq) sources with priority
  // For data(i): (1) current output (deqnum - i); (2) next-step data (i + 1)
  // For the next-step data(i): (1) enqueue data (enqnum); (2) data from storage (1)
  val nextStepData = Wire(Vec(2 * deqnum, new MicroOp))
  val ptrMatch = new QPtrMatchMatrix(headPtr, tailPtr)
  for (i <- 0 until 2 * deqnum) {
    val enqMatchVec = VecInit(ptrMatch(i))
    val enqBypassEnVec = io.enq.needAlloc.zip(enqOffset).map { case (v, o) => v && enqMatchVec(o) }
    val enqBypassEn = io.enq.canAccept && VecInit(enqBypassEnVec).asUInt.orR
    val enqBypassData = Mux1H(enqBypassEnVec, io.enq.req.map(_.bits))
    val readData = if (i < deqnum) deqData(i) else dataModule.io.rdata(i)
    nextStepData(i) := Mux(enqBypassEn, enqBypassData, readData)
  }
  for (i <- 0 until deqnum) {
    io.deqNext(i) := deqData(i)
    when(!io.redirect.valid) {
      io.deqNext(i) := ParallelPriorityMux(deqEnable_n, nextStepData.drop(i).take(deqnum + 1))
    }
  }
  deqData := io.deqNext
  // T-2: read data from storage: next
  dataModule.io.raddr := headPtrNext.map(_.value)

  val perfEvents = Seq(
    ("dispatchq_in", numEnq),
    ("dispatchq_out", PopCount(io.deq.map(_.fire))),
    ("dispatchq_out_try", PopCount(io.deq.map(_.valid)))
  )
  generatePerfEvent()
}
