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
import xiangshan.vector._
import xiangshan.backend.rob._
import xs.utils._

class VtypeInfo(implicit p: Parameters) extends CfCtrl{
  val robIdx = new RobPtr
  val vtypeIdx = new VtypePtr
  val ESEW = UInt(3.W)
  val ELMUL = UInt(3.W)
  val state = 0.U(2.W)
}

class VtypeDelayData(implicit  p: Parameters) extends VectorBaseBundle {
  val vtypeIdx = new VtypePtr
  val ESEW = UInt(3.W)
  val ELMUL = UInt(3.W)
  val state = 0.U(2.W)
}

class VtypeReg(implicit p: Parameters) extends MicroOp{
  val vma = UInt(1.W)
  val vta = UInt(1.W)
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
  val state = 0.U(2.W)
  val vtypeIdx = new VtypePtr
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

class VtypeRename(size: Int, enqnum: Int, deqnum: Int, numWbPorts: Int)(implicit p: Parameters) extends VectorBaseModule with HasPerfEvents with HasCircularQueuePtrHelper {

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val robCommits = Flipped(new RobCommitIO)
    val canAllocate = Output(Bool())
    val doAllocate = Input(Bool())
    val in = Vec(enqnum, Flipped(DecoupledIO(new MicroOp)))
    val out = Vec(enqnum, ValidIO(new VtypeInfo))
    val WbData = Vec(numWbPorts, DecoupledIO(new VtypeDelayData))
    val deq = Vec(VICommitWidth, DecoupledIO(new MicroOp))
    val writeback = Vec(numWbPorts, Flipped(ValidIO(new ExuOutput)))
  })

  val VtypeRegTable = RegInit(VecInit(Seq.tabulate(size - 1)(i => new VtypeReg)))

  class VtypePtr extends CircularQueuePtr[VtypePtr](size)

  val doRename = io.canAllocate && io.doAllocate && !io.redirect.valid

  // head: first valid entry
  val headPtr = RegInit(VtypePtr(false, 0))

  // tail: first invalid entry (free entry)
  val tailPtr = RegInit(VtypePtr(false, size - 1))

  val s_invalid :: s_valid :: s_busy :: Nil = Enum(3)


  /*
    enqueue logic and rename out logic
   */

  for (i <- 0 until enqnum) {
    when(io.in(i).valid) {
      val tempvtype = VtypeRegTable(tailPtr.value)
      io.out(i).bits.cf := tempvtype.cf
      io.out(i).bits.ctrl := tempvtype.ctrl
      io.out(i).bits.robIdx := tempvtype.robIdx
      io.out(i).bits.state := tempvtype.state
      io.out(i).bits.ESEW := tempvtype.vsew
      io.out(i).bits.ELMUL := tempvtype.vlmul
      io.out(i).bits.vtypeIdx := tailPtr
      io.out(i).valid := true.B
      if (io.in(i).bits.ctrl.isVtype == 1) {
        val tempvtype = new VtypeReg
        val freePtr = tailPtr + 1.U
        tempvtype.robIdx := io.in(i).bits.robIdx
        tempvtype.cf := io.in(i).bits.cf
        tempvtype.ctrl := io.in(i).bits.ctrl
        tempvtype.vtypeIdx := freePtr
        if (io.in(i).bits.cf.instr(31) == 0) {
          tempvtype.vma := io.in(i).bits.cf.instr(30)
          tempvtype.vta := io.in(i).bits.cf.instr(29)
          tempvtype.vsew := io.in(i).bits.cf.instr(28, 26)
          tempvtype.vlmul := io.in(i).bits.cf.instr(25, 23)
          tempvtype.state := s_valid
        } else if (io.in(i).bits.cf.instr(31, 30) == 11) {
          tempvtype.vma := io.in(i).bits.cf.instr(29)
          tempvtype.vta := io.in(i).bits.cf.instr(28)
          tempvtype.vsew := io.in(i).bits.cf.instr(27, 25)
          tempvtype.vlmul := io.in(i).bits.cf.instr(24, 22)
          tempvtype.state := s_valid
        } else {
          tempvtype.state := s_busy
        }
        VtypeRegTable(freePtr.value) := tempvtype
        tailPtr := Mux(doRename, freePtr, tailPtr)
      }
    }
  }

  /*
    dequeue logic when commit
   */
  for (i <- 0 until CommitWidth) {
    val tempvtype = VtypeRegTable(headPtr.value)
    when(io.robCommits.isCommit) {
      if (tempvtype.cf.ftqPtr == io.robCommits.info(i).ftqIdx && tempvtype.cf.ftqOffset == io.robCommits.info(i).ftqOffset) {
        VtypeRegTable(headPtr.value).state := s_invalid
        val headNextPtr = headPtr + 1.U
        headPtr := Mux(io.redirect.valid, headPtr, headNextPtr)
      }
    }
  }

  /*
    caculate the free entry
  */

  val freeRegCnt = distanceBetween(tailPtr, headPtr)
  val freeRegCntReg = RegNext(freeRegCnt)
  io.canAllocate := freeRegCntReg >= enqnum.U

  /*
    update point content  s_busy -> s_valid
   */
 for (i <- 0 until numWbPorts) {
   when(io.writeback(i).valid) {
     for ((v,w) <- VtypeRegTable.zip(io.writeback)) {
       if (v.robIdx == w.bits.uop.robIdx) {
         v.state := s_valid
         v.vma := w.bits.data(31)
         v.vta := w.bits.data(30)
         v.vsew := w.bits.data(29, 27)
         v.vlmul := w.bits.data(26, 24)
         io.backward(i).bits.cf := v.cf
         io.backward(i).bits.ctrl := v.ctrl
         io.backward(i).bits.robIdx := v.robIdx
         io.backward(i).bits.state := v.state
         io.backward(i).bits.ESEW := v.vsew
         io.backward(i).bits.ELMUL := v.vlmul
         io.backward(i).bits.vtypeIdx := v.vtypeIdx
         io.backward(i).valid := true.B
       }
     }
   }
 }

  val perfEvents = Seq(
    ("dispatchq_out", PopCount(io.deq.map(_.fire))),
    ("dispatchq_out_try", PopCount(io.deq.map(_.valid)))
  )
  generatePerfEvent()
}
