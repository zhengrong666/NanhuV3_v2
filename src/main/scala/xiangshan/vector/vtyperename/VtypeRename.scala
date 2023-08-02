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

//class VtypeInfo(implicit p: Parameters) extends CfCtrl{
//  val robIdx = new RobPtr
//  val VMA = UInt(1.W)
//  val VTA = UInt(1.W)
//  val ESEW = UInt(3.W)
//  val ELMUL = UInt(3.W)
//  val VL = UInt(8.W)
//  val state = 0.U(2.W)
//}

class VtypeReg(implicit p: Parameters) extends MicroOp{
  val state = UInt(2.W)
  val vtypeIdx = new VtypePtr
}

class VtypePtr(implicit p: Parameters) extends CircularQueuePtr[VtypePtr](
  p => p(XSCoreParamsKey).RobSize
) with HasCircularQueuePtrHelper {

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
    val in = Vec(enqnum, Flipped(ValidIO(new MicroOp)))
    val out = Vec(enqnum, ValidIO(new VtypeReg))
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
      val tempVtype = VtypeRegTable(tailPtr.value)
      io.out(i).bits <> tempVtype.cf
      val CurrentVL = tempVtype.vCsrInfo.vl
      val CurrentVLMAX = tempVtype.vCsrInfo.VLMAXGen()
      io.out(i).valid := io.canAllocate
      if (io.in(i).bits.ctrl.isVtype == 1) {
        val tempvtype = new VtypeReg
        val freePtr = tailPtr + 1.U
        tempvtype <> io.in(i).bits
        tempvtype.vtypeIdx := freePtr
        tempvtype.vCsrInfo.oldvl := CurrentVL
        if (io.in(i).bits.cf.instr(31) == 0) {
          tempvtype.vCsrInfo.vma := io.in(i).bits.cf.instr(30)
          tempvtype.vCsrInfo.vta := io.in(i).bits.cf.instr(29)
          tempvtype.vCsrInfo.vsew := io.in(i).bits.cf.instr(28, 26)
          tempvtype.vCsrInfo.vlmul := io.in(i).bits.cf.instr(25, 23)
          tempvtype.vCsrInfo.vlmax := tempvtype.vCsrInfo.VLMAXGen().U
          if (io.in(i).bits.ctrl.lsrc(0) != 0.U) {
            tempvtype.state := s_busy
          } else if (io.in(i).bits.ctrl.lsrc(0) != 0.U && io.in(i).bits.ctrl.lsrc(3) != 0) {
            tempvtype.state := s_valid
            tempvtype.vCsrInfo.vl := CurrentVLMAX.U
          } else {
            tempvtype.state := s_valid
            tempvtype.vCsrInfo.vl := CurrentVL
          }
        } else if (io.in(i).bits.cf.instr(31, 30) == 11) {
          tempvtype.vCsrInfo.vma := io.in(i).bits.cf.instr(29)
          tempvtype.vCsrInfo.vta := io.in(i).bits.cf.instr(28)
          tempvtype.vCsrInfo.vsew := io.in(i).bits.cf.instr(27, 25)
          tempvtype.vCsrInfo.vlmul := io.in(i).bits.cf.instr(24, 22)
          tempvtype.vCsrInfo.vl := io.in(i).bits.cf.instr(19, 15)
          tempvtype.vCsrInfo.vlmax := tempvtype.vCsrInfo.VLMAXGen().U
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

  val vsetvlNum = PopCount(io.in.map(_.bits.ctrl.isVtype))
  val freeRegCnt = distanceBetween(tailPtr, headPtr)
  val freeRegCntReg = RegNext(freeRegCnt)
  io.canAllocate := freeRegCntReg >= vsetvlNum

  /*
    update point content  s_busy -> s_valid
   */
 for (i <- 0 until numWbPorts) {
   when(io.writeback(i).valid) {
     for ((v,w) <- VtypeRegTable.zip(io.writeback)) {
       if (v.robIdx == w.bits.uop.robIdx) {
         v.state := s_valid
         v.vCsrInfo.vma := w.bits.data(31)
         v.vCsrInfo.vta := w.bits.data(30)
         v.vCsrInfo.vsew := w.bits.data(29, 27)
         v.vCsrInfo.vlmul := w.bits.data(26, 24)
         v.vCsrInfo.vl := w.bits.data(19, 15)
         v.vCsrInfo.vlmax := v.vCsrInfo.VLMAXGen().U
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
