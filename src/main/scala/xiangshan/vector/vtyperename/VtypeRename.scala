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

class VtypeReg(implicit p: Parameters) extends VectorBaseBundle {
  val uop = new MicroOp
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

  //TODO:
  //val VtypeRegTable = RegInit(VecInit(Seq.tabulate(size - 1)(i => new VtypeReg)))
  val VtypeRegTable = RegInit(VecInit(Seq.fill(VIWaitQueueWidth)(0.U.asTypeOf(new VtypeReg))))

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
      //TODO: 
      io.out(i).bits <> tempVtype
      val CurrentVL = tempVtype.uop.vCsrInfo.vl
      val CurrentVLMAX = tempVtype.uop.vCsrInfo.VLMAXGen()
      io.out(i).valid := io.canAllocate
      val tempvtype = WireInit((0.U.asTypeOf(new VtypeReg)))
      val freePtr = tailPtr + 1.U
      //TODO:
      tempvtype.uop := io.in(i).bits
      tempvtype.vtypeIdx := freePtr
      tempvtype.uop.vCsrInfo.oldvl := CurrentVL
      tempvtype.uop.vCsrInfo.vma := Mux(io.in(i).bits.cf.instr(31) === 0.U, io.in(i).bits.cf.instr(30), Mux(io.in(i).bits.cf.instr(31, 30) === 11.U, io.in(i).bits.cf.instr(29), 0.U))
      tempvtype.uop.vCsrInfo.vta := Mux(io.in(i).bits.cf.instr(31) === 0.U, io.in(i).bits.cf.instr(29), Mux(io.in(i).bits.cf.instr(31, 30) === 11.U, io.in(i).bits.cf.instr(28), 0.U))
      tempvtype.uop.vCsrInfo.vsew := Mux(io.in(i).bits.cf.instr(31) === 0.U, io.in(i).bits.cf.instr(28, 26), Mux(io.in(i).bits.cf.instr(31, 30) === 11.U, io.in(i).bits.cf.instr(27, 25), 0.U))
      tempvtype.uop.vCsrInfo.vlmul := Mux(io.in(i).bits.cf.instr(31) === 0.U, io.in(i).bits.cf.instr(25, 23), Mux(io.in(i).bits.cf.instr(31, 30) === 11.U, io.in(i).bits.cf.instr(24, 22), 0.U))
      tempvtype.uop.vCsrInfo.vlmax := Mux(io.in(i).bits.cf.instr(31) === 0.U, tempvtype.uop.vCsrInfo.VLMAXGen().U, Mux(io.in(i).bits.cf.instr(31, 30) === 11.U, tempvtype.uop.vCsrInfo.VLMAXGen().U, 0.U))
      //TODO:---
//      println(s"vtype index:$i")
      tempvtype.uop.vCsrInfo.vl := Mux(io.in(i).bits.cf.instr(31) === 0.U,
        Mux(io.in(i).bits.ctrl.lsrc(0) === 0.U, Mux(io.in(i).bits.ctrl.lsrc(2) === 0.U, CurrentVL, CurrentVLMAX.U), 0.U),
        Mux(io.in(i).bits.cf.instr(31, 30) === 11.U, io.in(i).bits.cf.instr(19, 15), 0.U))
//      tempvtype.uop.vCsrInfo.vl := 0.U
      tempvtype.state := Mux(io.in(i).bits.cf.instr(31) === 0.U,
        Mux(io.in(i).bits.ctrl.lsrc(0) === 0.U, s_valid, s_busy),
        Mux(io.in(i).bits.cf.instr(31, 30) === 11.U, s_valid, s_busy))
      //---------
      //        if (io.in(i).bits.cf.instr(31) == 0) {
      //          tempvtype.vCsrInfo.vma := io.in(i).bits.cf.instr(30)
      //          tempvtype.vCsrInfo.vta := io.in(i).bits.cf.instr(29)
      //          tempvtype.vCsrInfo.vsew := io.in(i).bits.cf.instr(28, 26)
      //          tempvtype.vCsrInfo.vlmul := io.in(i).bits.cf.instr(25, 23)
      //          tempvtype.vCsrInfo.vlmax := tempvtype.vCsrInfo.VLMAXGen().U
      //          if (io.in(i).bits.ctrl.lsrc(0) != 0.U) {
      //            tempvtype.state := s_busy
      //          } else if (io.in(i).bits.ctrl.lsrc(0) == 0.U && io.in(i).bits.ctrl.lsrc(3) != 0) {
      //            tempvtype.state := s_valid
      //            tempvtype.vCsrInfo.vl := CurrentVLMAX.U
      //          } else {
      //            tempvtype.state := s_valid
      //            tempvtype.vCsrInfo.vl := CurrentVL
      //          }
      //        } else if (io.in(i).bits.cf.instr(31, 30) == 11) {
      //          tempvtype.vCsrInfo.vma := io.in(i).bits.cf.instr(29)
      //          tempvtype.vCsrInfo.vta := io.in(i).bits.cf.instr(28)
      //          tempvtype.vCsrInfo.vsew := io.in(i).bits.cf.instr(27, 25)
      //          tempvtype.vCsrInfo.vlmul := io.in(i).bits.cf.instr(24, 22)
      //          tempvtype.vCsrInfo.vl := io.in(i).bits.cf.instr(19, 15)
      //          tempvtype.vCsrInfo.vlmax := tempvtype.vCsrInfo.VLMAXGen().U
      //          tempvtype.state := s_valid
      //        } else {
      //          tempvtype.state := s_busy
      //        }
      VtypeRegTable(freePtr.value) := tempvtype
      tailPtr := Mux(doRename, freePtr, tailPtr)

    }
  }

  /*
    dequeue logic when commit
   */
  for (i <- 0 until CommitWidth) {
    val tempvtype = VtypeRegTable(headPtr.value)
    val selectDeqEntry = tempvtype.uop.cf.ftqPtr === io.robCommits.info(i).ftqIdx && tempvtype.uop.cf.ftqOffset === io.robCommits.info(i).ftqOffset
    when(io.robCommits.isCommit && selectDeqEntry) {
      VtypeRegTable(headPtr.value).state := s_invalid
      val headNextPtr = headPtr + 1.U
      headPtr := Mux(io.redirect.valid, headPtr, headNextPtr)
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
      for ((v, w) <- VtypeRegTable.zip(io.writeback)) {
        val selectEntry = v.uop.robIdx === w.bits.uop.robIdx && v.uop.vtypeRegIdx === w.bits.uop.vtypeRegIdx
        v.state := Mux(selectEntry, s_valid, v.state)
        v.uop.vCsrInfo.vma := Mux(selectEntry, w.bits.data(7), 0.U)
        v.uop.vCsrInfo.vta := Mux(selectEntry, w.bits.data(6), 0.U)
        v.uop.vCsrInfo.vsew := Mux(selectEntry, w.bits.data(5, 3), 0.U)
        v.uop.vCsrInfo.vlmul := Mux(selectEntry, w.bits.data(2, 0), 0.U)
        v.uop.vCsrInfo.vl := Mux(selectEntry, w.bits.data(15, 8), 0.U)
        v.uop.vCsrInfo.vlmax := v.uop.vCsrInfo.VLMAXGen().U
      }
    }
  }

  val perfEvents = Seq(
    ("dispatchq_out", PopCount(io.deq.map(_.fire))),
    ("dispatchq_out_try", PopCount(io.deq.map(_.valid)))
  )
  generatePerfEvent()
}
