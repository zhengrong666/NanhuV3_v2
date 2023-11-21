/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

/*--------------------------------------------------------------------------------------
	Author: GMX
	Date: 2023-06-28
	email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.vector.virename

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

import xiangshan._
import xiangshan.vector._
import xiangshan.backend.rob.RobPtr

import xs.utils._

class VRobEntry(implicit p: Parameters) extends VectorBaseBundle {
  val robIdx = new RobPtr
  val logicRegIdx = UInt(5.W)
  val oldPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
  val newPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
}

class VRob(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {
  
  val size = VIPhyRegsNum

  class VRobPtr extends CircularQueuePtr[VRobPtr](VIPhyRegsNum)
  
  val io = IO(new Bundle {
    val enq = Vec(VIRenameWidth, Flipped(ValidIO(new VRobEntry)))
    val blockRename = Output(Bool())
    val commit = new Bundle {
      val rob = Flipped(new RobCommitIO)
      val rat = Output(new VIRatCommitPort)
    }
    val redirect = Flipped(ValidIO(new Redirect))
  })

  val enqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  val deqPtr = RegInit(0.U.asTypeOf(new VRobPtr))
  assert(enqPtr >= deqPtr)

  val entryNum = distanceBetween(enqPtr, deqPtr)

  val array = Reg(Vec(size, new VRobEntry))
  val array_v = RegInit(VecInit(Seq.fill(size)(false.B)))

  // enq
  val enqPtrVec = Wire(Vec(VIRenameWidth, UInt(log2Up(size).W)))
  enqPtrVec.zipWithIndex.foreach {
    case (ptr, i) => {
      ptr := (enqPtr + i.U).value
    }
  }

  enqPtrVec.zip(io.enq).foreach {
    case (ptr, enq) => {
      when(enq.valid) {
        array(ptr) := enq.bits
        array_v(ptr) := true.B
      }
    }
  }

  // deq
  val flushVec = Wire(Vec(size, Bool()))
  array.zip(array_v).zipWithIndex.foreach {
    case ((e, v), i) => {
      flushVec(i) := v && e.robIdx.needFlush(io.redirect)
    }
  }

  val walkNum = RegInit(0.U(log2Up(size + 1).W))
  

  // commit
  val commitPtrVec = Wire(Vec(8, UInt(log2Up(size).W)))
  val walkPtrVec = Wire(Vec(8, UInt(log2Up(size).W)))
  commitPtrVec.zip(walkPtrVec).zipWithIndex.foreach {
    case ((cptr, wptr), i) => {
      cptr := (deqPtr + i.U).value
      wptr := (enqPtr - i.U).value
    }
  }

  val commitValid = io.commit.rob.isCommit && io.commit.rob.commitValid.asUInt.orR
  val commitRobIdx = Mux1H(io.commit.rob.commitValid, io.commit.rob.robIdx)
  assert(PopCount(io.commit.rob.commitValid) <= 1.U, "Only one v inst should be walked or committed")
  
  when(io.redirect.valid) {
    walkNum := PopCount(flushVec)
  }.elsewhen(!commitValid) {
    walkNum := walkNum - PopCount(io.commit.rat.mask)
  }

  val deqPtrVec = Mux(io.commit.rob.isCommit, commitPtrVec, walkPtrVec)
  val deqEntryVec = Wire(Vec(8, new VRobEntry))
  val deqRobHitVec = Wire(Vec(8, Bool()))
  deqPtrVec.zipWithIndex.foreach {
    case (ptr, i) => {
      val entry = array(ptr)
      deqEntryVec(i) := entry
      deqRobHitVec(i) := entry.robIdx === commitRobIdx
    }
  }

  io.commit.rat.doCommit := commitValid
  io.commit.rat.doWalk := walkNum =/= 0.U && !commitValid

  (deqEntryVec).zip(deqRobHitVec).zipWithIndex.foreach {
    case ((e, hit), i) => {
      val actDeq = Mux(commitValid, hit && entryNum > i.U, entryNum > i.U && walkNum.orR)
      io.commit.rat.mask(i) := actDeq
      io.commit.rat.lrIdx(i) := e.logicRegIdx
      io.commit.rat.prIdxNew(i) := e.newPhyRegIdx
      io.commit.rat.prIdxOld(i) := e.oldPhyRegIdx
      when(actDeq) {
        array_v(deqPtrVec(i)) := false.B
      }
    }
  }

  when(commitValid) {
    deqPtr := deqPtr + PopCount(io.commit.rat.mask)
  }
  when(walkNum.orR) {
    enqPtr := enqPtr - PopCount(io.commit.rat.mask)
  }.otherwise {
    enqPtr := enqPtr + PopCount(io.enq.map(_.valid))
  }

  io.blockRename := walkNum.orR
}