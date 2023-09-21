/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
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

/*--------------------------------------------------------------------------------------
	Author: GMX
	Date: 2023-06-28
	email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.vector.virename

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import xiangshan._
import utils._
import xs.utils._

import xiangshan.vector._
import xiangshan.backend.execute.exu.ExuType

class RollBackListRenamePort(implicit p: Parameters) extends VectorBaseBundle {
  val robIdx = UInt(log2Up(RobSize).W)
  val lrIdx = UInt(5.W)
  val oldPrIdx = UInt(VIPhyRegIdxWidth.W)
  val newPrIdx = UInt(VIPhyRegIdxWidth.W)
}

class RollBackListEntry(implicit p: Parameters) extends VectorBaseBundle {
  val robIdx = UInt(log2Up(RobSize).W)
  val logicRegIdx = UInt(5.W)
  val oldPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
  val newPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
}

class VIRollBackList(implicit p: Parameters) extends VectorBaseModule  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val rename = Vec(VIRenameWidth, Flipped(ValidIO(new RollBackListRenamePort)))
    val commit = new Bundle {
      val rob = Flipped(new RobCommitIO)
      val rat = Output(new VIRatCommitPort)
    }
  })

  class RollBackListPtr extends CircularQueuePtr[RollBackListPtr](VIPhyRegsNum)
  object RollBackListPtr {
    def apply(f: Boolean, v: Int): RollBackListPtr = {
      val ptr = Wire(new RollBackListPtr)
      ptr.flag    := f.B
      ptr.value   := v.U
      ptr
    }
  }

  val rollBackList_ds = Vec(VIPhyRegsNum, new RollBackListEntry)
  val rollBackList = Reg(rollBackList_ds)

  val headPtr = RegInit(RollBackListPtr(false, 0))
  val tailPtr = RegInit(RollBackListPtr(false, 0))

  val entryNum = distanceBetween(headPtr, tailPtr)

  //rename write robIdx、sRAT_old(from sRAT)、sRAT_new(from freeList)
  for((w, i) <- io.rename.zipWithIndex) { 
    when(w.valid) {
      val writePtr = (headPtr + i.U).value
      rollBackList(writePtr).robIdx := w.bits.robIdx
      rollBackList(writePtr).oldPhyRegIdx := w.bits.oldPrIdx
      rollBackList(writePtr).newPhyRegIdx := w.bits.newPrIdx
    }
  }

  //commit
  val commitRobSel = Mux(io.commit.rob.isCommit, io.commit.rob.commitValid, io.commit.rob.walkValid)
  val commitRobIdx = Mux1H(commitRobSel, io.commit.rob.robIdx)

  val commitCandidates = Wire(Vec(8, new RollBackListEntry))
  val commitValid = Wire(Vec(8, Bool()))
  val walkCandidates = Wire(Vec(8, new RollBackListEntry))
  val walkValid = Wire(Vec(8, Bool()))

  val commitSelVec = Wire(Vec(8, UInt(VIPhyRegIdxWidth.W)))
  val walkSelVec = Wire(Vec(8, UInt(VIPhyRegIdxWidth.W)))
  for(((c, w), i) <- commitSelVec.zip(walkSelVec).zipWithIndex) {
    c := (tailPtr + i.U).value
    w := (headPtr - i.U).value
  }

  commitCandidates.zip(commitSelVec).zip(commitValid).zipWithIndex.foreach {
    case (((e, id), v), i) => {
      val selVec = Wire(Vec(VIPhyRegsNum, Bool()))
      rollBackList.zip(selVec).foreach {
        case (re, sel) => {
          sel := (re.robIdx === commitRobIdx) && (entryNum > i.U)
        }
      }
      e := Mux1H(selVec, rollBackList)
      v := (selVec.asUInt =/= 0.U) && (entryNum > i.U)
    }
  }

  walkCandidates.zip(walkSelVec).zip(walkValid).zipWithIndex.foreach {
    case (((e, id), v), i) => {
      val selVec = Wire(Vec(VIPhyRegsNum, Bool()))
      rollBackList.zip(selVec).foreach {
        case (re, sel) => {
          sel := (re.robIdx === commitRobIdx) && (entryNum > i.U)
        }
      }
      e := Mux1H(selVec, rollBackList)
      v := (selVec.asUInt =/= 0.U) && (entryNum > i.U)
    }
  }

  val realCommit = io.commit.rob.isCommit 

  headPtr := Mux(io.commit.rob.isWalk, headPtr - PopCount(walkValid), headPtr + PopCount(io.rename.map(_.valid)))
  tailPtr := Mux(io.commit.rob.isCommit, tailPtr + PopCount(commitValid), tailPtr)

  io.commit.rat.doCommit := io.commit.rob.isCommit
  io.commit.rat.doWalk := io.commit.rob.isWalk
  io.commit.rat.lrIdx := Mux(io.commit.rob.isCommit, VecInit(commitCandidates.map(_.logicRegIdx)), VecInit(walkCandidates.map(_.logicRegIdx)))
  io.commit.rat.prIdxNew := Mux(io.commit.rob.isCommit, VecInit(commitCandidates.map(_.newPhyRegIdx)), VecInit(walkCandidates.map(_.newPhyRegIdx)))
  io.commit.rat.prIdxOld := Mux(io.commit.rob.isCommit, VecInit(commitCandidates.map(_.oldPhyRegIdx)), VecInit(walkCandidates.map(_.oldPhyRegIdx)))
  io.commit.rat.mask := Mux(io.commit.rob.isCommit, commitValid.asUInt, walkValid.asUInt)
}
