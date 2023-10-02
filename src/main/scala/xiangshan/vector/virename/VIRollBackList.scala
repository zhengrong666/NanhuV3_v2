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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xs.utils._
import xiangshan.vector._

class RollBackListEntry(implicit p: Parameters) extends VectorBaseBundle {
  val robIdx = new RobPtr
  val logicRegIdx = UInt(5.W)
  val oldPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
  val newPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
}

class RollbackListPayload(implicit p: Parameters) extends VectorBaseModule {
  private val enqNum = VIRenameWidth
  private val size = VIPhyRegsNum
  val io = IO(new Bundle{
    val enq = Input(Vec(enqNum, Valid(new Bundle{
      val addr = UInt(log2Ceil(size).W)
      val data = new RollBackListEntry
    })))
    val read = new Bundle {
      val addr = Input(UInt(log2Ceil(size).W))
      val robPtr = Input(new RobPtr)
      val commit = Input(Bool())
      val data = Output(Vec(8, new Bundle{
        val hit = Bool()
        val logicRegIdx = UInt(5.W)
        val oldPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
        val newPhyRegIdx = UInt(VIPhyRegIdxWidth.W)
      }))
    }
  })
  private val array = Reg(Vec(size, new RollBackListEntry))
  for((entry, idx) <- array.zipWithIndex){
    val wens = io.enq.map(e => e.valid && e.bits.addr === idx.U)
    val data = Mux1H(wens, io.enq.map(_.bits.data))
    when(Cat(wens).orR){
      entry := data
    }
  }
  io.read.data.zipWithIndex.foreach({case(d,i) =>
    val entry = Mux(io.read.commit, array(io.read.addr + i.U), array(io.read.addr - i.U))
    d.hit := entry.robIdx === io.read.robPtr
    d.logicRegIdx := entry.logicRegIdx
    d.oldPhyRegIdx := entry.oldPhyRegIdx
    d.newPhyRegIdx := entry.newPhyRegIdx
  })
}

class RollBackListRenamePort(implicit p: Parameters) extends VectorBaseBundle {
  val robIdx = new RobPtr
  val lrIdx = UInt(5.W)
  val oldPrIdx = UInt(VIPhyRegIdxWidth.W)
  val newPrIdx = UInt(VIPhyRegIdxWidth.W)
}

class VIRollBackList(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val rename = Vec(VIRenameWidth, Flipped(ValidIO(new RollBackListRenamePort)))
    val commit = new Bundle {
      val rob = Flipped(new RobCommitIO)
      val rat = Output(new VIRatCommitPort)
    }
  })

  private class RollBackListPtr extends CircularQueuePtr[RollBackListPtr](VIPhyRegsNum)

  private val headPtr = RegInit(0.U.asTypeOf(new RollBackListPtr))
  private val tailPtr = RegInit(0.U.asTypeOf(new RollBackListPtr))

  private val entryNum = distanceBetween(headPtr, tailPtr)
  private val payload = Module(new RollbackListPayload)

  //rename write robIdx、sRAT_old(from sRAT)、sRAT_new(from freeList)
  private val allocateDeltas = Wire(Vec(VIRenameWidth, UInt(log2Ceil(VIRenameWidth).W)))
  allocateDeltas.zipWithIndex.foreach({case(d, i) =>
    if(i == 0){
      d := 0.U
    } else {
      d := PopCount(io.rename.take(i).map(_.valid))
    }
  })

  for((e, i) <- payload.io.enq.zipWithIndex){
    e.valid := io.rename(i).valid
    e.bits.addr := (headPtr + allocateDeltas(i)).value
    e.bits.data.robIdx := io.rename(i).bits.robIdx
    e.bits.data.logicRegIdx := io.rename(i).bits.lrIdx
    e.bits.data.oldPhyRegIdx := io.rename(i).bits.oldPrIdx
    e.bits.data.newPhyRegIdx := io.rename(i).bits.newPrIdx
  }

  //commit
  private val robIdxSel = Mux(io.commit.rob.isCommit, io.commit.rob.commitValid, io.commit.rob.walkValid)
  private val rollingRobIdx = Mux1H(robIdxSel, io.commit.rob.robIdx)
  payload.io.read.robPtr := rollingRobIdx
  payload.io.read.addr := Mux(io.commit.rob.isCommit, tailPtr.value, headPtr.value)
  payload.io.read.commit := io.commit.rob.isCommit

  io.commit.rat.doCommit := io.commit.rob.isCommit
  io.commit.rat.doWalk := io.commit.rob.isWalk
  assert(PopCount(Seq(io.commit.rat.doCommit, io.commit.rat.doWalk)) <= 1.U, "Walk and commit at the same time!")
  for(i <- 0 until 8){
    io.commit.rat.mask(i) := payload.io.read.data(i).hit && i.U <= entryNum
    io.commit.rat.lrIdx(i) := payload.io.read.data(i).logicRegIdx
    io.commit.rat.prIdxOld(i) := payload.io.read.data(i).oldPhyRegIdx
    io.commit.rat.prIdxNew(i) := payload.io.read.data(i).newPhyRegIdx
  }
  private val rollLeaving = PopCount(io.commit.rat.mask)

  private val commitLeaving = Mux(io.commit.rat.doCommit, rollLeaving, 0.U)
  private val walkLeaving = Mux(io.commit.rat.doWalk, rollLeaving, 0.U)
  private val doEnq = io.rename.map(_.valid).reduce(_||_)
  private val enqNum = PopCount(io.rename.map(_.valid))
  when(io.commit.rob.isCommit){
    tailPtr := tailPtr + commitLeaving
  }
  when(io.commit.rob.isWalk || doEnq){
    headPtr := headPtr + enqNum - walkLeaving
  }
}
