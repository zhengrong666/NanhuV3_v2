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

class RollBackListRenameWritePort(implicit p: Parameters) extends VectorBaseBundle {
    val robIdx      = UInt(log2Up(RobSize).W)
    val lrIdx       = UInt(5.W)
    val oldPrIdx    = UInt(VIPhyRegIdxWidth.W)
    val newPrIdx    = UInt(VIPhyRegIdxWidth.W)
}

class RollBackListRenamePort(implicit p: Parameters) extends VectorBaseBundle {
    val doRename    = Input(Bool())
    val writePorts  = Input(Vec(VIRenameWidth, new RollBackListRenameWritePort))
    val mask        = Input(UInt(VIRenameWidth.W))
}

class RollBackListCommitPort(implicit p: Parameters) extends VectorBaseBundle {
    val req     = Flipped(new VIRobIdxQueueDeqIO)
    val resp    = Output(new VIRatCommitPort)
}

class RollBackListBundle(implicit p: Parameters) extends VectorBaseBundle {
    val renamePort = new RollBackListRenamePort
    val commitPort = new RollBackListCommitPort
}

class RollBackListEntry(implicit p: Parameters) extends VectorBaseBundle {
    val robIdx          = UInt(log2Up(RobSize).W)
    val logicRegIdx     = UInt(5.W)
    val oldPhyRegIdx    = UInt(VIPhyRegIdxWidth.W)
    val newPhyRegIdx    = UInt(VIPhyRegIdxWidth.W)
}

class VIRollBackList(implicit p: Parameters) extends VectorBaseModule  with HasCircularQueuePtrHelper{
    val io = IO(new RollBackListBundle)

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
    val rollBackList    = Reg(rollBackList_ds)

    val headPtr = RegInit(RollBackListPtr(false, 0))
    val tailPtr = RegInit(RollBackListPtr(false, 0))

    //rename write robIdx、sRAT_old(from sRAT)、sRAT_new(from freeList)
    val renameWritePorts    = io.renamePort.writePorts
    val renameWriteMask     = io.renamePort.mask

    for((w, i) <- renameWritePorts.zipWithIndex) { 
        when(io.renamePort.doRename && renameWriteMask(i)) {
            val writePtr = (headPtr + i.U).value
            rollBackList(writePtr).robIdx := w.robIdx
            rollBackList(writePtr).oldPhyRegIdx := w.oldPrIdx
            rollBackList(writePtr).newPhyRegIdx := w.newPrIdx
        }
    }

    val headPtrNextRename = RegNext(headPtr + PopCount(renameWriteMask))
    headPtr := Mux(io.renamePort.doRename, headPtrNextRename, headPtr)

    //commit and rollBack
    val entryEqualHead = Wire(Vec(VICommitWidth-1, Bool()))
    val entryEqualTail = Wire(Vec(VICommitWidth-1, Bool()))

    val hasRobIdx           = RegInit(Bool(), false.B)
    val hasRobCommitType    = Reg(Bool())

    val hasRobIdxHead = RegInit(Bool(), false.B)
    val hasRobIdxTail = RegInit(Bool(), false.B)

    val RobIdxHead = Reg(UInt(log2Up(RobSize).W))
    val RobIdxTail = Reg(UInt(log2Up(RobSize).W))

    RobIdxHead := rollBackList((headPtr + VICommitWidth.U).value).robIdx
    RobIdxTail := rollBackList((tailPtr - VICommitWidth.U).value).robIdx


    val entryNum = Wire(UInt(VIPhyRegIdxWidth.W))
    entryNum := distanceBetween(tailPtr, headPtr)

    for(i <- 1 until VICommitWidth) {
        entryEqualTail(i-1) := (rollBackList((tailPtr - i.U).value).robIdx === rollBackList((tailPtr - (i-1).U).value).robIdx) & (entryNum >= i.U)
        entryEqualHead(i-1) := (rollBackList((headPtr + i.U).value).robIdx === rollBackList((tailPtr + (i-1).U).value).robIdx) & (entryNum >= i.U)
    }

    when(entryNum === 0.U) {
        io.commitPort.req.canCommitRobIdxNum := 0.U
        io.commitPort.req.canRollBackRobIdxNum := 0.U
    }.otherwise {
        val entryEqualHeadNum = PopCount(entryEqualHead)
        val entryEqualTailNum = PopCount(entryEqualHead)
        io.commitPort.req.canCommitRobIdxNum    := Mux(hasRobIdxTail === true.B, entryEqualHeadNum, (entryEqualHeadNum + 1.U))
        io.commitPort.req.canRollBackRobIdxNum  := Mux(hasRobIdxHead === true.B, entryEqualTailNum, (entryEqualTailNum + 1.U))
    }

    val hasPendingRobIdx    = RegInit(Bool(), false.B)
    val pendingRobIdx       = Reg(UInt(log2Up(RobSize).W))
    val pendingType         = Reg(Bool())

    //commit, tailPtr -> tailPtr+n
    val commitEntry     = Wire(Vec(VICommitWidth, new RollBackListEntry))
    val commitNextEntry = Wire(new RollBackListEntry)
    commitEntry     := (0 until VICommitWidth).map(i => rollBackList((tailPtr + i.U).value))
    commitNextEntry := rollBackList((tailPtr + VICommitWidth.U).value)

    val commitEqualMask = Wire(Vec(VICommitWidth, Bool()))
    commitEqualMask := commitEntry.map(entry => PopCount((0 until VICommitWidth).map(i => (io.commitPort.req.mask(i) === true.B && io.commitPort.req.robIdx(i) === entry.robIdx))) =/= 0.U)
    
    val commitEqualMaskPending = Wire(Vec(VICommitWidth, Bool()))
    commitEqualMaskPending := commitEntry.map(entry => PopCount((0 until VICommitWidth).map(i => (entry.robIdx === pendingRobIdx && hasPendingRobIdx === true.B && pendingType === false.B))) =/= 0.U)
    
    val commitMask = Wire(Vec(VICommitWidth, Bool()))
    commitMask := (0 until VICommitWidth).map(i => commitEqualMaskPending(i) || commitEqualMask(i))

    val commitNum   = PopCount(commitMask)
    val tailPtrNext = tailPtr + commitNum
    tailPtr := Mux(io.commitPort.req.doCommit, tailPtrNext, tailPtr)

    //roll back
    val walkEntry       = Wire(Vec(VICommitWidth, new RollBackListEntry))
    val walkNextEntry   = Wire(new RollBackListEntry)
    walkEntry       := (0 until VICommitWidth).map(i => rollBackList((headPtr - i.U).value))
    walkNextEntry   := rollBackList((headPtr - VICommitWidth.U).value)

    val walkEqualMask = Wire(Vec(VICommitWidth, Bool()))
    walkEqualMask := walkEntry.map(entry => PopCount((0 until VICommitWidth).map(i => (io.commitPort.req.mask(i) === true.B && io.commitPort.req.robIdx(i) === entry.robIdx))) =/= 0.U)

    val walkEqualMaskPending = Wire(Vec(VICommitWidth, Bool()))
    walkEqualMaskPending := walkEntry.map(entry => PopCount((0 until VICommitWidth).map(i => (entry.robIdx === pendingRobIdx && hasPendingRobIdx === true.B && pendingType === true.B))) =/= 0.U)

    val walkMask = Wire(Vec(VICommitWidth, Bool()))
    walkMask := (0 until VICommitWidth).map(i => walkEqualMaskPending(i) || walkEqualMask(i))

    val walkNum         = PopCount(walkMask)
    val headPtrNextWalk = headPtr - walkNum

    headPtr := Mux(io.commitPort.req.doWalk && (!io.renamePort.doRename), headPtrNextWalk, Mux(io.renamePort.doRename, headPtrNextRename, headPtr))

    val pendingRobIdxCommit = Wire(UInt(log2Up(RobSize).W))
    val pendingRobIdxWalk   = Wire(UInt(log2Up(RobSize).W))
    pendingRobIdxCommit := commitEntry(VICommitWidth - 1).robIdx
    pendingRobIdxWalk   := commitEntry(VICommitWidth - 1).robIdx


    when((commitMask(VICommitWidth - 1) === true.B) && (commitEntry(VICommitWidth-1).robIdx === rollBackList((tailPtr + VICommitWidth.U).value).robIdx)) {
        hasPendingRobIdx    := true.B
        pendingRobIdx       := pendingRobIdxCommit
        pendingType         := Mux(io.commitPort.req.doCommit, false.B, true.B)
    }.elsewhen((walkMask(VICommitWidth - 1) === true.B) && (walkEntry(VICommitWidth - 1).robIdx === rollBackList((headPtr - VICommitWidth.U).value).robIdx)) {
        hasPendingRobIdx    := true.B
        pendingRobIdx       := pendingRobIdxWalk
        pendingType         := true.B
    }.otherwise {
        hasPendingRobIdx := false.B
    }

    io.commitPort.req.pendingType       := pendingType
    io.commitPort.req.hasPendingRobIdx  := hasPendingRobIdx

    val commitLogicIdx  = Wire(Vec(VICommitWidth, UInt(5.W)))
    val walkLogicIdx    = Wire(Vec(VICommitWidth, UInt(5.W)))
    commitLogicIdx  := commitEntry.map(entry => entry.logicRegIdx)
    walkLogicIdx    := walkEntry.map(entry => entry.logicRegIdx)

    val commitPhyIdxOld = Wire(Vec(VICommitWidth, UInt(log2Up(RobSize).W)))
    val walkPhyIdxOld   = Wire(Vec(VICommitWidth, UInt(log2Up(RobSize).W)))
    commitPhyIdxOld := commitEntry.map(entry => entry.oldPhyRegIdx)
    walkPhyIdxOld   := walkEntry.map(entry => entry.oldPhyRegIdx)

    val commitPhyIdxNew = Wire(Vec(VICommitWidth, UInt(log2Up(RobSize).W)))
    val walkPhyIdxNew   = Wire(Vec(VICommitWidth, UInt(log2Up(RobSize).W)))
    commitPhyIdxNew := commitEntry.map(entry => entry.newPhyRegIdx)
    walkPhyIdxNew   := walkEntry.map(entry => entry.newPhyRegIdx)

    commitLogicIdx  := commitEntry.map(entry => entry.logicRegIdx)
    walkLogicIdx    := walkEntry.map(entry => entry.logicRegIdx)

    io.commitPort.resp.lrIdx    := Mux(io.commitPort.req.doCommit, commitLogicIdx, walkLogicIdx)
    io.commitPort.resp.mask     := Mux(io.commitPort.req.doCommit, commitMask, walkMask)
    io.commitPort.resp.prIdxOld := Mux(io.commitPort.req.doCommit, commitPhyIdxOld, walkPhyIdxOld)
    io.commitPort.resp.prIdxNew := Mux(io.commitPort.req.doCommit, commitPhyIdxNew, walkPhyIdxNew)
    io.commitPort.resp.doCommit := io.commitPort.req.doCommit
    io.commitPort.resp.doWalk   := io.commitPort.req.doWalk
}
