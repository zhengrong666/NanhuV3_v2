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

    Free List for allocating free physical reg id
---------------------------------------------------------------------------------------*/

package xiangshan.vector.virename

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

import xiangshan._
import utils._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, CircularShift}

import xiangshan.vector._

class VIFreeListBundle(implicit p: Parameters) extends VectorBaseBundle {
    def reqNumWidth: Int = log2Up(VIRenameWidth + 1)

    //be different from int and float rename module
    //freeEntry num, determines whether the requests can enter RenameModule
    val canAllocateNum  = Output(UInt(reqNumWidth.W))
    val doAllocate      = Input(Bool())

    //connects with RAT, offer PhyRegIndex
    val allocateReqNum = Input(UInt(reqNumWidth.W))
    val allocatePhyReg = Output(Vec(VIRenameWidth, UInt(VIPhyRegIdxWidth.W)))

    //connects with RollBackList
    val releaseMask     = Input(UInt(VICommitWidth.W))
    val releasePhyReg   = Input(Vec(VICommitWidth, UInt(VIPhyRegIdxWidth.W)))
}

class VIFreeList(implicit p: Parameters) extends VectorBaseModule with HasCircularQueuePtrHelper {
    val io = IO(new VIFreeListBundle)

    class VIFreeListPtr extends CircularQueuePtr[VIFreeListPtr](VIPhyRegsNum)
    object VIFreeListPtr {
        def apply(f: Boolean, v: Int): VIFreeListPtr = {
            val ptr = Wire(new VIFreeListPtr)
            ptr.flag := f.B
            ptr.value := v.U
            ptr
        }
    }

    //free list
    val freeList_ds = VecInit(Seq.tabulate(VIPhyRegsNum)(i => i.U(VIPhyRegIdxWidth.W)))
    val freeList    = RegInit(freeList_ds)

    //head and tail pointer
    val headPtr = RegInit(VIFreeListPtr(false, 0))
    val tailPtr = RegInit(VIFreeListPtr(true, 0))

    //Allocate
    val freeEntryNum = Wire(UInt(log2Up(VIPhyRegsNum + 1).W))
    freeEntryNum := distanceBetween(tailPtr, headPtr) + 1.U

    io.canAllocateNum := Mux(freeEntryNum >= VIRenameWidth.U, VIRenameWidth.U, freeEntryNum)

    //val allocateNum = io.allocateReqNum
    val headPtrNext = headPtr + io.allocateReqNum
    headPtr := Mux(io.doAllocate, headPtrNext, headPtr)

    val headPtrOHVec = Wire(Vec(VIRenameWidth, UInt(VIPhyRegsNum.W)))
    for(i <- 0 until VIRenameWidth) {
        headPtrOHVec(i) := (headPtr + i.U).toOH
    }

    val phyRegCandidates = Wire(Vec(VIRenameWidth, UInt(VIPhyRegIdxWidth.W)))
    phyRegCandidates := headPtrOHVec.map(sel => Mux1H(sel, freeList))
    io.allocatePhyReg := phyRegCandidates

    //Release
    val releaseNum = PopCount(io.releaseMask)
    val tailPtrNext = tailPtr + releaseNum
    tailPtr := tailPtrNext

    for(i <- 0 until VICommitWidth) {
        val releasePtr = tailPtr + i.U
        when(io.releaseMask(i) === 1.U) {
            freeList(releasePtr.value) := io.releasePhyReg(i)
        }
    }
}
