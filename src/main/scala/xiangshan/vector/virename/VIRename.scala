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

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import xiangshan._
import utils._

import xiangshan.vector._

class VIRenameReqLr(implicit p: Parameters) extends VectorBaseBundle {
    def robIdxWidth = log2Up(RobSize)

    val lvs1        = UInt(5.W)
    val lvs2        = UInt(5.W)
    val lvd         = UInt(5.W)
    val robIdx      = UInt(robIdxWidth.W)
    val needRename  = Bool()
}

// class VIRenameReq(implicit p: Parameters) extends VectorBaseBundle {
//     val req         = Vec(VIRenameWidth, new VIRenameReqLr)
//     val mask        = UInt(VIRenameWidth.W)
// }

class VIRenameResp(implicit p: Parameters) extends VectorBaseBundle {
    val pvs1    = UInt(VIPhyRegIdxWidth.W)
    val pvs2    = UInt(VIPhyRegIdxWidth.W)
    val pvd     = UInt(VIPhyRegIdxWidth.W)
    val pmask   = UInt(VIPhyRegIdxWidth.W)
}

class VIRename(implicit p: Parameters) extends VectorBaseModule {
    val io = IO(new Bundle{
        val redirect    = Flipped(ValidIO(new Redirect))
        //rename, from waitqueue
        val renameReq   = Vec(VIRenameWidth, Flipped(DecoupledIO(new VIRenameReqLr)))
        val renameResp  = Vec(VIRenameWidth, DecoupledIO(new VIRenameResp))
        //commit, from ROB
        val commitReq   = Flipped(DecoupledIO(new VIRobIdxQueueEnqIO))
        val hasWalk     = Output(Bool())
    })

    val freeList        = Module(new VIFreeList)
    val renameTable     = Module(new VIRenameTable)
    val rollBackList    = Module(new VIRollBackList)
    val robIdxQueue     = Module(new VIRobIdxQueue(VIWalkRobIdxQueueWidth))

    //-------------------------------------------- Rename --------------------------------------------
    
    val renameReqNum = PopCount(io.renameReq.map(port => port.valid && (port.bits.needRename === true.B)))

    //TODO: !redirect && !walk
    val canAllocateNum = Wire(UInt(log2Up(VIRenameWidth + 1).W))
    canAllocateNum := freeList.io.canAllocateNum
    for(i <- 0 until VIRenameWidth) {
        io.renameReq(i).ready := (i.U < canAllocateNum)
    }

    val doRename = Wire(Bool())
    doRename := (renameReqNum <= freeList.io.canAllocateNum) && (!io.redirect.valid) && (!io.hasWalk)// & queue.hasWalk

    //doRename, allocate FreeList Ptr, write rat and rollbackList
    freeList.io.doAllocate                  := doRename
    rollBackList.io.renamePort.doRename     := doRename

    freeList.io.allocateReqNum := renameReqNum
    renameTable.io.renameWritePort.prIdx := freeList.io.allocatePhyReg

    //read RAT
    val renameReqPort = io.renameReq
    for((rdp, rp) <- renameTable.io.renameReadPorts.zip(renameReqPort)) {
        rdp.vd.lrIdx    := rp.bits.lvd
        rdp.vs1.lrIdx   := rp.bits.lvs1
        rdp.vs2.lrIdx   := rp.bits.lvs2
    }

    for((port, rdp) <- io.renameResp.zip(renameTable.io.renameReadPorts)) {
        port.bits.pvs1   := rdp.vs1.prIdx
        port.bits.pvs2   := rdp.vs2.prIdx
        port.bits.pvd    := rdp.vd.prIdx
        port.bits.pmask  := rdp.vmask
    }

    for((resp, req) <- io.renameResp.zip(io.renameReq)) {
        resp.valid := req.valid
        //req.ready := resp.ready
    }

    //read old value
    for((oldRdp, rp) <- renameTable.io.oldPhyRegIdxReadPorts.zip(renameReqPort)) {
        oldRdp.lrIdx := rp.bits.lvd
    }

    //write RAT
    val renameMask = VecInit(io.renameReq.map(_.fire()))
    val ratRenamePortW = renameTable.io.renameWritePort
    ratRenamePortW.doRename := doRename
    ratRenamePortW.mask     := renameMask.asUInt
    ratRenamePortW.lrIdx    := io.renameReq.map(_.bits.lvd)
    ratRenamePortW.prIdx    := freeList.io.allocatePhyReg

    //write roll back list
    for((wp, i) <- rollBackList.io.renamePort.writePorts.zipWithIndex) {
        wp.lrIdx    := io.renameReq(i).bits.lvd
        wp.newPrIdx := freeList.io.allocatePhyReg(i)
        wp.oldPrIdx := renameTable.io.oldPhyRegIdxReadPorts(i).prIdx
        wp.robIdx   := io.renameReq(i).bits.robIdx
    }
    rollBackList.io.renamePort.mask := VecInit(io.renameReq.map(e => e.bits.needRename)).asUInt

    //-------------------------------------------- TODO: commit & walk --------------------------------------------
    robIdxQueue.io.in <> io.commitReq
    rollBackList.io.commitPort.req <> robIdxQueue.io.out
    renameTable.io.commitPort <> rollBackList.io.commitPort.resp
    
    freeList.io.releaseMask := rollBackList.io.commitPort.resp.mask
    freeList.io.releasePhyReg := Mux(rollBackList.io.commitPort.resp.doCommit, rollBackList.io.commitPort.resp.prIdxOld, rollBackList.io.commitPort.resp.prIdxNew)

    io.hasWalk := robIdxQueue.io.hasWalk
}
