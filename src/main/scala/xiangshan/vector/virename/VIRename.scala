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

class VIRenameReq(implicit p: Parameters) extends VectorBaseBundle {
    val req         = Input(Vec(VIRenameWidth, new VIRenameReqLr))
    val mask        = Input(UInt(VIRenameWidth.W))
    val canAccept   = Output(Bool())
}

class VIRenameResp(implicit p: Parameters) extends VectorBaseBundle {
    val pvs1    = UInt(VIPhyRegIdxWidth.W)
    val pvs2    = UInt(VIPhyRegIdxWidth.W)
    val pvd     = UInt(VIPhyRegIdxWidth.W)
}

class VIRename(implicit p: Parameters) extends VectorBaseModule {
    val io = IO(new Bundle{
        val redirect    = Flipped(ValidIO(new Redirect))

        val renameReq   = new VIRenameReq
        val renameResp  = Output(Vec(VIRenameWidth, new VIRenameResp))

        val commitReq   = new VIRobIdxQueueEnqIO
        val hasWalk     = Output(Bool())
        //val redirect    = new
    })

    val freeList        = Module(new VIFreeList)
    val renameTable     = Module(new VIRenameTable)
    val rollBackList    = Module(new VIRollBackList)
    val robIdxQueue     = Module(new VIRobIdxQueue(VIWalkRobIdxQueueWidth))

    //-------------------------------------------- Rename --------------------------------------------
    val doRename = Wire(Bool())
    val renameReqNum = PopCount(io.renameReq.req.map(i => (i.needRename === true.B)))

    //TODO: !redirect && !walk
    doRename := (freeList.io.canAllocateNum >= renameReqNum) && (!io.redirect.valid) && (!io.hasWalk)// & queue.hasWalk

    freeList.io.doAllocate := doRename
    renameTable.io.renameWritePort.doRename := doRename
    rollBackList.io.renamePort.doRename := doRename
    //for handshake with vi wait queue
    io.renameReq.canAccept := doRename

    val freeListIO = freeList.io
    freeListIO.allocateReqNum := renameReqNum
    renameTable.io.renameWritePort.prIdx := freeListIO.allocatePhyReg

    //read RAT
    val renameReqPort = io.renameReq.req
    for((rdp, rp) <- renameTable.io.renameReadPorts.zip(renameReqPort)) {
        rdp.vd.lrIdx    := rp.lvd
        rdp.vs1.lrIdx   := rp.lvs1
        rdp.vs2.lrIdx   := rp.lvs2
    }

    for((port, rdp) <- io.renameResp.zip(renameTable.io.renameReadPorts)) {
        port.pvs1   := rdp.vs1.prIdx
        port.pvs2   := rdp.vs2.prIdx
        port.pvd    := rdp.vd.prIdx
    }

    //read old value
    for((oldRdp, rp) <- renameTable.io.oldPhyRegIdxReadPorts.zip(renameReqPort)) {
        oldRdp.lrIdx := rp.lvd
    }

    //write RAT
    val ratRenamePortW = renameTable.io.renameWritePort
    ratRenamePortW.lrIdx    := (0 until VIRenameWidth).map(i => io.renameReq.req(i).lvd)
    ratRenamePortW.mask     := (0 until VIRenameWidth).map(i => io.renameReq.req(i).needRename) //align port
    ratRenamePortW.prIdx    := freeListIO.allocatePhyReg

    //write roll back list
    for((wp, i) <- rollBackList.io.renamePort.writePorts.zipWithIndex) {
        wp.lrIdx := io.renameReq.req(i).lvd
        wp.newPrIdx := freeListIO.allocatePhyReg(i)
        wp.oldPrIdx := renameTable.io.oldPhyRegIdxReadPorts(i).prIdx
        wp.robIdx := io.renameReq.req(i).robIdx
    }
    rollBackList.io.renamePort.mask := io.renameReq.req.map(e => e.needRename)

    //-------------------------------------------- TODO: commit & walk --------------------------------------------
    robIdxQueue.io.in <> io.commitReq
    rollBackList.io.commitPort.req <> robIdxQueue.io.out
    renameTable.io.commitPort <> rollBackList.io.commitPort.resp
    
    freeList.io.releaseMask := rollBackList.io.commitPort.resp.mask
    freeList.io.releasePhyReg := Mux(rollBackList.io.commitPort.resp.doCommit, rollBackList.io.commitPort.resp.prIdxOld, rollBackList.io.commitPort.resp.prIdxNew)

    io.hasWalk := robIdxQueue.io.hasWalk
}
