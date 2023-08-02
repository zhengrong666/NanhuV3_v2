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
    Date: 2023-07-05
    email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.vector.virename

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import xiangshan._
import utils._

import xiangshan.vector._
import xiangshan.backend.rob._

class VIRenameWrapper(implicit p: Parameters) extends VectorBaseModule {
    val io = IO(new Bundle {
        val redirect = Flipped(ValidIO(new Redirect))

        //rename in, from WaitQueue
        val canAccept   = Output(Bool())
        val uopIn       = Vec(VIRenameWidth, Flipped(ValidIO(new MicroOp)))

        //rename out, to Dispatch
        val uopOut = Vec(VIRenameWidth, ValidIO(new MicroOp))
        
        //commit, from ROB via RobIdxQueue
        val commit = new VIRobIdxQueueEnqIO
    })

    val rename = Module(new VIRename)

    //when vector pipeline is walking, block all pipeline
    val hasWalk = rename.io.hasWalk

    //rename
    val reqMask = Wire(UInt(VIRenameWidth.W))
    reqMask := io.uopIn.map(req => req.valid).asUInt

    val robIdxForRename = Wire(Vec(VIRenameWidth, UInt(log2Up(RobSize).W)))
    robIdxForRename := io.uopIn.map(req => req.bits.robIdx.value)

    val reqForRename = Wire(Vec(VIRenameWidth, new VIRenameReqLr))

    for((req, i) <- reqForRename.zipWithIndex) {
        req.lvs1 := io.uopIn(i).bits.ctrl.lsrc(0)
        req.lvs2 := io.uopIn(i).bits.ctrl.lsrc(1)
        req.lvd  := io.uopIn(i).bits.ctrl.lsrc(2)
        req.robIdx      := io.uopIn(i).bits.robIdx.value
        req.needRename  := io.uopIn(i).bits.canRename
    }
    rename.io.renameReq.req     := reqForRename
    rename.io.renameReq.mask    := reqMask

    io.canAccept := rename.io.renameReq.canAccept && (!hasWalk) && (!io.redirect.valid)

    (0 until VIRenameWidth).map(i => io.uopOut(i).bits  := io.uopIn(i).bits)
    (0 until VIRenameWidth).map(i => io.uopOut(i).valid := reqMask(i) & io.canAccept)

    val reqSrcType = Wire(Vec(VIRenameWidth, Vec(3, SrcType())))
    reqSrcType := io.uopIn.map(req => req.bits.ctrl.srcType)

    for((port, i) <- io.uopOut.zipWithIndex) {
        port.bits.psrc(0) := Mux(reqSrcType(i)(0) === SrcType.vec, rename.io.renameResp(i).pvs1, io.uopIn(i).bits.psrc(0))
        port.bits.psrc(1) := Mux(reqSrcType(i)(1) === SrcType.vec, rename.io.renameResp(i).pvs2, io.uopIn(i).bits.psrc(1))
        port.bits.psrc(2) := Mux(reqSrcType(i)(2) === SrcType.vec, rename.io.renameResp(i).pvd, io.uopIn(i).bits.psrc(2))
    }

    //commit
    rename.io.commitReq <> io.commit
}
