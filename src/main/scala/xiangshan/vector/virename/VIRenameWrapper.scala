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
        //robIdxQueue has walk req
        val hasWalk = Output(Bool())
        //rename in
        val renameReq = Vec(VIRenameWidth, Flipped(ValidIO(new Bundle {
            val uopIn = new MicroOp
            val robIdx = new RobPtr
        })))
        val canRename = Output(Bool())
        //rename out
        val uopOut = Vec(VIRenameWidth, ValidIO(new MicroOp))
        //commit
        val commit = new VIRobIdxQueueEnqIO
    })

    val rename = new VIRename

    //when vector pipeline is walking, block all pipeline
    io.hasWalk := rename.io.hasWalk

    //rename
    val reqMask = Wire(Vec(VIRenameWidth, Bool()))
    reqMask := io.renameReq.map(req => req.valid)

    val robIdxForRename = Wire(Vec(VIRenameWidth, UInt(log2Up(RobSize).W)))
    robIdxForRename := io.renameReq.map(req => req.bits.robIdx.value)
    val reqForRename = Wire(Vec(VIRenameWidth, new Bundle {
        val lvs1 = UInt(5.W)
        val lvs2 = UInt(5.W)
        val lvd = UInt(5.W)
        val needRename = Bool()
    }))
    for((req, i) <- reqForRename.zipWithIndex) {
        req.lvs1 := io.renameReq(i).bits.uopIn.ctrl.lsrc(0)
        req.lvs2 := io.renameReq(i).bits.uopIn.ctrl.lsrc(1)
        req.lvd := io.renameReq(i).bits.uopIn.ctrl.lsrc(2)
        req.needRename := io.renameReq(i).bits.uopIn.canRename
    }
    rename.io.renameReq.req := reqForRename
    rename.io.renameReq.robIdx := robIdxForRename
    rename.io.renameReq.mask := reqMask
    io.canRename := rename.io.renameReq.canAccept

    io.uopOut := DontCare
    (0 until VIRenameWidth).map(i => io.uopOut(i).bits := io.renameReq(i).bits)
    (0 until VIRenameWidth).map(i => io.uopOut(i).valid := reqMask(i) & io.canRename)

    //commit
    rename.io.commitReq <> io.commit

}
