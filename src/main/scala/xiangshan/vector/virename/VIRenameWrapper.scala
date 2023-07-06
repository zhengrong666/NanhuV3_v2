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

class VIRenameWrapper(implicit p: Parameters) extends VectorBaseModule {
    val io = IO(new Bundle {
        val hasWalk = Output(Bool())
        val in  = Input(Vec(VIRenameWidth, new MicroOp))
        val mask = Input(Vec(VIRenameWidth, Bool()))
        val robIdx = Input(Vec(VIRenameWidth, UInt(log2Up(RobSize).W)))
        val out = Output(Vec(VIRenameWidth, new MicroOp))
    })

    val rename = new VIRename
    
    //when vector pipeline is walking, block all pipeline
    val walk = rename.io.hasWalk
    io.hasWalk := walk

    val renameReq = Wire(new VIRenameReq)
    renameReq.robIdx := io.robIdx
    renameReq.mask := io.mask
    for(i <- 0 until VIRenameWidth) {
        renameReq.req(i).lvs1 := io.in(i).psrc(i)
        
    }
}
