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
import xs.utils.CircularShift
import xiangshan.backend.rename.freelist.BaseFreeList
import xiangshan.vector.virename._

class VIRatReadPortSingle(implicit p: Parameters) extends VIRenameBundle {
    //input logic reg index, output physical reg index
    val lrIdx = Input(UInt(5.W))
    val prIdx = Output(UInt(VIPhyRegIdxWidth.W))
}

class VIRatReadPortInstr(implicit p: Parameters) extends VIRenameBundle {
    val vs1 = new VIRatReadPortSingle
    val vs2 = new VIRatReadPortSingle
    val vd = new VIRatReadPortSingle
}

class VIRatRenamePort(implicit p: Parameters) extends VIRenameBundle {
    val doRename = Input(Bool())
    val mask = Vec(VIRenameWidth, Input(Bool()))
    val lrIdx = Vec(VIRenameWidth, Input(UInt(5.W)))
    val prIdx = Vec(VIRenameWidth, Input(UInt(VIPhyRegIdxWidth.W)))
    //val needRename = Vec(VIRenameWidth, Input(Bool()))
}

class VIRatCommitPort(implicit p: Parameters) extends VIRenameBundle {
    val doCommit = Input(Bool())
    val doWalk = Input(Bool())
    val mask = Input(Vec(VICommitWidth, Bool()))
    val lrIdx = Input(Vec(VICommitWidth, UInt(5.W)))
    val prIdx = Input(Vec(VICommitWidth, UInt(VIPhyRegIdxWidth.W)))
}

class VIRenameTable(implicit p: Parameters) extends VIRenameModule {
    val io = IO(new Bundle{
        val renameReadPorts = Vec(VIRenameWidth, new VIRatReadPortInstr)
        val oldPhyRegIdxReadPorts = Vec(VIRenameWidth, new VIRatReadPortSingle)
        val renameWritePort = new VIRatRenamePort
        val commitPort = new VIRatCommitPort
        val debugReadPorts = Output(Vec(32, UInt(VIPhyRegIdxWidth.W))) //for difftest
    })
    //RAT
    val rat_ds = VecInit.tabulate(32)(i => i.U(VIPhyRegIdxWidth.W))
    val sRAT = RegInit(rat_ds)
    val aRAT = RegInit(rat_ds)

    io.debugReadPorts := aRAT

    //read
    for((port, i) <- io.renameReadPorts.zipWithIndex) {
        port.vs1.prIdx := (0 until i).foldLeft(sRAT(port.vs1.lrIdx))((p, k) => 
            Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === io.renameReadPorts(i).vs1.lrIdx && io.renameWritePort.doRename === true.B), 
                io.renameWritePort.prIdx(k), p))
        port.vs2.prIdx := (0 until i).foldLeft(sRAT(port.vs2.lrIdx))((p, k) => 
            Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === io.renameReadPorts(i).vs2.lrIdx && io.renameWritePort.doRename === true.B), 
                io.renameWritePort.prIdx(k), p))
        //port.vd.prIdx := sRAT(port.vd.lrIdx)
        port.vd.prIdx := (0 until i).foldLeft(sRAT(port.vd.lrIdx))((p, k) => 
            Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === io.renameReadPorts(i).vd.lrIdx && io.renameWritePort.doRename === true.B), 
                io.renameWritePort.prIdx(k), p))
    }

    //old regId read, for rollBackList storage
    val oldLrOHVec = io.oldPhyRegIdxReadPorts.map(port => UIntToOH(port.lrIdx))
    for((port, i) <- io.oldPhyRegIdxReadPorts.zipWithIndex) {
        port.prIdx := Mux1H(oldLrOHVec(i), sRAT)
    }

    //rename write sRAT
    val prIdxs_rename = io.renameWritePort.prIdx
    val lrIdxs_rename = io.renameWritePort.lrIdx
    for((lr, i) <- lrIdxs_rename.zipWithIndex) {
        when(io.renameWritePort.doRename && (io.renameWritePort.mask(i) === 1.U)) {
            sRAT(lr) := prIdxs_rename(i)
        }
    }

    val lrIdxs_commit = io.commitPort.lrIdx
    val prIdxs_commit = io.commitPort.prIdx
    //XSError((io.commitPort.doCommit && io.commitPort.doWalk), s"commit and walk")
    //commit write aRAT, rollBack write sRAT
    for((lr, i) <- lrIdxs_commit.zipWithIndex) {
        when(io.commitPort.doCommit && (io.commitPort.mask(i) === true.B)) {
            aRAT(lr) := prIdxs_commit(i)
        }.elsewhen(io.commitPort.doCommit && (io.commitPort.mask(i) === true.B)) {
            sRAT(lr) := prIdxs_commit(i)
        }
    }
}
