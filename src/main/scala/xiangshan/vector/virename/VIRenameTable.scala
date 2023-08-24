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
import xs.utils.CircularShift

import xiangshan.vector._

class VIRatReadPortSingle(implicit p: Parameters) extends VectorBaseBundle {
	//input logic reg index, output physical reg index
	val lrIdx = Input(UInt(5.W))
	val prIdx = Output(UInt(VIPhyRegIdxWidth.W))
}

class VIRatReadPortOneInstr(implicit p: Parameters) extends VectorBaseBundle {
	val vs1 = new VIRatReadPortSingle
	val vs2 = new VIRatReadPortSingle
	val vd  = new VIRatReadPortSingle
	val vmask = Output(UInt(VIPhyRegIdxWidth.W))
}

class VIRatRenamePort(implicit p: Parameters) extends VectorBaseBundle {
	val doRename    = Bool()
	val mask        = UInt(VIRenameWidth.W)
	val lrIdx       = Vec(VIRenameWidth, UInt(5.W))
	val prIdx       = Vec(VIRenameWidth, UInt(VIPhyRegIdxWidth.W))
}

class VIRatCommitPort(implicit p: Parameters) extends VectorBaseBundle {
	val doCommit    = Bool()
	val doWalk      = Bool()
	val mask        = UInt(VICommitWidth.W)
	val lrIdx       = Vec(VICommitWidth, UInt(5.W))
	val prIdxOld    = Vec(VICommitWidth, UInt(VIPhyRegIdxWidth.W))
	val prIdxNew    = Vec(VICommitWidth, UInt(VIPhyRegIdxWidth.W))
}

class VIRenameTable(implicit p: Parameters) extends VectorBaseModule {
	val io = IO(new Bundle{
		val renameReadPorts         = Vec(VIRenameWidth, new VIRatReadPortOneInstr)
		val oldPhyRegIdxReadPorts   = Vec(VIRenameWidth, new VIRatReadPortSingle) //for RollBackList write
		val renameWritePort         = Input(new VIRatRenamePort)
		val commitPort      = Input(new VIRatCommitPort)
		val debugReadPorts  = Output(Vec(32, UInt(VIPhyRegIdxWidth.W))) //for difftest
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
		port.vmask := (0 until i).foldLeft(sRAT(0.U))((p, k) => 
			Mux((io.renameWritePort.mask(k) === true.B && io.renameWritePort.lrIdx(k) === 0.U && io.renameWritePort.doRename === true.B), 
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
	val prIdxs_commit = Mux(io.commitPort.doCommit, io.commitPort.prIdxNew, io.commitPort.prIdxOld)
	//XSError((io.commitPort.doCommit && io.commitPort.doWalk), s"commit and walk")
	//commit write aRAT, rollBack write sRAT
	for((lr, i) <- lrIdxs_commit.zipWithIndex) {
		when(io.commitPort.doCommit && (io.commitPort.mask(i) === true.B)) {
			aRAT(lr) := prIdxs_commit(i)
		}.elsewhen(io.commitPort.doWalk && (io.commitPort.mask(i) === true.B)) {
			sRAT(lr) := prIdxs_commit(i)
		}
	}
}
