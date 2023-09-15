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

class VIRename(implicit p: Parameters) extends VectorBaseModule {
  val io = IO(new Bundle{
    val redirect = Flipped(ValidIO(new Redirect))
    //rename, from waitqueue
    val rename = Vec(VIRenameWidth, new Bundle {
      val in = Flipped(DecoupledIO(new MicroOp))
      val out = DecoupledIO(new MicroOp)
    })
    //commit, from ROB
    val commit = Flipped(new RobCommitIO)
  })

  val freeList        = Module(new VIFreeList)
  val renameTable     = Module(new VIRenameTable)
  val rollBackList    = Module(new VIRollBackList)

  //-------------------------------------------- Rename --------------------------------------------
  val renameReqNum = PopCount(io.rename.map(port => port.in.fire() && (port.in.bits.canRename === true.B)))

  //TODO: !redirect && !walk
  val canAllocateNum = freeList.io.canAllocateNum

  val doRename = Wire(Bool())
  doRename := (renameReqNum <= freeList.io.canAllocateNum) && (!io.redirect.valid) && (!io.commit.isWalk)

  //doRename, allocate FreeList Ptr, write rat and rollbackList
  freeList.io.doAllocate := doRename
  rollBackList.io.rename.doRename := doRename

  freeList.io.allocateReqNum := renameReqNum
  renameTable.io.renameWritePort.prIdx := freeList.io.allocatePhyReg

  io.rename.zipWithIndex.foreach {
    case (port, i) => {
      port.out.bits := port.in.bits
      port.in.ready := (i.U < canAllocateNum) && port.out.ready
      port.out.valid := port.in.valid && (i.U < canAllocateNum)
    }
  }

  //read RAT
  for((rdp, rp) <- renameTable.io.renameReadPorts.zip(io.rename)) {
    rdp.vd.lrIdx    := rp.in.bits.ctrl.ldest
    rdp.vs1.lrIdx   := rp.in.bits.ctrl.lsrc(0)
    rdp.vs2.lrIdx   := rp.in.bits.ctrl.lsrc(1)
  }

  for((port, rdp) <- io.rename.zip(renameTable.io.renameReadPorts)) {
    val srcType = port.in.bits.ctrl.srcType
    port.out.bits.psrc(0) := Mux(srcType(0)===SrcType.vec, rdp.vs1.prIdx, port.in.bits.psrc(0))
    port.out.bits.psrc(1) := Mux(srcType(1)===SrcType.vec, rdp.vs2.prIdx, port.in.bits.psrc(1))
    port.out.bits.pdest := Mux(port.in.bits.canRename, rdp.vd.prIdx, port.in.bits.pdest)
    port.out.bits.vm := rdp.vmask
  }

  //read old value
  for((oldRdp, rp) <- renameTable.io.oldPhyRegIdxReadPorts.zip(io.rename)) {
    oldRdp.lrIdx := rp.in.bits.ctrl.ldest
  }

  //write RAT
  val ratRenamePortW = renameTable.io.renameWritePort
  ratRenamePortW.doRename := doRename
  ratRenamePortW.mask     := VecInit(io.rename.map(_.in.fire())).asUInt
  ratRenamePortW.lrIdx    := io.rename.map(_.in.bits.ctrl.ldest)
  ratRenamePortW.prIdx    := freeList.io.allocatePhyReg

  //write roll back list
  for((wp, i) <- rollBackList.io.rename.writePorts.zipWithIndex) {
    wp.bits.lrIdx    := io.rename(i).in.bits.ctrl.ldest
    wp.bits.newPrIdx := freeList.io.allocatePhyReg(i)
    wp.bits.oldPrIdx := renameTable.io.oldPhyRegIdxReadPorts(i).prIdx
    wp.bits.robIdx   := io.rename(i).in.bits.robIdx.value
  }

  for((port, i) <- rollBackList.io.rename.writePorts.zipWithIndex) {
    port.valid := io.rename(i).in.bits.canRename
  }
  //-------------------------------------------- TODO: commit & walk --------------------------------------------

  rollBackList.io.commit.rob <> io.commit
  renameTable.io.commitPort <> rollBackList.io.commit.rat

  for((rls, i) <- freeList.io.releasePhyReg.zipWithIndex) {
    rls.valid := rollBackList.io.commit.rat.mask(i)
    rls.bits := Mux(rollBackList.io.commit.rat.doCommit, rollBackList.io.commit.rat.prIdxOld(i), rollBackList.io.commit.rat.prIdxNew(i))
  }
}
