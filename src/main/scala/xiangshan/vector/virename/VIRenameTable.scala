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
import org.chipsalliance.cde.config.Parameters

import xiangshan._
import utils._
import xs.utils.CircularShift

import xiangshan.vector._

class VIRatRenameIO(implicit p: Parameters) extends VectorBaseBundle {
  val in = Flipped(ValidIO(new Bundle {
    val lvd = UInt(5.W)
    val lvs1 = UInt(5.W)
    val lvs2 = UInt(5.W)
    val lvs3 = UInt(5.W)
    val doRename = Bool()
    val allocIdx = UInt(VIPhyRegIdxWidth.W)
  }))
  val out = Output(new Bundle {
    val pvs1 = UInt(VIPhyRegIdxWidth.W)
    val pvs2 = UInt(VIPhyRegIdxWidth.W)
    val pvs3 = UInt(VIPhyRegIdxWidth.W)
    val pmask = UInt(VIPhyRegIdxWidth.W)
  })
}

class VIRatCommitPort(implicit p: Parameters) extends VectorBaseBundle {
  val doCommit    = Bool()
  val doWalk      = Bool()
  val mask        = Vec(8, Bool())
  val lrIdx       = Vec(8, UInt(5.W))
  val prIdxOld    = Vec(8, UInt(VIPhyRegIdxWidth.W))
  val prIdxNew    = Vec(8, UInt(VIPhyRegIdxWidth.W))

  def Pipe:VIRatCommitPort = {
    val pipe = Wire(new VIRatCommitPort)
    pipe.doCommit := RegNext(this.doCommit, false.B)
    pipe.doWalk := RegNext(this.doCommit, false.B)
    for(i <- 0 until 8){
      pipe.mask(i) := RegNext(this.mask(i), false.B)
      pipe.lrIdx(i) := RegEnable(this.lrIdx(i), this.mask(i))
      pipe.prIdxOld(i) := RegEnable(this.prIdxOld(i), this.mask(i))
      pipe.prIdxNew(i) := RegEnable(this.prIdxNew(i), this.mask(i))
    }
    pipe
  }
}

class VIRenameTable(implicit p: Parameters) extends VectorBaseModule {
  val io = IO(new Bundle{
    val rename = Vec(VIRenameWidth, new VIRatRenameIO)
    val commit      = Input(new VIRatCommitPort)
    val debug  = Output(Vec(32, UInt(VIPhyRegIdxWidth.W))) //for difftest
  })
  //RAT
  val rat_ds = VecInit.tabulate(32)(i => i.U(VIPhyRegIdxWidth.W))
  val sRAT = RegInit(rat_ds)
  val aRAT = RegInit(rat_ds)

  io.debug := aRAT

  for(((pi, po), bypassNum) <- io.rename.map(_.in).zip(io.rename.map(_.out)).zipWithIndex) {
    po.pvs1 := sRAT(pi.bits.lvs1)
    po.pvs2 := sRAT(pi.bits.lvs2)
    po.pvs3 := sRAT(pi.bits.lvs3)
    po.pmask := sRAT(0)
    if(bypassNum != 0) {
      val renamePorts = io.rename.take(bypassNum)
      val wmasks = renamePorts.map(_.in.valid)
      val wlrs = renamePorts.map(_.in.bits.lvd)
      val wprs = renamePorts.map(_.in.bits.allocIdx)
      Seq(pi.bits.lvs1, pi.bits.lvs2, pi.bits.lvs3, 0.U).zip(
        Seq(po.pvs1, po.pvs2, po.pvs3, po.pmask)
      ).foreach {
        case(lr, pr) => {
          for(i <- 0 until bypassNum) {
            val hit = wmasks(i) && (wlrs(i) === lr && renamePorts(i).in.bits.doRename)
            when(hit) {
              pr := wprs(i)
            }
          }
        }
      }
    }
  }

  //old regId read, for rollBackList storage
  private val sRatRenameNext = WireInit(sRAT)
  private val sRatWalkNext = WireInit(sRAT)

  io.rename.foreach(r => {
    when(r.in.valid){
      sRatRenameNext(r.in.bits.lvd) := r.in.bits.allocIdx
    }
  })

  for (i <- 0 until 8) {
    when(io.commit.doWalk && io.commit.mask(i)) {
      sRatWalkNext(io.commit.lrIdx(i)) := io.commit.prIdxOld
    }
  }
  private val doRename = io.rename.map(_.in.valid).reduce(_|_)
  private val doWalk = io.commit.doWalk && io.commit.mask.reduce(_|_)
  when(doRename){
    sRAT := sRatRenameNext
  }.elsewhen(doWalk){
    sRAT := sRatWalkNext
  }

  for(i <- 0 until 8){
    when(io.commit.doCommit && io.commit.mask(i)){
      aRAT(io.commit.lrIdx(i)) := io.commit.prIdxNew
    }
  }
}
