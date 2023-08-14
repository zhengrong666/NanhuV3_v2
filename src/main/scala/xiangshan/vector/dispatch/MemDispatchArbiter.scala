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
    Date: 2023-08-06
    email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.vector.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import utils._
import xiangshan._
import xiangshan.mem.mdp._

import xiangshan.vector._
import xiangshan.backend.rob._

class MemDispatchArbiter(arbWidth: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle {
        val memIn = Vec(arbWidth, Flipped(DecoupledIO(new MicroOp)))
        val vmemIn = Vec(arbWidth, Flipped(DecoupledIO(new MicroOp)))
        val toMem2RS = Vec(arbWidth, DecoupledIO(new MicroOp))
    })
    
    val s_mem :: s_vmem :: Nil = Enum(2)
    val arbState = RegInit(s_mem)

    val memCanDeqVec = Wire(Vec(arbWidth, Bool()))
    val hasVVec = Wire(Vec(arbWidth, Bool()))
    val vmemCanDeqVec = Wire(Vec(arbWidth, Bool()))
    val memDeqNum = PopCount(io.memIn.map(_.fire()))
    val vmemDeqNum = PopCount(io.vmemIn.map(_.fire()))

    val vRobIdx = Reg(new RobPtr)
    val vRobIdxValid = RegInit(Bool(), false.B)

    memCanDeqVec(0) := io.memIn(0).valid && (!io.memIn(0).bits.ctrl.isVector)
    for(i <- 1 until arbWidth) {
        memCanDeqVec(i) := io.memIn(i).valid && (!io.memIn(i).bits.ctrl.isVector) && memCanDeqVec(i-1)
    }

    vmemCanDeqVec(0) := io.vmemIn(0).valid
    for(i <- 1 until arbWidth) {
        vmemCanDeqVec(i) := io.vmemIn(i).valid && vmemCanDeqVec(i-1) && (io.vmemIn(i).bits.robIdx === vRobIdx)
    }
    for((v, port) <- hasVVec.zip(io.memIn)) {
        v := port.bits.ctrl.isVector
    }

    for((in, i) <- io.memIn.zipWithIndex) {
        in.ready := (arbState === s_mem) && io.toMem2RS(i).ready && memCanDeqVec(i) || ((i.U === PopCount(memCanDeqVec)) && hasVVec.asUInt.orR)
    }

    for((in, out) <- io.vmemIn.zip(io.toMem2RS)) {
        in.ready := (arbState === s_vmem) && out.ready
    }

    

    when(arbState === s_mem && io.memIn(0).fire() && io.memIn(0).bits.ctrl.isVector) {
        arbState := s_vmem
        vRobIdx := io.memIn(0).bits.robIdx
    }

    when(arbState === s_vmem) {
        arbState := Mux((io.vmemIn(vmemDeqNum).bits.uopIdx + 1.U) === io.vmemIn(vmemDeqNum).bits.uopNum, s_mem, s_vmem)
    }

    for(i <- 0 until arbWidth) {
        io.toMem2RS(i).bits := Mux(arbState === s_mem, io.memIn(i).bits, io.vmemIn(i).bits)
        io.toMem2RS(i).valid := Mux(arbState === s_mem, memCanDeqVec(i), vmemCanDeqVec(i))
    }
}
