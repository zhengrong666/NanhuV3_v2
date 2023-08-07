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

class MemDispatchArbiter(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle {
        val memIn = Flipped(DecoupledIO(new MicroOp))
        val vmemIn = Flipped(DecoupledIO(new MicroOp))
        val toMem2RS = DecoupledIO(new MicroOp)
    })
    
    val s_mem :: s_vmem :: Nil = Enum(2)
    val arbState = RegInit(s_mem)

    io.memIn.ready := (arbState === s_mem) && io.toMem2RS.ready
    io.vmemIn.ready := (arbState === s_vmem) && io.toMem2RS.ready

    val vRobIdx = Reg(new RobPtr)
    val vRobIdxValid = RegInit(Bool(), false.B)

    when(arbState === s_mem && io.memIn.bits.fire() && io.memIn.bits.ctrl.isVector) {
        arbState := s_vmem
        vRobIdx := io.memIn.bits.robIdx
    }

    when(arbState === s_vmem && io.vmemIn.fire() && (vRobIdx === io.vmemIn.bits.robIdx) && (io.vmemIn.bits.uopIdx + 1.U) === io.vmemIn.bits.uopNum) {
        arbState := s_mem
    }

    io.toMem2RS.bits := Mux(arbState === s_mem, io.memIn.bits, io.vmemIn.bits)
    io.toMem2RS.valid := Mux(arbState === s_mem, Mux(!io.memIn.bits.isVector, io.memIn.valid, false.B), io.vmemIn.valid)
}
