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

package xiangshan.vector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import xiangshan._
import utils._

//TODO: Vector Micro OP interface align
//fake interface
class VectorMicroOP(implicit p: Parameters) extends VectorBaseBundle {
    def typeJudge(sel_type: UInt): Bool = sel_type === 1.U
}

class VIRenameReq(implicit p: Parameters) extends VectorBaseBundle {
    val lvs1    = UInt(5.W)
    val lvs2    = UInt(5.W)
    val lvd     = UInt(5.W)
    //TODO: WaitQueue port aligning...
    val en      = Bool()
}

class VIWaitQueueToRenameBundle(implicit p: Parameters) extends VectorBaseBundle {
    val renameReq   = Input(Vec(VIRenameWidth, new VIRenameReq))
    val robIdx      = Input(Vec(VIRenameWidth, UInt(log2Up(RobSize).W)))
    val needRename = Input(Vec(VIRenameWidth, Bool()))
    val vtypeValue  = Input(UInt(64.W))
    val doRename = Output(Bool())
}

class VICtrlFlow(implicit p: Parameters) extends VectorBaseBundle {
    val cf = new CtrlFlow
    val funct6 = UInt(6.W)
    val funct3 = UInt(3.W)
    val vm = UInt(1.W)
    val vs1_imm = UInt(5.W)
    val widen = Bool()
    val widen2 = Bool()
    val narrow = Bool()
    val narrow_to_1 = Bool()
}

class VICsrInfo(implicit p: Parameters) extends VectorBaseBundle {
    val ma = UInt(1.W)
    val ta = UInt(1.W)
    val vsew = UInt(3.W)
    val vlmul = UInt(3.W)
    val vl = UInt(8.W)
    val vstart = UInt(7.W)
    val vxrm = UInt(2.W)
    val frm = UInt(3.W)
}

class VICtrl(implicit p: Parameters) extends VectorBaseBundle {
    val vicf = new VICtrlFlow
    val viinfo = new VICsrInfo
    val visignal = new CtrlSignals
    val vs1 = UInt(128.W)
    val vs2 = UInt(128.W)
    val rs1 = UInt(64.W)
    val oldvd = UInt(128.W)
    val mask = UInt(128.W)
    val vd = UInt(128.W)
    val vxsat = UInt(1.W)
    val fflags = UInt(5.W)
}
