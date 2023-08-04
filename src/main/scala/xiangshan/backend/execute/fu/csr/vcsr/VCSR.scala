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
    Date: 2023-07-27
    email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.backend.execute.fu.csr.vcsr

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import utils._
import xiangshan._

import xiangshan.vector._
import utility.MaskedRegMap
import xiangshan.backend.execute.fu.FUWithRedirect

class VCSRInfo(implicit p: Parameters) extends VectorBaseBundle with HasVCSRConst {
    val vstart  = UInt(XLEN.W)
    val vxsat   = UInt(XLEN.W)
    val vxrm    = UInt(XLEN.W)
    val vcsr    = UInt(XLEN.W)
    val vl      = UInt(XLEN.W)
    val vlenb   = UInt(XLEN.W)
}

class VCSR(implicit p: Parameters) extends FUWithRedirect with HasVCSRConst {
    val cfIn = io.in.bits.uop.cf
    val cfOut = Wire(new CtrlFlow)

    //csr define
    val vtype   = RegInit(UInt(XLEN.W), 0.U)
    val vstart  = RegInit(UInt(XLEN.W), 0.U)
    val vxsat   = RegInit(UInt(XLEN.W), 0.U)
    val vxrm    = RegInit(UInt(XLEN.W), 0.U)
    val vcsr    = RegInit(UInt(XLEN.W), 0.U)
    val vl      = RegInit(UInt(XLEN.W), 0.U)
    val vlenb   = RegInit(UInt(XLEN.W), 0.U)

    val vectorCSRMapping = Map(
        MaskedRegMap(vstartAddr, vstart),
        MaskedRegMap(vxsatAddr, vxsat),
        MaskedRegMap(vxrmAddr, vxrm),
        MaskedRegMap(vcsrAddr, vcsr),
        MaskedRegMap(vlAddr, vl),
        MaskedRegMap(vlenbAddr, vlenb)
    )

    val otherType :: vsetvlType :: vsetvliType :: vsetivliType :: Nil = Enum(4)
    val instrType = WireInit(otherType)
    val typeHitVec = Wire(Vec(3, Bool()))
    
    //vsetvl
    typeHitVec(0) := cfIn.instr(31, 30) === "b10".U
    //vsetvl{i}
    typeHitVec(1) := cfIn.instr(31) === "b0".U
    //vset{i}vl{i}
    typeHitVec(2) := cfIn.instr(31, 30) === "b11".U


    //src1 -> AVL, src2 -> 
    val rs1 = io.in.bits.uop.ctrl.lsrc(0)
    val rd = io.in.bits.uop.ctrl.ldest

    val valid = io.in.valid
    val avl = io.in.bits.src(0)
    val vtypeNew = Mux(typeHitVec(0), io.in.bits.src(1), io.in.bits.uop.ctrl.imm)
}
