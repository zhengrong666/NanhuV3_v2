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
import xs.utils._
import xiangshan.backend.execute.fu.csr._
import xiangshan.backend.execute.fu.FuOutput

class VCSRInfo(implicit p: Parameters) extends VectorBaseBundle with HasVCSRConst {
    val vtype   = UInt(XLEN.W)
    val vstart  = UInt(XLEN.W)
    val vxsat   = UInt(XLEN.W)
    val vxrm    = UInt(XLEN.W)
    val vcsr    = UInt(XLEN.W)
    val vl      = UInt(XLEN.W)
    val vlenb   = UInt(XLEN.W)
}

class VCSRWIO(implicit p: Parameters) extends VectorBaseBundle with HasVCSRConst {
    val vstartW = Flipped(ValidIO(UInt(XLEN.W)))
    val vxsatW  = Flipped(ValidIO(UInt(XLEN.W)))
}

class VCSR(implicit p: Parameters) extends FUWithRedirect with HasVCSRConst {
    val cfIn = io.in.bits.uop.cf
    //val cfOut = Wire(new CtrlFlow)
    redirectOutValid := DontCare
    redirectOut := DontCare

    val vcsr_io = IO(new Bundle {
        val vcsrInfo = Output(new VCSRInfo)
        val wbFromRob = new VCSRWIO
        val vtypeWb = ValidIO(new FuOutput(64))
        val wen = Input(Bool())
    })

    //csr define
    val vtype   = RegInit(UInt(XLEN.W), 0.U)
    val vstart  = RegInit(UInt(XLEN.W), 0.U)
    val vxsat   = RegInit(UInt(XLEN.W), 0.U)
    val vxrm    = RegInit(UInt(XLEN.W), 0.U)
    val vcsr    = RegInit(UInt(XLEN.W), 0.U)
    val vl      = RegInit(UInt(XLEN.W), 0.U)
    val vlenb   = RegInit(UInt(XLEN.W), (VLEN / 8).U)

    vcsr_io.vcsrInfo.vtype  := vtype
    vcsr_io.vcsrInfo.vstart := vstart
    vcsr_io.vcsrInfo.vxsat  := vxsat
    vcsr_io.vcsrInfo.vxrm   := vxrm
    vcsr_io.vcsrInfo.vcsr   := vcsr
    vcsr_io.vcsrInfo.vl     := vl
    vcsr_io.vcsrInfo.vlenb  := vlenb

    val wbFromRob = vcsr_io.wbFromRob
    vstart := Mux(wbFromRob.vstartW.valid, wbFromRob.vstartW.bits, vstart)
    vxsat := Mux(wbFromRob.vxsatW.valid, wbFromRob.vxsatW.bits, vstart)
    
    //vsetvl
    val isVsetvl = cfIn.instr(31, 30) === "b10".U
    //vsetvl{i}
    val isVsetvli = cfIn.instr(31) === "b0".U
    //vset{i}vl{i}
    val isVsetivli = cfIn.instr(31, 30) === "b11".U

    //src1 -> AVL, src2 ->
    val valid = io.in.valid
    val rs1 = io.in.bits.uop.ctrl.lsrc(0)
    val rs1IsX0 = (rs1 === 0.U)
    val rd = io.in.bits.uop.ctrl.ldest
    val rdIsX0 = (rd === 0.U)

    val src0 = io.in.bits.src(0)
    val src1 = io.in.bits.src(1)
    val imm = io.in.bits.uop.ctrl.imm

    val func = io.in.bits.uop.ctrl.fuOpType

    //vtype
    val vtypeValue = Wire(UInt(8.W))
    val vtypei = Mux(isVsetvli, imm(7, 0), (imm >> 5.U)(7, 0))
    vtypeValue := Mux(isVsetvl, src1(7,0), vtypei)

    val vlmul = vtypeValue(2, 0)
    val vsew = vtypeValue(5, 3)

    //vsew inside {3'b000, 3'b001, 3'b010, 3'b011}, vsew[2] == 0
    //LMUL = 2 ^ signed(vlmul)
    //SEW = 2 ^ vsew * 8
    //VLMAX = VLEN * LMUL / SEW = VLEN * (LMUL / SEW) = VLEN * 2 ^ (signed(vlmul) - vsew) / 8, (vsew[2] == 0) => signed(vlmul) - vsew = signed(vlmul) - signed(vsew))
    val vlmul_tmp = Mux(vlmul(2)===0.U, Cat(vlmul(2), 0.U(1.W), vlmul(1, 0)), Cat(vlmul(2), ~Cat(0.U(1.W), vlmul(1, 0)) + 1.U))
    val vsew_neg_tmp = Cat(1.U(1.W), ~(vsew+3.U)) + 1.U
    val vlmax_sel = vlmul + vsew_neg_tmp + 7.U

    val vlmax_vec = (0 until 8).map(i => 1.U(8.W) << i)
    val vlmax = ParallelMux((0 until 8).map(_.U).map(_===vlmax_sel), vlmax_vec)

    val avl = Mux(isVsetivli, Cat(0.U(59.W), imm(4, 0)), src0)
    val vlNewSetivli = Mux(vlmax >= avl, avl, vlmax)
    val vlNewOther = Mux(!rs1IsX0, Mux(src0 >= vlmax, vlmax, src0), Mux(!rdIsX0, vlmax, vl))
    vl := Mux(isVsetivli, vlNewSetivli, vlNewOther)

    val vill = 0.U(1.W) //TODO: set vill -> illegal vtype set
    vtype := Cat(vill, 0.U(55.W), vtypeValue)

    //val vtypeWb = Cat(vill, vtypeValue)
    vcsr_io.vtypeWb.bits.data := Cat(vill, 0.U(55.W), vtypeValue)
    vcsr_io.vtypeWb.bits.uop := io.in.bits.uop
    vcsr_io.vtypeWb.valid := (isVsetivli || isVsetvl || isVsetvli) && io.in.fire()
    // vcsr_io.vtypeWb.valid := io.in.valid

    val vectorCSRMapping = Map(
        MaskedRegMap(vstartAddr, vstart),
        MaskedRegMap(vxsatAddr, vxsat),
        MaskedRegMap(vxrmAddr, vxrm),
        MaskedRegMap(vcsrAddr, vcsr),
        MaskedRegMap(vlAddr, vl),
        MaskedRegMap(vlenbAddr, vlenb)
    )
    val addr = imm(11, 0)
    val rdata = Wire(UInt(XLEN.W))
    val csri = ZeroExt(imm(16, 12), XLEN)
    val wdata = LookupTree(func, List(
        CSROpType.wrt  -> src1,
        CSROpType.set  -> (rdata | src0),
        CSROpType.clr  -> (rdata & (~src0).asUInt),
        CSROpType.wrti -> csri,
        CSROpType.seti -> (rdata | csri),
        CSROpType.clri -> (rdata & (~csri).asUInt)
    ))
    MaskedRegMap.generate(vectorCSRMapping, addr, rdata, vcsr_io.wen, wdata)

    io.in.ready := true.B
    
    io.out.bits.uop := io.in.bits.uop
    io.out.bits.data := Mux((isVsetivli || isVsetvl || isVsetvli), Cat(vill, 0.U(55.W), vtypeValue), rdata)
    io.out.valid := io.in.valid

}
