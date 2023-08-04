/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan.vector.videcode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{BitPat, _}
import freechips.rocketchip.util.uintToBitPat
import xs.utils.{SignExt}
import xiangshan._
import xiangshan.vector._
import freechips.rocketchip.rocket.Instructions._

abstract trait DecodeConstants {
  // This X should be used only in 1-bit signal. Otherwise, use->List(SrcType.fp,  SrcType.imm, SrcType.vec, FuType.fmisc, FuOpType.X, N, Y, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
    def X = BitPat("b?")
    def N = BitPat("b0")
    def Y = BitPat("b1")

   def decodeDefault: List[BitPat] = // illegal instruction
  //   srcType(0) srcType(1) srcType(2) fuType    fuOpType    rfWen
  //   |          |          |          |         |           |  fpWen
  //   |          |          |          |         |           |  |  vdWen
  //   |          |          |          |         |           |  |  |  isorder
  //   |          |          |          |         |           |  |  |  |  Widen
  //   |          |          |          |         |           |  |  |  |  |  Narrow
  //   |          |          |          |         |           |  |  |  |  |  |  selImm
  //   |          |          |          |         |           |  |  |  |  |  |  |
    List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.X, FuOpType.X, N, N, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.INVALID_INSTR) // Use SelImm to indicate invalid instr

    val table: Array[(BitPat, List[BitPat])]
}

trait DecodeUnitConstants
{
  // abstract out instruction decode magic numbers
    val VD_MSB = 11
    val VD_LSB = 7
    val VS1_MSB = 19
    val VS1_LSB = 15
    val VS2_MSB = 24
    val VS2_LSB = 20
    val F6_MSB = 31
    val F6_LSB = 26
    val F3_MSB = 14
    val F3_LSB = 12
    val VM_LSB = 25
    val NF_MSB = 31
    val NF_LSB = 29
}

object VectorArithDecode extends DecodeConstants {
    val table: Array[(BitPat, List[BitPat])] = Array(

        //arithmetic instruction
        VADC_VIM ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VADC_VVM ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VADC_VXM ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VADD_VI ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VADD_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VADD_VX ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VAND_VI ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VAND_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VAND_VX ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSLL_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSLL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSLL_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSRA_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSRA_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSRA_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSRL_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSRL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSRL_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSUB_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),

        VMAX_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMAX_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMAXU_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMAXU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMERGE_VIM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMERGE_VVM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMERGE_VXM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMIN_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMIN_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMINU_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMINU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),

        VMV1R_V -> List(SrcType.vec, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMV2R_V -> List(SrcType.vec, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMV4R_V -> List(SrcType.vec, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMV8R_V -> List(SrcType.vec, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMV_S_X -> List(SrcType.reg, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMV_V_I -> List(SrcType.imm, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VMV_V_V -> List(SrcType.vec, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMV_V_X -> List(SrcType.reg, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMV_X_S -> List(SrcType.vec, SrcType.X, SrcType.X, FuType.valu, FuOpType.X, Y, N, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VOR_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VOR_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VOR_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSBC_VVM -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSBC_VXM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSEXT_VF2 -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSEXT_VF4 -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSEXT_VF8 -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VXOR_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VXOR_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VXOR_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VZEXT_VF2 -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VZEXT_VF4 -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VZEXT_VF8 -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),

        // fixed-point Instruction
        VASUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VASUB_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VASUBU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VASUBU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VAADD_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VAADD_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VAADDU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VAADDU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSRA_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSSRA_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSRA_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSRL_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSSRL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSRL_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSUB_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSUBU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSUBU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSADD_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSADD_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSADD_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSADDU_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSADDU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSADDU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),


        //mac
        VMUL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMUL_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMULH_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMULH_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMULHSU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMULHSU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMULHU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMULHU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMACC_VV -> List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMACC_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VNMSAC_VV -> List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VNMSAC_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMADD_VV -> List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMADD_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VNMSUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VNMSUB_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSMUL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSMUL_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),


        //float
        VFADD_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFADD_VV ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFCLASS_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFCVT_F_X_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFCVT_F_XU_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFCVT_RTZ_X_F_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFCVT_RTZ_XU_F_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFCVT_X_F_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFCVT_XU_F_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMACC_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMACC_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMADD_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMADD_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMAX_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMAX_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMERGE_VFM ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMIN_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMIN_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMSAC_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMSAC_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMSUB_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMSUB_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMUL_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMUL_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMV_F_S ->List(SrcType.vec,  SrcType.vec, SrcType.reg, FuType.vfp, FuOpType.X, N, Y, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMV_S_F ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFMV_V_F ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMACC_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMACC_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMADD_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMADD_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMSAC_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMSAC_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMSUB_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFNMSUB_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFREC7_V ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),

        //reduction-float
        VFREDMAX_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFREDMIN_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFREDOSUM_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFREDUSUM_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),

        VFRSQRT7_V ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFRSUB_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSGNJ_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSGNJ_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSGNJN_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSGNJN_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSGNJX_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSGNJX_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSUB_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSUB_VV ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),


        //div
        VFDIV_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFDIV_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFRDIV_VF ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSQRT_V ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VDIV_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VDIV_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VDIVU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VDIVU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREM_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREM_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREMU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREMU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vdiv, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),

        //mask
        VMXNOR_MM ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMXOR_MM ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMAND_MM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMANDN_MM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMNAND_MM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMNOR_MM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMOR_MM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMORN_MM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFIRST_M -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, Y, N, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VCPOP_M -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, Y, N, N, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMSBF_M -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMSIF_M -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VMSOF_M -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VID_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VIOTA_M -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmask, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),


        //reduction
        VREDAND_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREDMAX_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREDMAXU_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREDMIN_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREDMINU_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREDOR_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREDSUM_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VREDXOR_VS ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VRSUB_VI ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VRSUB_VX ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),


        //permutation instructions
        VCOMPRESS_VM ->List(SrcType.vec,  SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSLIDE1DOWN_VX ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSLIDE1UP_VX ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSLIDEDOWN_VI ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSLIDEDOWN_VX ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSLIDEUP_VI ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VSLIDEUP_VX ->List(SrcType.reg,  SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSLIDE1DOWN_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VFSLIDE1UP_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VRGATHER_VI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VA),
        VRGATHER_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VRGATHER_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VRGATHEREI16_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vpermu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),


        //narrowing
        VNSRA_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.IMM_VA),
        VNSRA_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VNSRA_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VNSRL_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.IMM_VA),
        VNSRL_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VNSRL_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VNCLIP_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, N, Y, SelImm.IMM_VA),
        VNCLIP_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, N, Y, SelImm.X),
        VNCLIP_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, N, Y, SelImm.X),
        VNCLIPU_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, N, Y, SelImm.IMM_VA),
        VNCLIPU_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, N, Y, SelImm.X),
        VNCLIPU_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, N, Y, SelImm.X),
        VFNCVT_F_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VFNCVT_F_X_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VFNCVT_F_XU_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VFNCVT_ROD_F_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VFNCVT_RTZ_X_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VFNCVT_RTZ_XU_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VFNCVT_X_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VFNCVT_XU_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),

        VMADC_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMADC_VIM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMADC_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMADC_VVM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMADC_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMADC_VXM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSBC_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSBC_VVM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSBC_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSBC_VXM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),

        VMSEQ_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSEQ_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSEQ_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSGT_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSGT_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSGTU_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSGTU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLE_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLE_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLE_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLEU_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLEU_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLEU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLT_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLT_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLTU_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSLTU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSNE_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSNE_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMSNE_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFEQ_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFEQ_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFGE_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFGT_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFLE_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFLE_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFLT_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFLT_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFNE_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),
        VMFNE_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, N, Y, SelImm.X),


        //widening
        VWADD_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWADD_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWADD_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWADD_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWADDU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWADDU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWADDU_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWADDU_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUB_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUB_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUB_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUBU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUBU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUBU_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWSUBU_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMUL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMUL_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMULSU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMULSU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMULU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMULU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMACC_VV -> List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMACC_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMACCSU_VV -> List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.valu, FuOpType.X, N, Y, N, N, Y, N, SelImm.X),
        VWMACCSU_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.valu, FuOpType.X, N, Y, N, N, Y, N, SelImm.X),
        VWMACCU_VV -> List(SrcType.vec, SrcType.vec, SrcType.vec, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMACCU_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VWMACCUS_VX -> List(SrcType.reg, SrcType.vec, SrcType.vec, FuType.valu, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),

        VFWADD_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWADD_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWADD_WF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWADD_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWSUB_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWSUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWSUB_WF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWSUB_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWMUL_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWMUL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWMACC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWMACC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWNMACC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWNMACC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWMSAC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWMSAC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWNMSAC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWNMSAC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWCVT_F_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWCVT_F_X_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWCVT_F_XU_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWCVT_RTZ_X_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWCVT_RTZ_XU_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWCVT_X_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),
        VFWCVT_XU_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Y, N, SelImm.X),

        VWREDSUM_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Y, N, SelImm.X),
        VWREDSUMU_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Y, N, SelImm.X),
        VFWREDOSUM_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Y, N, SelImm.X),
        VFWREDUSUM_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Y, N, SelImm.X),

        )
    }

object VectorLoadDecode extends DecodeConstants {
    val table: Array[(BitPat, List[BitPat])] = Array(

        VL1RE16_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL1RE32_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL1RE64_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL1RE8_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL2RE16_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL2RE32_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL2RE64_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL2RE8_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL4RE16_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL4RE32_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL4RE64_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL4RE8_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL8RE16_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL8RE32_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL8RE64_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VL8RE8_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE16_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE16FF_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE32_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE32FF_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE64_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE64FF_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE8_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLE8FF_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLM_V ->List(SrcType.imm,  SrcType.fp, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VL),
        VLOXEI16_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLOXEI32_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLOXEI64_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLOXEI8_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLSE16_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLSE32_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLSE64_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLSE8_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLUXEI16_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLUXEI32_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLUXEI64_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VLUXEI8_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.ldu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),

    )
}

object VectorStoreDecode extends DecodeConstants {
    val table: Array[(BitPat, List[BitPat])] = Array(

        VSE16_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSE32_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSE64_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSE8_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSM_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSOXEI16_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSOXEI32_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSOXEI64_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSOXEI8_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSE16_V ->List(SrcType.fp,  SrcType.fp, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSE32_V ->List(SrcType.fp,  SrcType.fp, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSE64_V ->List(SrcType.fp,  SrcType.fp, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSSE8_V ->List(SrcType.fp,  SrcType.fp, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSUXEI16_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSUXEI32_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSUXEI64_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSUXEI8_V ->List(SrcType.fp,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VS1R_V ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VS),
        VS2R_V ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VS),
        VS4R_V ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VS),
        VS8R_V ->List(SrcType.imm,  SrcType.vec, SrcType.X, FuType.stu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_VS),

    )
}

object VectorConfDecode extends DecodeConstants {
    val table: Array[(BitPat, List[BitPat])] = Array(

        VSETIVLI ->List(SrcType.imm,  SrcType.imm, SrcType.X, FuType.csr, FuOpType.X, N, Y, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_CI),
        VSETVL ->List(SrcType.reg,  SrcType.reg, SrcType.X, FuType.csr, FuOpType.X, N, Y, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.X),
        VSETVLI ->List(SrcType.imm,  SrcType.reg, SrcType.X, FuType.csr, FuOpType.X, N, Y, N, N, Widen.NotWiden, Narrow.NotNarrow, SelImm.IMM_C),

    )
}

object VectorWidenDecode extends DecodeConstants {
    val table: Array[(BitPat, List[BitPat])] = Array(

        //widening
        VWADD_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWADD_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWADD_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWADD_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWADDU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWADDU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWADDU_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWADDU_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWSUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWSUB_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWSUB_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWSUB_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWSUBU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWSUBU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWSUBU_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWSUBU_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWMUL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMUL_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMULSU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMULSU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMULU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMULU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMACC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMACC_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMACCSU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, Y, N, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMACCSU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, Y, N, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMACCU_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMACCU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VWMACCUS_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),

        VFWADD_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWADD_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWADD_WF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VFWADD_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VFWSUB_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWSUB_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWSUB_WF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VFWSUB_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VFWMUL_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWMUL_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWMACC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWMACC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWNMACC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWNMACC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWMSAC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWMSAC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWNMSAC_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWNMSAC_VV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWCVT_F_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWCVT_F_X_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWCVT_F_XU_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWCVT_RTZ_X_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWCVT_RTZ_XU_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWCVT_X_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWCVT_XU_F_V -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.Widen, Narrow.NotNarrow, SelImm.X),

        VWREDSUM_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VWREDSUMU_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vreduc, FuOpType.X, N, N, Y, Y, Widen.Widen2, Narrow.NotNarrow, SelImm.X),
        VFWREDOSUM_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Widen.Widen, Narrow.NotNarrow, SelImm.X),
        VFWREDUSUM_VS -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, Y, Widen.Widen, Narrow.NotNarrow, SelImm.X),

    )
}

object VectorNarrowDecode extends DecodeConstants {
    val table: Array[(BitPat, List[BitPat])] = Array(

        //narrowing
        VNSRA_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.IMM_VA),
        VNSRA_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VNSRA_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VNSRL_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.IMM_VA),
        VNSRL_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VNSRL_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VNCLIP_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.IMM_VA),
        VNCLIP_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VNCLIP_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VNCLIPU_WI -> List(SrcType.imm, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.IMM_VA),
        VNCLIPU_WV -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VNCLIPU_WX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_F_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_F_X_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_F_XU_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_ROD_F_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_RTZ_X_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_RTZ_XU_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_X_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),
        VFNCVT_XU_F_W -> List(SrcType.vec, SrcType.vec, SrcType.X, FuType.vfp, FuOpType.X, N, N, Y, N, Widen.NotWiden, Narrow.Narrow, SelImm.X),

        VMADC_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMADC_VIM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMADC_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMADC_VVM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMADC_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMADC_VXM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSBC_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSBC_VVM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSBC_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSBC_VXM -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),

        VMSEQ_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSEQ_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSEQ_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSGT_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSGT_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSGTU_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSGTU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLE_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLE_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLE_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLEU_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLEU_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLEU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLT_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLT_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLTU_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSLTU_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSNE_VI -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSNE_VV -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMSNE_VX -> List(SrcType.reg, SrcType.vec, SrcType.X, FuType.valu, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFEQ_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFEQ_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFGE_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFGT_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFLE_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFLE_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFLT_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFLT_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFNE_VF -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X),
        VMFNE_VV -> List(SrcType.fp, SrcType.vec, SrcType.X, FuType.vmac, FuOpType.X, N, N, Y, Y, Widen.NotWiden, Narrow.Narrow2, SelImm.X)
    )
}


abstract class Imm(val len: Int) extends Bundle {
    def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
    def do_toImm32(minBits: UInt): UInt
    def minBitsFromInstr(instr: UInt): UInt
}

case class Imm_I() extends Imm(5) {
    override def do_toImm32(minBits: UInt): UInt = SignExt(minBits(len - 1, 0), 32)
    override def minBitsFromInstr(instr: UInt): UInt = Cat(instr(24, 20))
}

case class Imm_L() extends Imm(5) {
    override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)
    override def minBitsFromInstr(instr: UInt): UInt = Cat(instr(24, 20))
}

case class Imm_S() extends Imm(5) {
    override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)
    override def minBitsFromInstr(instr: UInt): UInt = Cat(instr(24, 20))
}

case class Imm_C() extends Imm(11) {
    override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

    override def minBitsFromInstr(instr: UInt): UInt = Cat(instr(30, 20))
}

case class Imm_CI() extends Imm(16) {
    override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)
    override def minBitsFromInstr(instr: UInt): UInt = Cat(instr(29, 20), instr(19,15))
}

object ImmUnion {
    val VA = Imm_I()
    val VL = Imm_L()
    val VS = Imm_S()
    val VC = Imm_C()
    val VCI = Imm_CI()

    val imms = Seq(VA, VL, VS, VC, VCI)
    val maxLen = imms.maxBy(_.len).len
    val immSelMap = Seq(
        SelImm.IMM_VA,
        SelImm.IMM_VL,
        SelImm.IMM_VS,
        SelImm.IMM_C,
        SelImm.IMM_CI,
    ).zip(imms)
    println(s"ImmUnion max len: $maxLen")
}


/**
  * IO bundle for the Decode unit
  */
class DecodeUnitIO(implicit p: Parameters) extends VectorBaseBundle {
    val in = Vec(VIDecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
  // to Rename
    val out = Vec(VIDecodeWidth, ValidIO(new MicroOp))
    val canOut = Input(Bool())
}

/**
  * Decode unit that takes in a single CtrlFlow and generates a MicroOp.
  */
class VIDecodeUnit(implicit p: Parameters) extends VectorBaseModule with DecodeUnitConstants {
    val io = IO(new DecodeUnitIO)

    val cf_ctrl = Wire(new MicroOp)

    for ( i <- 0 until VIDecodeWidth ) {

        val decode_table = VectorArithDecode.table ++
          VectorStoreDecode.table ++
          VectorLoadDecode.table ++
          VectorConfDecode.table ++
          VectorWidenDecode.table ++
          VectorNarrowDecode.table

        // output
        //val cs: CtrlSignals = Wire(new CtrlSignals).decodev(io.in(i).bits.cf.instr, decode_table)
        val cs = Wire(new CtrlSignals)
        cs := io.in(i).bits.ctrl

        // read src1~3 location
//        cs.lsrc(0) := io.in(i).bits.cf.instr(VS1_MSB, VS1_LSB)
//        cs.lsrc(1) := io.in(i).bits.cf.instr(VS2_MSB, VS2_LSB)
        cs.lsrc(2) := io.in(i).bits.cf.instr(VD_MSB, VD_LSB)
        cs.ldest := io.in(i).bits.cf.instr(VD_MSB, VD_LSB)
        cs.srcType(2) := Mux(cs.vdWen, SrcType.vec, Mux(cs.fpWen, SrcType.fp, SrcType.reg))
        cs.funct6 := io.in(i).bits.cf.instr(F6_MSB, F6_LSB)
        cs.funct3 := io.in(i).bits.cf.instr(F3_MSB, F3_LSB)
        cs.NFiled := io.in(i).bits.cf.instr(NF_MSB, NF_LSB)
        cs.vm := io.in(i).bits.cf.instr(VM_LSB)

        if (cs.selImm != SelImm.X) {
            cs.imm := io.in(i).bits.cf.instr(VS1_MSB, VS1_LSB)
        }

        if (cs.NFiled != 0.U && (cs.fuType == FuType.ldu || cs.fuType == FuType.stu)) {
            cs.isSeg := true.B
            cs.isVLS := true.B
        } else if (cs.fuType == FuType.ldu || cs.fuType == FuType.stu) {
            cs.isVLS := true.B
            cs.isSeg := false.B
        } else {
            cs.isVLS := false.B
            cs.isSeg := false.B
        }


        cf_ctrl.ctrl := cs
        cf_ctrl.cf := io.in(i).bits.cf

        io.out(i).bits := cf_ctrl
        io.out(i).valid := io.in(i).valid && io.canOut

    }
}