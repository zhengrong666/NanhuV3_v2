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
package xiangshan

import chisel3._
import chisel3.util._

package object vector {
  object SrcType {
    def reg = "b00".U
    def vs  = "b01".U
    def imm = "b01".U
    def fp  = "b10".U
    def DC  = imm // Don't Care
    def X   = BitPat("b??")

    def isReg(srcType: UInt)      = srcType === reg
    def isVc(srcType: UInt)       = srcType === vs
    def isImm(srcType: UInt)      = srcType === imm
    def isFp(srcType: UInt)       = srcType(1)
    def isPcOrImm(srcType: UInt)  = srcType(0)
    def isRegOrFp(srcType: UInt)  = !srcType(0)
    def regIsFp(srcType: UInt)    = srcType(1)
    
    def apply() = UInt(2.W)
  }

  object SrcState {
    def busy  = "b0".U
    def rdy   = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)
  }

  object FuType {
    def alu = "b0000".U
    def mac = "b0001".U
    def fp  = "b0010".U
    def div = "b0011".U
    def mask = "b0100".U
    def reduction = "b0101".U
    def permutation = "b0110".U
    def ldu = "b0111".U
    def stu = "b1000".U
    def X = BitPat("b????")
    def num = 9
    def apply() = UInt(log2Up(num).W)
  }

  object FuOpType {
    def X = BitPat("b???????")
    def apply() = UInt(7.W)
  }

  object ALUOpType {

  }

  object SelImm {
    def IMM_VA = "b0111".U
    def IMM_VL = "b0000".U
    def IMM_VS = "b0001".U
    def IMM_C = "b0010".U
    def IMM_CI = "b0011".U
    def INVALID_INSTR = "b0110".U
    def X = BitPat("b????")

    def apply() = UInt(4.W)
  }

  object CommitType {
    def VARITH = "b000".U // int/fp
    def VCONF = "b001".U // int/fp
    def VLOAD = "b010".U // load
    def VSTORE = "b011".U // store
    
    def isFused(commitType: UInt): Bool = commitType(2)
    def isLoadStore(commitType: UInt): Bool = !isFused(commitType) && commitType(1)
    def lsInstIsStore(commitType: UInt): Bool = commitType(0)
    def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)
    def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1) && !isFused(commitType)

    def apply() = UInt(3.W)
  }

  object IsWiden {
    def Widen = "b00".U
    def Widen2 = "b01".U
    def NotWiden = "b10".U
    def X = BitPat("b??")
    def apply() = UInt(2.W)
  }

  object IsNarrow {
    def Narrow = "b00".U
    def Narrow2 = "b01".U
    def NotNarrow = "b10".U
    def X = BitPat("b??")
    def apply() = UInt(2.W)
  }
}