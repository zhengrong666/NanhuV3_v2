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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.{BitPat, _}
import freechips.rocketchip.rocket.Instructions
import freechips.rocketchip.util.uintToBitPat
import xs.utils.SignExt
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
class VIDecodeUnit(implicit p: Parameters) extends VectorBaseModule with VDecodeUnitConstants {
  val io = IO(new DecodeUnitIO)
  for ( i <- 0 until VIDecodeWidth ) {
    io.in(i).ready := io.canOut

    // output
    //val cs: CtrlSignals = Wire(new CtrlSignals).decodev(io.in(i).bits.cf.instr, decode_table)
    val cs = WireInit(io.in(i).bits.ctrl)
    // read src1~3 location
    //        cs.lsrc(0) := io.in(i).bits.cf.instr(VS1_MSB, VS1_LSB)
    //        cs.lsrc(1) := io.in(i).bits.cf.instr(VS2_MSB, VS2_LSB)
    cs.lsrc(2) := io.in(i).bits.cf.instr(VD_MSB, VD_LSB)
    cs.ldest := io.in(i).bits.cf.instr(VD_MSB, VD_LSB)
    cs.srcType(2) := Mux(cs.vdWen, SrcType.vec, Mux(cs.fpWen, SrcType.fp, SrcType.reg))
    cs.funct6 := io.in(i).bits.cf.instr(F6_MSB, F6_LSB)
    cs.funct3 := io.in(i).bits.cf.instr(F3_MSB, F3_LSB)
    cs.NField := io.in(i).bits.cf.instr(NF_MSB, NF_LSB)
    cs.vm := !io.in(i).bits.cf.instr(VM_LSB)

    when(cs.selImm =/= SelImm.X) {
      cs.imm := io.in(i).bits.cf.instr(VS1_MSB, VS1_LSB)
    }
    cs.isVLS := io.in(i).bits.ctrl.fuType === FuType.ldu || io.in(i).bits.ctrl.fuType === FuType.stu
    cs.isVmvnr := Seq(Instructions.VMV8R_V, Instructions.VMV8R_V, Instructions.VMV8R_V, Instructions.VMV8R_V).map(b => b === io.in(i).bits.cf.instr).reduce(_||_)

    io.out(i).bits := DontCare
    io.out(i).bits.cf := io.in(i).bits.cf
    io.out(i).bits.ctrl := cs
    io.out(i).bits.vCsrInfo.vill := false.B
    io.out(i).bits.vCsrInfo.vma := 1.U
    io.out(i).bits.vCsrInfo.vta := 1.U
    io.out(i).bits.vCsrInfo.vsew := 3.U
    io.out(i).bits.vCsrInfo.vlmul := cs.NField
    io.out(i).bits.vCsrInfo.vl := Cat(cs.NField, 0.U(1.W))
    io.out(i).bits.vCsrInfo.vlmax := cs.NField
    io.out(i).valid := io.in(i).valid
  }
}