package xiangshan.vector.vbackend.vexecute.vfu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSBundle

// Temporary. Will replaced by system Uop class.
class VUopCtrl extends Bundle {
  val funct6 = UInt(6.W)
  val funct3 = UInt(3.W)
  val vm = Bool()
  val vs1_imm = UInt(5.W)
  val widen = Bool()
  val widen2 = Bool()
  val narrow = Bool()
  val narrow_to_1 = Bool()
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2)
  def vi = !funct3(2) && funct3(1) && funct3(0)
}
class VUopInfo extends Bundle {
  val ma = Bool()
  val ta = Bool()
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
  val vl = UInt(bVL.W)
  val vstart = UInt(bVSTART.W)
  val vxrm = UInt(2.W)
  val frm = UInt(3.W)
}
class VUop extends Bundle {
  val ctrl = new VUopCtrl
  val info = new VUopInfo
  val uopIdx = UInt(3.W)
  val uopEnd = Bool()
}

class SewOH extends Bundle {  // 0   1   2   3
  // val oneHot = Vec(4, Bool()) // 8, 16, 32, 64
  val oneHot = UInt(4.W) // b0-b3: 8, 16, 32, 64
  def is8 = oneHot(0)
  def is16 = oneHot(1)
  def is32 = oneHot(2)
  def is64 = oneHot(3)
}
object SewOH {
  def apply(vsew: UInt): SewOH = {
    val sew = Wire(new SewOH)
    // sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U))
    sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)).asUInt
    sew
  }
}

// Input of FU
class VFuInput(implicit p: Parameters) extends XSBundle {
  val uop = new VUop
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val rs1 = UInt(XLEN.W)
  val oldVd = UInt(128.W)
  val mask = UInt(128.W)
}
// Output of ALU
class VAluOutput(implicit p: Parameters) extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
}

class VPermInput(implicit p: Parameters) extends XSBundle {
  val uop = new VUop
  val rs1 = UInt(XLEN.W)
  val vs1_preg_idx = Vec(8, UInt(PhyRegIdxWidth.W))
  val vs2_preg_idx = Vec(8, UInt(PhyRegIdxWidth.W))
  val old_vd_preg_idx = Vec(8, UInt(PhyRegIdxWidth.W))
  val mask_preg_idx = UInt(PhyRegIdxWidth.W)
  val uop_valid = Bool()
  val uop_rob_idx = UInt(9.W)
  val rdata = UInt(VLEN.W)
  val rvalid = Bool()
  val flush_vld = Bool()
  val flush_rob_idx = UInt(9.W)
}

class VPermOutput extends Bundle {
  val rd_en = Bool()
  val rd_preg_idx = UInt(8.W)
  val wb_vld = Bool()
  val wb_data = UInt(128.W)
  val perm_busy = Bool()
}

