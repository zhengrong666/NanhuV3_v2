package xiangshan.vector.vbackend.vexecute.vfu.s2v
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import darecreek.exu.vfu.{VAluOutput, VFuInput}
import xiangshan.XSModule
import xs.utils.ZeroExt
object S2vOpType {
  def vx = 0.U
  def vi = 1.U
  def sx = 2.U
}
class Scalar2Vector(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new VFuInput))
    val out = ValidIO(new VAluOutput)
  })
  private val validReg_s1 = RegNext(io.in.valid, false.B)
  private val outWires = Wire(new VAluOutput)
  private val ta = io.in.bits.uop.info.ta
  private val oldVd = Mux(ta, Cat(Seq.fill(VLEN)(true.B)), io.in.bits.oldVd)
  private val sxvd = MuxCase(0.U, Seq(
    (io.in.bits.uop.info.vsew === 0.U) -> Cat(oldVd(VLEN - 1, 8), io.in.bits.rs1(7, 0)),
    (io.in.bits.uop.info.vsew === 1.U) -> Cat(oldVd(VLEN - 1, 16), io.in.bits.rs1(15, 0)),
    (io.in.bits.uop.info.vsew === 2.U) -> Cat(oldVd(VLEN - 1, 32), io.in.bits.rs1(31, 0)),
    (io.in.bits.uop.info.vsew === 3.U) -> Cat(oldVd(VLEN - 1, 64), io.in.bits.rs1(63, 0)),
  ))
  private val vivd = MuxCase(0.U, Seq(
    (io.in.bits.uop.info.vsew === 0.U) -> Cat(Seq.fill(VLEN / 8)(ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 8)).reverse),
    (io.in.bits.uop.info.vsew === 1.U) -> Cat(Seq.fill(VLEN / 16)(ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 16)).reverse),
    (io.in.bits.uop.info.vsew === 2.U) -> Cat(Seq.fill(VLEN / 32)(ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 32)).reverse),
    (io.in.bits.uop.info.vsew === 3.U) -> Cat(Seq.fill(VLEN / 64)(ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 64)).reverse),
  ))
  private val vxvd = MuxCase(0.U, Seq(
    (io.in.bits.uop.info.vsew === 0.U) -> Cat(Seq.fill(VLEN / 8)(io.in.bits.rs1(7, 0)).reverse),
    (io.in.bits.uop.info.vsew === 1.U) -> Cat(Seq.fill(VLEN / 16)(io.in.bits.rs1(15, 0)).reverse),
    (io.in.bits.uop.info.vsew === 2.U) -> Cat(Seq.fill(VLEN / 32)(io.in.bits.rs1(31, 0)).reverse),
    (io.in.bits.uop.info.vsew === 3.U) -> Cat(Seq.fill(VLEN / 64)(io.in.bits.rs1(63, 0)).reverse),
  ))

  outWires.vd := MuxCase(sxvd, Seq(
    (io.in.bits.uop.sysUop.ctrl.fuOpType === S2vOpType.vi) -> vivd,
    (io.in.bits.uop.sysUop.ctrl.fuOpType === S2vOpType.vx) -> vxvd,
  ))
  outWires.vxsat := 0.U
  private val bitsReg_s1 = RegEnable(outWires, io.in.valid)
  io.out.valid := validReg_s1
  io.out.bits := bitsReg_s1
}
