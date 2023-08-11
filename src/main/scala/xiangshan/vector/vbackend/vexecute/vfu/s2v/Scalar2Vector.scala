package xiangshan.vector.vbackend.vexecute.vfu.s2v
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import darecreek.exu.fu2.{VAluOutput, VFuInput}
import xiangshan.XSModule
class Scalar2Vector(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new VFuInput))
    val out = ValidIO(new VAluOutput)
  })
  private val validReg_s1 = RegNext(io.in.valid, false.B)
  private val outWires = Wire(new VAluOutput)
  private val ta = io.in.bits.uop.info.ta
  private val oldVd = Mux(ta, Cat(Seq.fill(VLEN)(true.B)), io.in.bits.oldVd)
  outWires.vd := MuxCase(0.U, Seq(
    (io.in.bits.uop.info.vsew === 0.U) -> Cat(oldVd(VLEN - 1, 8), io.in.bits.rs1(7, 0)),
    (io.in.bits.uop.info.vsew === 1.U) -> Cat(oldVd(VLEN - 1, 16), io.in.bits.rs1(15, 0)),
    (io.in.bits.uop.info.vsew === 2.U) -> Cat(oldVd(VLEN - 1, 32), io.in.bits.rs1(31, 0)),
    (io.in.bits.uop.info.vsew === 3.U) -> Cat(oldVd(VLEN - 1, 64), io.in.bits.rs1(63, 0)),
  ))
  outWires.vxsat := 0.U
  private val bitsReg_s1 = RegEnable(outWires, io.in.valid)
  io.out.valid := validReg_s1
  io.out.bits := bitsReg_s1
}
