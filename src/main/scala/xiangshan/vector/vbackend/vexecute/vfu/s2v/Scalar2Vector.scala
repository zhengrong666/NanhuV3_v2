package xiangshan.vector.vbackend.vexecute.vfu.s2v
import chisel3._
import chisel3.util.{Cat, _}
import org.chipsalliance.cde.config.Parameters
import darecreek.exu.vfu.{VAluOutput, VFuInput}
import xiangshan.XSModule
import xs.utils.{LogicShiftLeft, ZeroExt}

import scala.collection.immutable.Seq
import darecreek.exu.vfu.HasVFuParameters
import darecreek.exu.vfu.VFuModule
object S2vOpType {
  def vx = 0.U
  def vi = 1.U
  def sx = 2.U
}
class Scalar2Vector(implicit p: Parameters) extends VFuModule{
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new VFuInput))
    val out = ValidIO(new VAluOutput)
  })
  private val validReg_s1 = RegNext(io.in.valid, false.B)
  private val outWires = Wire(new VAluOutput)
  private val ta = io.in.bits.uop.info.ta
  private val vl = io.in.bits.uop.info.vl
  private val vstart = io.in.bits.uop.info.vstart
  private val uidx = io.in.bits.uop.uopIdx
  private val fuOpType = io.in.bits.uop.sysUop.ctrl.fuOpType
  private val oldVd = Mux(ta, Cat(Seq.fill(VLEN)(true.B)), io.in.bits.oldVd)
  private val sxvd = MuxCase(0.U, Seq(
    (io.in.bits.uop.info.vsew === 0.U) -> Cat(oldVd(VLEN - 1, 8), io.in.bits.rs1(7, 0)),
    (io.in.bits.uop.info.vsew === 1.U) -> Cat(oldVd(VLEN - 1, 16), io.in.bits.rs1(15, 0)),
    (io.in.bits.uop.info.vsew === 2.U) -> Cat(oldVd(VLEN - 1, 32), io.in.bits.rs1(31, 0)),
    (io.in.bits.uop.info.vsew === 3.U) -> Cat(oldVd(VLEN - 1, 64), io.in.bits.rs1(63, 0)),
  ))

  private val ovd8 = WireInit(VecInit(Seq.tabulate(VLEN / 8)(i => io.in.bits.oldVd(i * 8 + 7, i * 8))))
  private val ovd16 = WireInit(VecInit(Seq.tabulate(VLEN / 16)(i => io.in.bits.oldVd(i * 16 + 15, i * 16))))
  private val ovd32 = WireInit(VecInit(Seq.tabulate(VLEN / 32)(i => io.in.bits.oldVd(i * 32 + 31, i * 32))))
  private val ovd64 = WireInit(VecInit(Seq.tabulate(VLEN / 64)(i => io.in.bits.oldVd(i * 64 + 63, i * 64))))
  private val vd8 = Wire(Vec(VLEN / 8, UInt(8.W)))
  private val vd16 = Wire(Vec(VLEN / 16, UInt(16.W)))
  private val vd32 = Wire(Vec(VLEN / 32, UInt(32.W)))
  private val vd64 = Wire(Vec(VLEN / 64, UInt(64.W)))
  private val vi8 = WireInit(VecInit(Seq.tabulate(VLEN / 8)(i => i.U | LogicShiftLeft(ZeroExt(uidx, bVL), log2Ceil(VLEN / 8)))))
  private val vi16 = WireInit(VecInit(Seq.tabulate(VLEN / 16)(i => i.U | LogicShiftLeft(ZeroExt(uidx, bVL), log2Ceil(VLEN / 16)))))
  private val vi32 = WireInit(VecInit(Seq.tabulate(VLEN / 32)(i => i.U | LogicShiftLeft(ZeroExt(uidx, bVL), log2Ceil(VLEN / 32)))))
  private val vi64 = WireInit(VecInit(Seq.tabulate(VLEN / 64)(i => i.U | LogicShiftLeft(ZeroExt(uidx, bVL), log2Ceil(VLEN / 64)))))
  private val nd8 = Mux(fuOpType === S2vOpType.vi, ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 8), io.in.bits.rs1(7, 0))
  private val nd16 = Mux(fuOpType === S2vOpType.vi, ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 16), io.in.bits.rs1(15, 0))
  private val nd32 = Mux(fuOpType === S2vOpType.vi, ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 32), io.in.bits.rs1(31, 0))
  private val nd64 = Mux(fuOpType === S2vOpType.vi, ZeroExt(io.in.bits.uop.ctrl.vs1_imm, 64), io.in.bits.rs1(63, 0))

  private def FillElem(data:Vec[UInt], od:Vec[UInt], eindices:Vec[UInt], nd:UInt) = {
    for(((dst, odst), ei) <- data.zip(od).zip(eindices)){
      require(dst.getWidth == odst.getWidth && dst.getWidth == nd.getWidth)
      val u = ta === 0.U && ei >= vl || ei < vstart
      val a = ta === 1.U && ei >= vl
      dst := Mux(u, odst, Mux(a, Fill(dst.getWidth, 1.U(1.W)), nd))
    }
  }
  FillElem(vd8, ovd8, vi8, nd8)
  FillElem(vd16, ovd16, vi16, nd16)
  FillElem(vd32, ovd32, vi32, nd32)
  FillElem(vd64, ovd64, vi64, nd64)

  private val vixvd = MuxCase(0.U, Seq(
    (io.in.bits.uop.info.vsew === 0.U) -> Cat(vd8.reverse),
    (io.in.bits.uop.info.vsew === 1.U) -> Cat(vd16.reverse),
    (io.in.bits.uop.info.vsew === 2.U) -> Cat(vd32.reverse),
    (io.in.bits.uop.info.vsew === 3.U) -> Cat(vd64.reverse),
  ))

  outWires.vd := Mux(fuOpType === S2vOpType.sx, sxvd, vixvd)
  outWires.vxsat := 0.U
  private val bitsReg_s1 = RegEnable(outWires, io.in.valid)
  io.out.valid := validReg_s1
  io.out.bits := bitsReg_s1
}
