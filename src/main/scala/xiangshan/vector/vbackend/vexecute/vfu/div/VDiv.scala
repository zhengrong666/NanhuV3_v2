package darecreek.exu.fu2.div

import chisel3._
import chisel3.util._
import darecreek.exu.fu2._
// import darecreek.exu.fu2.VFUParam._
import chipsalliance.rocketchip.config._

class VDiv(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val out = Decoupled(new VFpuOutput)
  })

  val divTops = Seq.fill(2)(Module(new DivTop))

  /** VFuInput -> LaneFuInput */
  for (i <- 0 until 2) {
    // divTops(i).io.in.valid := io.in.valid
    divTops(i).io.in.bits.uop.connectFromVUop(io.in.bits.uop, isDiv = true)
    divTops(i).io.in.bits.vs1 := UIntSplit(io.in.bits.vs1, 64)(i)
    divTops(i).io.in.bits.vs2 := UIntSplit(io.in.bits.vs2, 64)(i)
    divTops(i).io.in.bits.old_vd := UIntSplit(io.in.bits.oldVd, 64)(i)
    divTops(i).io.in.bits.rs1 := io.in.bits.rs1
    divTops(i).io.in.bits.prestart := 0.U // !!!!
    divTops(i).io.in.bits.mask := "hff".U // !!!!
    divTops(i).io.in.bits.tail := 0.U // !!!!
  }
  
  /** LaneFUOutput -> VFpOutput */
  val outVdReg = Reg(Vec(2, UInt(64.W)))
  val outfflagsReg = Reg(Vec(2, UInt(5.W)))
  val outUopReg = Reg(new VUop)
  val outValidReg = RegInit(VecInit.fill(2)(false.B))
  val allOutValidRegClear = !outValidReg(0) && !outValidReg(1)
  io.out.valid := outValidReg(0) && outValidReg(1)
  for (i <- 0 until 2) {
    divTops(i).io.out.ready := true.B
    when (divTops(i).io.out.valid) {
      outValidReg(i) := true.B
    }.elsewhen (io.out.fire) {
      outValidReg(i) := false.B
    }.otherwise {
      outValidReg(i) := outValidReg(i)
    }
    when (divTops(i).io.out.valid) {
      outVdReg(i) := divTops(i).io.out.bits.vd
      outfflagsReg(i) := divTops(i).io.out.bits.fflags
    }
  }
  io.out.bits.vd := Cat(outVdReg(1), outVdReg(0))
  io.out.bits.fflags := outfflagsReg(1) | outfflagsReg(0)
  val outVUop = Wire(new VUop)
  outVUop.connectFromLaneUop(divTops(0).io.out.bits.uop)
  when (divTops(0).io.out.valid) {
    outUopReg := outVUop
  }
  io.out.bits.uop := outUopReg

  val outIsReady = io.out.fire || allOutValidRegClear
  for (i <- 0 until 2) {
    divTops(i).io.in.valid := io.in.valid && outIsReady
  }
  io.in.ready := divTops(0).io.in.ready && divTops(1).io.in.ready && outIsReady
}

object Main extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new VDiv()(p.alterPartial({case VFuParamsKey => VFuParameters()})), Array("--target-dir", "generated",
              "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}