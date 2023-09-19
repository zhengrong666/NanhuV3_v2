package darecreek.exu.fu2.fp

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import org.chipsalliance.cde.config.Parameters
import darecreek.exu.fu2._
import xiangshan.Redirect

class VFPUWrapper(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VFuInput))
    val redirect = Input(ValidIO(new Redirect))
    val out = DecoupledIO(new VFpuOutput)
  })

  val ctrl = io.in.bits.uop.ctrl
  val info = io.in.bits.uop.info
  val funct6 = io.in.bits.uop.ctrl.funct6
  val funct3 = io.in.bits.uop.ctrl.funct3
  val vm = io.in.bits.uop.ctrl.vm
  val vs1_imm = io.in.bits.uop.ctrl.vs1_imm
  val widen = io.in.bits.uop.ctrl.widen
  val widen2 = io.in.bits.uop.ctrl.widen2
  val narrow = io.in.bits.uop.ctrl.narrow
  val narrow_to_1 = io.in.bits.uop.ctrl.narrow_to_1
  val ma = io.in.bits.uop.info.ma
  val ta = io.in.bits.uop.info.ta
  val vsew = io.in.bits.uop.info.vsew
  val vlmul = io.in.bits.uop.info.vlmul
  val vl = io.in.bits.uop.info.vl
  val vstart = io.in.bits.uop.info.vstart
  val vxrm = io.in.bits.uop.info.vxrm
  val frm = io.in.bits.uop.info.frm
  val uopIdx = io.in.bits.uop.uopIdx
  val uopEnd = io.in.bits.uop.uopEnd
  val sysUop = io.in.bits.uop.sysUop
  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val rs1 = io.in.bits.rs1
  val old_vd = io.in.bits.oldVd
  val vmask = io.in.bits.mask
  val fire = io.in.fire

  val vfredosum_vs = (funct6 === "b000011".U) && (funct3 === "b001".U)
  val vfredusum_vs = (funct6 === "b000001".U) && (funct3 === "b001".U)
  val vfredmax_vs = (funct6 === "b000111".U) && (funct3 === "b001".U)
  val vfredmin_vs = (funct6 === "b000101".U) && (funct3 === "b001".U)
  val vfwredosum_vs = (funct6 === "b110011".U) && (funct3 === "b001".U)
  val vfwredusum_vs = (funct6 === "b110001".U) && (funct3 === "b001".U)

  val fpu_red = vfredosum_vs ||
    vfredusum_vs ||
    vfredmax_vs ||
    vfredmin_vs ||
    vfwredosum_vs ||
    vfwredusum_vs

  val vd = Wire(Vec(2, UInt(64.W)))


  val idle :: calc_vs2 :: calc_vs1 :: Nil = Enum(3)

  val vsew_bytes = 1.U << vsew
  val vsew_bits = RegEnable(8.U << vsew, 0.U, fire)
  val red_state = RegInit(idle)

  val vs2_cnt = RegInit(0.U(vlenbWidth.W))
  val vs2_rnd = RegInit(0.U(vlenbWidth.W))

  when(fire) {
    when(vfwredosum_vs) {
      vs2_rnd := vlenb.U / vsew_bytes
    }.elsewhen(vfredosum_vs) {
      vs2_rnd := vlenb.U / vsew_bytes - 1.U
    }.otherwise {
      vs2_rnd := (vlenbWidth - 1).U - vsew
    }
  }

  val fpu_busy = RegInit(false.B)
  val fpu_out_valid = Wire(Bool())
  val fpu_out_ready = Wire(Bool())
  fpu_out_ready := true.B
  val fpu_valid = RegInit(false.B)
  val fpu_in_valid = fpu_valid || ((red_state === calc_vs2) && fpu_out_valid)
  val fpu_in_ready = Wire(Bool())

  val fpu_out = Wire(Vec(NLanes / 2, new LaneFUOutput))
  val red_in = Wire(Vec(NLanes / 2, new LaneFUInput))
  val red_out = fpu_out
  val red_out_vd = Wire(Vec(NLanes / 2, UInt(LaneWidth.W)))
  val vd_mask = (~0.U(VLEN.W))
  val vd_mask_half = (~0.U((VLEN / 2).W))
  val vd_mask_lane = (~0.U((LaneWidth).W))
  val red_vd_bits = Cat(0.U((VLEN / 2).W), Cat(red_out_vd.reverse))

  val red_busy = fpu_busy
  val red_in_valid = fpu_in_valid
  val red_in_ready = fpu_in_ready
  val red_out_valid = fpu_out_valid
  val red_out_ready = fpu_out_ready

  val old_vd_bits = RegInit(0.U(VLEN.W))
  val red_vd_tail_one = (vd_mask << vsew_bits) | (red_vd_bits & (vd_mask >> (VLEN.U - vsew_bits)))
  val red_vd_tail_vd = (old_vd_bits & (vd_mask << vsew_bits)) | (red_vd_bits & (vd_mask >> (VLEN.U - vsew_bits)))

  val red_vd = Mux(ta, red_vd_tail_one, red_vd_tail_vd)

  for (i <- 0 until NLanes / 2) {
    red_out_vd(i) := red_out(i).vd
  }

  switch(red_state) {
    is(idle) {
      when(fpu_red && fire) {
        red_state := calc_vs2
      }
    }

    is(calc_vs2) {
      when((vs2_cnt === (vs2_rnd - 1.U)) && (red_out_valid && red_out_ready)) {
        red_state := calc_vs1
      }
    }

    is(calc_vs1) {
      when(red_out_valid && red_out_ready) {
        red_state := idle
      }
    }
  }

  val expdIdxZero = RegInit(false.B)
  val output_en = RegInit(false.B)

  when(fire) {
    when(uopEnd) {
      output_en := true.B
    }.otherwise {
      output_en := false.B
    }
  }

  when(fire) {
    when(uopIdx === 0.U) {
      expdIdxZero := true.B
    }.otherwise {
      expdIdxZero := false.B
    }
  }

  when(fire) {
    old_vd_bits := old_vd
  }

  when(fpu_red && fire) {
    fpu_busy := true.B
  }.elsewhen((red_state === calc_vs1) && red_out_valid && red_out_ready) {
    fpu_busy := false.B
  }

  fpu_valid := false.B
  when(fpu_red && fire) {
    fpu_valid := true.B
  }

  when((red_state === calc_vs2) && red_out_valid && red_out_ready) {
    when(vs2_cnt === (vs2_rnd - 1.U)) {
      vs2_cnt := 0.U
    }.otherwise {
      vs2_cnt := vs2_cnt + 1.U
    }
  }

  def zero(w: Int) = 0.U(w.W)

  def umax(w: Int) = ~(0.U(w.W))

  def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w - 1).W)))

  def smin(w: Int) = Cat(1.U(1.W), 0.U((w - 1).W))

  def fmax(w: Int) = {
    w match {
      case 32 => Cat(0.U(1.W), ~(0.U(8.W)), 0.U(23.W))
      case 64 => Cat(0.U(1.W), ~(0.U(11.W)), 0.U(52.W))
      case _ => Cat(0.U(1.W), ~(0.U((w - 1).W)))
    }
  }

  def fmin(w: Int) = {
    w match {
      case 32 => Cat(1.U(1.W), ~(0.U(8.W)), 0.U(23.W))
      case 64 => Cat(1.U(1.W), ~(0.U(11.W)), 0.U(52.W))
      case _ => Cat(1.U(1.W), ~(0.U((w - 1).W)))
    }
  }

  val ele64 = Wire(UInt(64.W))
  val eew = SewOH(vsew)
  ele64 := 0.U
  when(fire) {
    when(vfredmax_vs) {
      ele64 := Mux1H(eew.oneHot, Seq(8, 16, 32, 64).map(n => fmin(n)))
    }.elsewhen(vfredmin_vs) {
      ele64 := Mux1H(eew.oneHot, Seq(8, 16, 32, 64).map(n => fmax(n)))
    }
  }

  val ele_cnt = vlenb.U / vsew_bytes
  val vmask_bits = vmask >> (ele_cnt * uopIdx)

  val vs2_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2((i + 1) * 8 - 1, i * 8)))
  val vs2m_bytes = Wire(Vec(vlenb, UInt(8.W)))
  val vs2m_bits = RegEnable(Cat(vs2m_bytes.reverse), 0.U, fire)
  val vlRemainBytes = Wire(UInt(8.W))
  vlRemainBytes := Mux((vl << vsew) >= Cat(uopIdx, 0.U(4.W)), (vl << vsew) - Cat(uopIdx, 0.U(4.W)), 0.U)

  for (i <- 0 until vlenb) {
    vs2m_bytes(i) := vs2_bytes(i)
    when((!vm && !vmask_bits(i.U / vsew_bytes)) || (i.U >= vlRemainBytes)) {
      when(vsew === 0.U) {
        vs2m_bytes(i) := ele64(7, 0)
      }.elsewhen(vsew === 1.U) {
        vs2m_bytes(i) := ele64((i % 2 + 1) * 8 - 1, (i % 2) * 8)
      }.elsewhen(vsew === 2.U) {
        vs2m_bytes(i) := ele64((i % 4 + 1) * 8 - 1, (i % 4) * 8)
      }.elsewhen(vsew === 3.U) {
        vs2m_bytes(i) := ele64((i % 8 + 1) * 8 - 1, (i % 8) * 8)
      }
    }
  }

  val div_cnt = 1.U << vs2_cnt
  val vs2_rnd0 = fpu_valid
  val vs2_rnd01 = (vs2_cnt === 0.U) && red_out_valid && red_out_ready
  val vs2_rnd1 = vs2_cnt === 1.U
  val vs2_rndx = (red_state === calc_vs2) && (vs2_cnt === (vs2_rnd - 1.U)) && red_out_valid && red_out_ready
  val red_out_bits = Cat(red_out_vd.reverse)
  val vs2m_bits_hi = vs2m_bits(VLEN - 1, VLEN / 2)
  val red_out_hi = (red_out_bits & (vd_mask_half >> ((VLEN / 2).U - (VLEN / 2).U / div_cnt))) >> (VLEN / 4).U / div_cnt
  val vs2m_bits_lo = vs2m_bits(VLEN / 2 - 1, 0)
  val red_out_lo = red_out_bits & (vd_mask_half >> ((VLEN / 2).U - (VLEN / 4).U / div_cnt))

  val vd_vsew = Mux(widen, vsew + 1.U, vsew)
  val eewVd = SewOH(vd_vsew)
  val vs1_zero = Wire(UInt(64.W))
  vs1_zero := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(XLEN - n, 0.U), vs1(n - 1, 0))) :+ vs1(63, 0))
  // vs1_zero := vs1(63, 0)

  val red_zero = RegEnable(red_out_bits(63, 0), (red_state === calc_vs1) && red_out_valid && red_out_ready)
  // val red_vs1_zero = Mux(expdIdxZero, Cat(Fill(VLEN / 2 - 64, 0.U), vs1_zero), Cat(Fill(VLEN / 2 - 64, 0.U), red_zero))
  val red_vs1_zero = Mux(expdIdxZero, vs1_zero, red_zero)
  val vs2_order = (vs2 >> vs2_cnt * vsew_bits) & (vd_mask >> ((VLEN).U - vsew_bits))
  val red_vs1_bits = Wire(UInt((VLEN / 2).W))
  val red_vs2_bits = Wire(UInt((VLEN / 2).W))
  val red_vs1 = VecInit(Seq.tabulate(NLanes / 2)(i => red_vs1_bits((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val red_vs2 = VecInit(Seq.tabulate(NLanes / 2)(i => red_vs2_bits((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val red_uop = Reg(new VExpdUOp)

  when(vfredosum_vs || vfwredosum_vs) {
    when(vs2_rnd0) {
      red_vs1_bits := vs1_zero
    }.otherwise {
      red_vs1_bits := red_out_bits
    }
    red_vs2_bits := vs2_order(VLEN / 2 - 1, 0)
  }.otherwise {
    when(vs2_rnd0) {
      red_vs1_bits := vs2m_bits_lo
      red_vs2_bits := vs2m_bits_hi
    }.elsewhen(vs2_rndx) {
      red_vs1_bits := red_out_bits
      red_vs2_bits := red_vs1_zero
    }.otherwise {
      red_vs1_bits := red_out_lo
      red_vs2_bits := red_out_hi
    }
  }

  red_uop.ctrl.funct6 := red_uop.ctrl.funct6
  red_uop.ctrl.funct3 := red_uop.ctrl.funct3
  red_uop.info.vsew := red_uop.info.vsew
  red_uop.info.destEew := red_uop.info.destEew
  red_uop.ctrl.lsrc(0) := vs1_imm
  red_uop.ctrl.lsrc(1) := 0.U
  red_uop.ctrl.ldest := 0.U
  red_uop.ctrl.vm := vm
  red_uop.ctrl.widen := widen
  red_uop.ctrl.widen2 := widen2
  red_uop.ctrl.narrow := narrow
  red_uop.ctrl.narrow_to_1 := narrow_to_1
  red_uop.info.vstart := vstart
  red_uop.info.vl := vl
  red_uop.info.vxrm := vxrm
  red_uop.info.frm := frm
  red_uop.info.vlmul := vlmul
  red_uop.info.vsew := vsew
  red_uop.info.ma := ma
  red_uop.info.ta := ta
  red_uop.info.destEew := Mux(ctrl.widen || ctrl.widen2,
    info.vsew + 1.U, info.vsew)
  red_uop.pdestVal := false.B

  when(fire) {
    red_uop.expdIdx := 0.U
    red_uop.expdEnd := true.B
    when(vfredosum_vs) {
      red_uop.ctrl.funct6 := "b000000".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfredusum_vs) {
      red_uop.ctrl.funct6 := "b000000".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfredmax_vs) {
      red_uop.ctrl.funct6 := "b000110".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfredmin_vs) {
      red_uop.ctrl.funct6 := "b000100".U
      red_uop.ctrl.funct3 := "b001".U
    }.elsewhen(vfwredosum_vs) {
      when(vs2_rnd0) {
        red_uop.ctrl.funct6 := "b110000".U
        red_uop.ctrl.funct3 := "b001".U
      }.otherwise {
        red_uop.ctrl.funct6 := "b110100".U
        red_uop.ctrl.funct3 := "b001".U
      }
    }.elsewhen(vfwredusum_vs) {
      when(vs2_rnd0) {
        red_uop.ctrl.funct6 := "b110000".U
        red_uop.ctrl.funct3 := "b001".U
      }.otherwise {
        red_uop.ctrl.funct6 := "b000000".U
        red_uop.ctrl.funct3 := "b001".U
        red_uop.info.vsew := vsew + 1.U
      }
    }
  }

  for (i <- 0 until NLanes / 2) {
    red_in(i).uop := red_uop
    red_in(i).vs1 := red_vs1(i)
    red_in(i).vs2 := red_vs2(i)
    red_in(i).old_vd := "hffffffffffffffff".U
    red_in(i).rs1 := rs1
    red_in(i).prestart := 0.U
    red_in(i).mask := "hff".U
    red_in(i).tail := 0.U
  }


  val output_valid = RegInit(false.B)
  val output_data = RegInit(0.U(VLEN.W))

  when(io.out.valid && io.out.ready) {
    output_valid := false.B
  }.elsewhen(output_en && (red_state === calc_vs1) && red_out_valid && red_out_ready) {
    output_valid := true.B
  }

  when(output_en && (red_state === calc_vs1) && red_out_valid && red_out_ready) {
    output_data := red_vd_tail_vd
  }

  //---- Tail gen ----
  val tail = TailGen(io.in.bits.uop.info.vl, uopIdx, eewVd)
  //---- Prestart gen ----
  val prestart = PrestartGen(io.in.bits.uop.info.vstart, uopIdx, eewVd)
  //---- Mask gen ----
  val mask16b = MaskExtract(io.in.bits.mask, uopIdx, eewVd)

  val tailReorg = MaskReorg(tail, eewVd)
  val prestartReorg = MaskReorg(prestart, eewVd)
  val mask16bReorg = MaskReorg(mask16b, eewVd)


  val fpu = Seq.fill(NLanes)(Module(new VFPUTop))
  for (i <- 0 until NLanes / 2) {
    fpu(i).io.in.valid := io.in.valid || fpu_in_valid
    fpu(i).io.in.bits.uop.ctrl.lsrc(0) := vs1_imm
    fpu(i).io.in.bits.uop.ctrl.lsrc(1) := 0.U
    fpu(i).io.in.bits.uop.ctrl.ldest := 0.U
    fpu(i).io.in.bits.uop.ctrl.vm := vm
    fpu(i).io.in.bits.uop.ctrl.funct6 := Mux(fpu_in_valid, red_in(i).uop.ctrl.funct6, funct6)
    fpu(i).io.in.bits.uop.ctrl.funct3 := Mux(fpu_in_valid, red_in(i).uop.ctrl.funct3, funct3)
    fpu(i).io.in.bits.uop.ctrl.widen := widen
    fpu(i).io.in.bits.uop.ctrl.widen2 := widen2
    fpu(i).io.in.bits.uop.ctrl.narrow := narrow
    fpu(i).io.in.bits.uop.ctrl.narrow_to_1 := narrow_to_1
    fpu(i).io.in.bits.uop.info.vstart := vstart
    fpu(i).io.in.bits.uop.info.vl := vl
    fpu(i).io.in.bits.uop.info.vxrm := vxrm
    fpu(i).io.in.bits.uop.info.frm := frm
    fpu(i).io.in.bits.uop.info.vlmul := vlmul
    fpu(i).io.in.bits.uop.info.vsew := vsew
    fpu(i).io.in.bits.uop.info.ma := ma
    fpu(i).io.in.bits.uop.info.ta := ta
    fpu(i).io.in.bits.uop.info.destEew := Mux(ctrl.widen || ctrl.widen2,
      info.vsew + 1.U, info.vsew)
    fpu(i).io.in.bits.uop.expdIdx := Mux(fpu_in_valid, red_in(i).uop.expdIdx, uopIdx)
    fpu(i).io.in.bits.uop.expdEnd := Mux(fpu_in_valid, red_in(i).uop.expdEnd, uopEnd)
    fpu(i).io.in.bits.uop.pdestVal := false.B
    fpu(i).io.in.bits.uop.sysUop := sysUop
    fpu(i).io.in.bits.vs1 := Mux(fpu_in_valid, red_in(i).vs1, vs1(VLEN / 2, 0))
    fpu(i).io.in.bits.vs2 := Mux(fpu_in_valid, red_in(i).vs2, vs2(VLEN / 2, 0))
    fpu(i).io.in.bits.old_vd := Mux(fpu_in_valid, red_in(i).old_vd, old_vd(VLEN / 2, 0))
    fpu(i).io.in.bits.rs1 := rs1
    fpu(i).io.in.bits.prestart := UIntSplit(prestartReorg, 8)(i)
    fpu(i).io.in.bits.mask := UIntSplit(mask16bReorg, 8)(i)
    fpu(i).io.in.bits.tail := UIntSplit(tailReorg, 8)(i)
    fpu(i).io.in.bits.prestart := UIntSplit(prestartReorg, 8)(i)
    fpu(i).io.out.ready := io.out.ready
    fpu(i).io.redirect := io.redirect
    vd(i) := fpu(i).io.out.bits.vd
    fpu_out(i) := fpu(i).io.out.bits
  }

  val vmask2 = Wire(UInt(8.W))
  vmask2 := vmask(15, 8)
  when(vsew === 1.U) {
    vmask2 := vmask(11, 4)
  }.elsewhen(vsew === 2.U) {
    vmask2 := vmask(9, 2)
  }.elsewhen(vsew === 3.U) {
    vmask2 := vmask(8, 1)
  }

  for (i <- NLanes / 2 until NLanes) {
    fpu(i).io.in.valid := io.in.valid
    fpu(i).io.in.bits.uop.ctrl.lsrc(0) := vs1_imm
    fpu(i).io.in.bits.uop.ctrl.lsrc(1) := 0.U
    fpu(i).io.in.bits.uop.ctrl.ldest := 0.U
    fpu(i).io.in.bits.uop.ctrl.vm := vm
    fpu(i).io.in.bits.uop.ctrl.funct6 := funct6
    fpu(i).io.in.bits.uop.ctrl.funct3 := funct3
    fpu(i).io.in.bits.uop.ctrl.widen := widen
    fpu(i).io.in.bits.uop.ctrl.widen2 := widen2
    fpu(i).io.in.bits.uop.ctrl.narrow := narrow
    fpu(i).io.in.bits.uop.ctrl.narrow_to_1 := narrow_to_1
    fpu(i).io.in.bits.uop.info.vstart := vstart
    fpu(i).io.in.bits.uop.info.vl := vl
    fpu(i).io.in.bits.uop.info.vxrm := vxrm
    fpu(i).io.in.bits.uop.info.frm := frm
    fpu(i).io.in.bits.uop.info.vlmul := vlmul
    fpu(i).io.in.bits.uop.info.vsew := vsew
    // fpu(i).io.in.bits.uop.info.vill := false.B
    fpu(i).io.in.bits.uop.info.ma := ma
    fpu(i).io.in.bits.uop.info.ta := ta
    fpu(i).io.in.bits.uop.info.destEew := Mux(ctrl.widen || ctrl.widen2,
      info.vsew + 1.U, info.vsew)
    fpu(i).io.in.bits.uop.expdIdx := uopIdx
    fpu(i).io.in.bits.uop.expdEnd := uopEnd
    fpu(i).io.in.bits.uop.pdestVal := false.B
    fpu(i).io.in.bits.uop.sysUop := sysUop
    fpu(i).io.in.bits.vs1 := vs1(VLEN - 1, VLEN / 2)
    fpu(i).io.in.bits.vs2 := vs2(VLEN - 1, VLEN / 2)
    fpu(i).io.in.bits.old_vd := old_vd(VLEN - 1, VLEN / 2)
    fpu(i).io.in.bits.rs1 := rs1
    fpu(i).io.in.bits.prestart := UIntSplit(prestartReorg, 8)(i)
    fpu(i).io.in.bits.mask := UIntSplit(mask16bReorg, 8)(i)
    fpu(i).io.in.bits.tail := UIntSplit(tailReorg, 8)(i)
    fpu(i).io.redirect := io.redirect
    fpu(i).io.out.ready := io.out.ready
    vd(i) := fpu(i).io.out.bits.vd
  }

  io.out.bits.uop.ctrl.funct6 := fpu(0).io.out.bits.uop.ctrl.funct6
  io.out.bits.uop.ctrl.funct3 := fpu(0).io.out.bits.uop.ctrl.funct3
  io.out.bits.uop.ctrl.vm := fpu(0).io.out.bits.uop.ctrl.vm
  io.out.bits.uop.ctrl.vs1_imm := fpu(0).io.out.bits.uop.ctrl.lsrc(0)
  io.out.bits.uop.ctrl.widen := fpu(0).io.out.bits.uop.ctrl.widen
  io.out.bits.uop.ctrl.widen2 := fpu(0).io.out.bits.uop.ctrl.widen2
  io.out.bits.uop.ctrl.narrow := fpu(0).io.out.bits.uop.ctrl.narrow
  io.out.bits.uop.ctrl.narrow_to_1 := fpu(0).io.out.bits.uop.ctrl.narrow_to_1
  io.out.bits.uop.info.ma := fpu(0).io.out.bits.uop.info.ma
  io.out.bits.uop.info.ta := fpu(0).io.out.bits.uop.info.ta
  io.out.bits.uop.info.vsew := fpu(0).io.out.bits.uop.info.vsew
  io.out.bits.uop.info.vlmul := fpu(0).io.out.bits.uop.info.vlmul
  io.out.bits.uop.info.vl := fpu(0).io.out.bits.uop.info.vl
  io.out.bits.uop.info.vstart := fpu(0).io.out.bits.uop.info.vstart
  io.out.bits.uop.info.vxrm := fpu(0).io.out.bits.uop.info.vxrm
  io.out.bits.uop.info.frm := fpu(0).io.out.bits.uop.info.frm
  io.out.bits.uop.uopIdx := fpu(0).io.out.bits.uop.expdIdx
  io.out.bits.uop.uopEnd := fpu(0).io.out.bits.uop.expdEnd
  io.out.bits.uop.sysUop := fpu(0).io.out.bits.uop.sysUop

  //  io.in.ready := (!io.in.valid || io.out.ready) && !red_busy
  //  io.out.valid := output_valid
  //  io.out.bits.vd := VecInit(Seq.tabulate(NLanes)(i => output_data((i + 1) * LaneWidth - 1, i * LaneWidth)))
  // compare output (narrow-to-1)

  val o_eew = SewOH(io.out.bits.uop.info.vsew)
  //---- Compare vd rearrangement ----
  val cmpOuts = fpu.map(_.io.out.bits.vd)
  val cmpOut128b = Mux1H(o_eew.oneHot, Seq(8, 4, 2, 1).map(
    k => Cat(0.U((128 - 2 * k).W), cmpOuts(1)(k - 1, 0), cmpOuts(0)(k - 1, 0))))
  val cmpOutOff128b = Mux1H(o_eew.oneHot, Seq(8, 4, 2, 1).map(
    k => Cat(0.U((128 - 2 * k).W), ~0.U((2 * k).W))))
  val shiftCmpOut = Wire(UInt(7.W))
  shiftCmpOut := Mux1H(o_eew.oneHot, Seq(4, 3, 2, 1).map(i => io.out.bits.uop.uopIdx(2, 0) << i))
  val cmpOutKeep = Wire(UInt(128.W))
  cmpOutKeep := cmpOut128b << shiftCmpOut
  val cmpOutOff = Wire(UInt(128.W))
  cmpOutOff := ~(cmpOutOff128b << shiftCmpOut)

  val old_cmpOutResult = RegInit(0.U(128.W))
  val cmpOutResult = old_cmpOutResult & cmpOutOff | cmpOutKeep // Compare
  when(fpu(0).io.out.valid) {
    old_cmpOutResult := Mux(io.out.bits.uop.uopEnd, 0.U, cmpOutResult)
  }

  io.out.bits.vd := Mux(io.out.bits.uop.ctrl.narrow_to_1, cmpOutResult, Cat(vd.reverse))
  io.in.ready := fpu(0).io.in.ready & fpu(1).io.in.ready & io.out.ready
  io.out.bits.fflags := fpu(0).io.out.bits.fflags | fpu(1).io.out.bits.fflags
  io.out.valid := Mux(io.out.bits.uop.ctrl.narrow_to_1, io.out.bits.uop.uopEnd & fpu(0).io.out.valid, fpu(0).io.out.valid)
  fpu_in_ready := fpu(0).io.in.ready
  fpu_out_valid := fpu(0).io.out.valid
}


import xiangshan._
object Main extends App {
  println("Generating hardware")
  val p = Parameters.empty.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })
  emitVerilog(new VFPUWrapper()(p.alterPartial({ case VFuParamsKey => VFuParameters() })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}