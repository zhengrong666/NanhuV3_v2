package xiangshan.vector.vbackend.vexecute.vfu.permutation

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import xiangshan.vector.vbackend.vexecute.vfu._

class permutation extends Module {
  val VLEN = 128
  val io = IO(new Bundle {
    val in = Input(new VPermInput)
    val out = Output(new VPermOutput)
  })

  val vlenb = VLEN / 8
  val funct6 = io.in.uop.ctrl.funct6
  val funct3 = io.in.uop.ctrl.funct3
  val vm = io.in.uop.ctrl.vm
  val vsew = io.in.uop.info.vsew
  val rs1 = io.in.rs1
  val vs1_preg_idx = io.in.vs1_preg_idx
  val vs2_preg_idx = io.in.vs2_preg_idx
  val old_vd_preg_idx = io.in.old_vd_preg_idx
  val mask_preg_idx = io.in.mask_preg_idx
  val ta = io.in.uop.info.ta
  val ma = io.in.uop.info.ma
  val vstart = io.in.uop.info.vstart
  val vl = io.in.uop.info.vl
  val vlmul = io.in.uop.info.vlmul
  val uop_valid = io.in.uop_valid
  val uop_rob_idx = io.in.uop_rob_idx
  val rdata = io.in.rdata
  val rvalid = io.in.rvalid
  val flush_vld = io.in.flush_vld
  val flush_rob_idx = io.in.flush_rob_idx


  val vslideup = funct6 === "b001110".U
  val vslidedn = funct6 === "b001111".U
  val vslide1up_vx = (funct6 === "b001110".U) && (funct3 === "b110".U)
  val vfslide1up_vf = (funct6 === "b001110".U) && (funct3 === "b101".U)
  val vslide1up = vslide1up_vx || vfslide1up_vf
  val vslide1dn_vx = (funct6 === "b001111".U) && (funct3 === "b110".U)
  val vfslide1dn_vf = (funct6 === "b001111".U) && (funct3 === "b101".U)
  val vslide1dn = vslide1dn_vx || vfslide1dn_vf
  val vrgather_vv = (funct6 === "b001100".U) && (funct3 === "b000".U)
  val vrgather16 = (funct6 === "b001110".U) && (funct3 === "b000".U)
  val vrgather_vx = (funct6 === "b001100".U) && (funct3 === "b100".U)
  val vrgather_vi = (funct6 === "b001100".U) && (funct3 === "b011".U)
  val vrgather_vxi = vrgather_vx || vrgather_vi
  val vcompress = (funct6 === "b010111".U) && (funct3 === "b010".U)
  val vrgather16_sew8 = vrgather16 && (vsew === 0.U)
  val vrgather16_sew32 = vrgather16 && (vsew === 2.U)
  val vrgather16_sew64 = vrgather16 && (vsew === 3.U)
  val vslide = vslideup || vslidedn || vslide1up || vslide1dn
  val vrgather = vrgather_vv || vrgather_vxi || vrgather16

  val funct6_reg = RegInit(0.U(6.W))
  val funct3_reg = RegInit(0.U(3.W))
  val vsew_reg = RegInit(0.U(3.W))
  val mask = RegInit(0.U(128.W))
  val old_vd = RegInit(0.U(128.W))
  val vs2_reg = RegInit(0.U(128.W))
  val mask_reg = RegNext(mask)
  val old_vd_reg = RegNext(old_vd)
  val rs1_reg = RegInit(0.U(64.W))
  val vs1_preg_idx_reg = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val vs2_preg_idx_reg = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val old_vd_preg_idx_reg = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val mask_preg_idx_reg = RegInit(0.U(8.W))
  val vm_reg = RegInit(false.B)
  val ta_reg = RegInit(false.B)
  val ma_reg = RegInit(false.B)
  val vstart_reg = RegInit(0.U(7.W))
  val vl_reg = RegInit(0.U(8.W))
  val vlmul_reg = RegInit(0.U(3.W))
  val uop_rob_idx_reg = RegInit(0.U(9.W))

  val vl_reg_bytes = vl_reg << vsew_reg

  val reg_vslideup = funct6_reg === "b001110".U
  val reg_vslidedn = funct6_reg === "b001111".U
  val reg_vslide1up_vx = (funct6_reg === "b001110".U) && (funct3_reg === "b110".U)
  val reg_vfslide1up_vf = (funct6_reg === "b001110".U) && (funct3_reg === "b101".U)
  val reg_vslide1up = reg_vslide1up_vx || reg_vfslide1up_vf
  val reg_vslide1dn_vx = (funct6_reg === "b001111".U) && (funct3_reg === "b110".U)
  val reg_vfslide1dn_vf = (funct6_reg === "b001111".U) && (funct3_reg === "b101".U)
  val reg_vslide1dn = reg_vslide1dn_vx || reg_vfslide1dn_vf
  val reg_vrgather_vv = (funct6_reg === "b001100".U) && (funct3_reg === "b000".U)
  val reg_vrgather16 = (funct6_reg === "b001110".U) && (funct3_reg === "b000".U)
  val reg_vrgather_vx = (funct6_reg === "b001100".U) && (funct3_reg === "b100".U)
  val reg_vrgather_vi = (funct6_reg === "b001100".U) && (funct3_reg === "b011".U)
  val reg_vrgather_vxi = reg_vrgather_vx || reg_vrgather_vi
  val reg_vcompress = (funct6_reg === "b010111".U) && (funct3_reg === "b010".U)
  val reg_vrgather16_sew8 = reg_vrgather16 && (vsew_reg === 0.U)
  val reg_vrgather16_sew32 = reg_vrgather16 && (vsew_reg === 2.U)
  val reg_vrgather16_sew64 = reg_vrgather16 && (vsew_reg === 3.U)
  val reg_vslide = reg_vslideup || reg_vslidedn || reg_vslide1up || reg_vslide1dn
  val reg_vrgather = reg_vrgather_vv || reg_vrgather_vxi || reg_vrgather16
  val vs1_type = Mux(reg_vrgather16, 1.U, vsew_reg)

  val wb_vld = Wire(Bool())
  val wb_idx = RegInit(0.U(3.W))
  //  val old_vd_idx = Mux(reg_vcompress, rd_vd_idx, Mux(vs_idx === vlmul_reg, vs_idx, vs_idx + 1.U)) //todo

  val vlRemain = RegInit(0.U(8.W))
  val vlRemainBytes = vlRemain << vsew_reg
  val vd_mask = (~0.U(VLEN.W))

  val vlRemain_reg = RegNext(vlRemain)
  val vlRemainBytes_reg = vlRemain_reg << vsew_reg
  val tail_bytes = Mux((vlRemainBytes_reg >= vlenb.U), 0.U, vlenb.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := vd_mask >> tail_bits
  val tail_old_vd = old_vd_reg & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)

  val vd_reg = RegInit(0.U(VLEN.W))

  //  val update_vd_idx = Mux(reg_vcompress, cmprs_update_vd_idx, Mux(reg_vrgather, vrgather_wb_vld_reg(4), rd_sent_reg(4)))
  val eewVs2 = SewOH(vsew_reg)

  //  val base = Wire(UInt(7.W))
  val vsew_bytes = Wire(UInt(3.W))
  val vsew_shift = Wire(UInt(3.W))
  val vmask_byte_strb = Wire(Vec(vlenb, UInt(1.W)))
  val vslideup_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslidedn_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1up_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1dn_vd = Wire(Vec(vlenb, UInt(8.W)))
  val perm_vd = Wire(UInt(VLEN.W))
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))

  val perm_busy = RegInit(false.B)
  val vs_idx = RegInit(0.U(3.W))
  val rd_vs_idx = RegInit(0.U(3.W))
  val vd_idx = RegInit(0.U(3.W))
  val update_vs_idx = Wire(Bool())
  val update_vd_idx = RegInit(false.B)
  val vslide_ele = Mux(reg_vslide1up || reg_vslide1dn, 1.U, rs1_reg)
  val vslide_bytes = vslide_ele << vsew_reg
  val vslide_lo_valid = Mux(reg_vslideup || reg_vslide1up, vslide_bytes(65, 4) + 1.U <= vs_idx, (reg_vslidedn || reg_vslide1dn) && (vs_idx + vslide_bytes(65, 4) <= vlmul_reg))
  val vslide_hi_valid = Mux(reg_vslideup || reg_vslide1up, vslide_bytes(65, 4) <= vs_idx, (reg_vslidedn || reg_vslide1dn) && (vs_idx + vslide_bytes(65, 4) + 1.U <= vlmul_reg))
  val vslide_cnt_max = Wire(UInt(2.W))
  val rd_cnt = RegInit(0.U(4.W))
  val rd_mask_en = RegInit(false.B)
  val rd_vs_en = RegInit(false.B)
  val rd_preg_idx = Wire(UInt(8.W))
  val vs1_rd_idx = RegInit(0.U(3.W))
  val rdata_rd_mask_en = Wire(Bool())
  val rdata_update_vs_idx = Wire(Bool())
  val rdata_vslide_lo_valid = Wire(Bool())
  val rdata_vslide_hi_valid = Wire(Bool())
  val rdata_rd_cnt = Wire(UInt(4.W))

  val rec_done = uop_valid
  val rd_done = (vs_idx === vlmul_reg) && update_vs_idx
  val calc_done = RegInit(false.B)

  when(flush_vld) {
    funct6_reg := 0.U
    funct3_reg := 0.U
    vsew_reg := 0.U
    rs1_reg := 0.U
    vs1_preg_idx_reg := VecInit(Seq.fill(8)(0.U(8.W)))
    vs2_preg_idx_reg := VecInit(Seq.fill(8)(0.U(8.W)))
    old_vd_preg_idx_reg := VecInit(Seq.fill(8)(0.U(8.W)))
    mask_preg_idx_reg := 0.U
    vm_reg := false.B
    ta_reg := false.B
    ma_reg := false.B
    vstart_reg := 0.U
    vl_reg := 0.U
    vlmul_reg := 0.U
    uop_rob_idx_reg := 0.U
  }.elsewhen(uop_valid) {
    funct6_reg := funct6
    funct3_reg := funct3
    vsew_reg := vsew
    rs1_reg := rs1
    vs1_preg_idx_reg := vs1_preg_idx
    vs2_preg_idx_reg := vs2_preg_idx
    old_vd_preg_idx_reg := old_vd_preg_idx
    mask_preg_idx_reg := mask_preg_idx
    vm_reg := vm
    ta_reg := ta
    ma_reg := ma
    vstart_reg := vstart
    vl_reg := Mux(vslideup && (rs1 > vl), Mux(rs1 > VLEN.U, VLEN.U, rs1), vl)
    vlmul_reg := (1.U << vlmul) - 1.U
    uop_rob_idx_reg := uop_rob_idx
  }

  vslide_cnt_max := Cat(0.U(1.W), vslide_lo_valid) + Cat(0.U(1.W), vslide_hi_valid)

  rd_mask_en := false.B
  when(uop_valid) {
    rd_mask_en := true.B
  }

  when(rd_mask_en) {
    rd_vs_en := true.B
  }.elsewhen(update_vs_idx && (vs_idx === vlmul_reg)) {
    rd_vs_en := false.B
  }

  when(rd_vs_en) {
    when(rd_cnt === vslide_cnt_max) {
      rd_cnt := 0.U
    }.otherwise {
      rd_cnt := rd_cnt + 1.U
    }
  }

  wb_vld := update_vs_idx
  update_vs_idx := reg_vslide && (rd_cnt === vslide_cnt_max) && rd_vs_en
  update_vd_idx := RegNext(rdata_update_vs_idx)

  when(flush_vld) {
    vs_idx := 0.U
  }.elsewhen(update_vs_idx) {
    when(vs_idx === vlmul_reg) {
      vs_idx := 0.U
    }.otherwise {
      vs_idx := vs_idx + 1.U
    }
  }

  when(flush_vld) {
    rd_vs_idx := 0.U
  }.elsewhen(rdata_update_vs_idx) {
    when(rd_vs_idx === vlmul_reg) {
      rd_vs_idx := 0.U
    }.otherwise {
      rd_vs_idx := rd_vs_idx + 1.U
    }
  }

  when(flush_vld) {
    vd_idx := 0.U
  }.elsewhen(update_vd_idx) {
    when(vd_idx === vlmul_reg) {
      vd_idx := 0.U
    }.otherwise {
      vd_idx := vd_idx + 1.U
    }
  }

  val rd_idx_lo = Wire(UInt(3.W))
  val rd_idx_hi = Wire(UInt(3.W))

  rd_idx_lo := 0.U
  rd_idx_hi := 0.U
  //  when(reg_vrgather) {
  //    rd_idx_lo := Mux(reg_vrgather16_sew8, Cat(table_lo_idx(1, 0), 0.U(1.W)), table_lo_idx)
  //    rd_idx_hi := Mux(reg_vrgather16_sew8, Cat(table_hi_idx(1, 0), 0.U(1.W)), table_hi_idx)
  //  }.elsewhen(reg_vslideup || reg_vslide1up) {
  when(reg_vslideup || reg_vslide1up) {
    rd_idx_lo := vs_idx - vslide_bytes(6, 4) - 1.U
    rd_idx_hi := vs_idx - vslide_bytes(6, 4)
  }.elsewhen(reg_vslidedn || reg_vslide1dn) {
    rd_idx_lo := vs_idx + vslide_bytes(6, 4)
    rd_idx_hi := vs_idx + vslide_bytes(6, 4) + 1.U
  }.elsewhen(reg_vcompress) {
    rd_idx_lo := vs_idx
  }

  rd_preg_idx := 0.U
  when(rd_mask_en) {
    rd_preg_idx := mask_preg_idx_reg
  }.elsewhen(reg_vslide && rd_vs_en) {
    when(rd_cnt === 0.U) {
      rd_preg_idx := old_vd_preg_idx_reg(vs_idx)
    }.elsewhen((rd_cnt === 1.U) && vslide_lo_valid) {
      rd_preg_idx := vs2_preg_idx_reg(rd_idx_lo)
    }.elsewhen((rd_cnt === 1.U) && vslide_hi_valid && !vslide_lo_valid) {
      rd_preg_idx := vs2_preg_idx_reg(rd_idx_hi)
    }.elsewhen(rd_cnt === 2.U) {
      rd_preg_idx := vs2_preg_idx_reg(rd_idx_hi)
    }
  }

  val rdata_reg = RegNext(rdata)
  val rvalid_reg = RegNext(rvalid)

  val vperm_fifo = Module(new perm_RegFifo(UInt(8.W), 8))

  vperm_fifo.io.enq.bits := Cat(rd_cnt, vslide_hi_valid, vslide_lo_valid, update_vs_idx, rd_mask_en)
  vperm_fifo.io.enq.valid := rd_mask_en || rd_vs_en
  vperm_fifo.io.deq.ready := rvalid_reg
  rdata_rd_mask_en := Mux(rvalid_reg, vperm_fifo.io.deq.bits(0), false.B)
  rdata_update_vs_idx := Mux(rvalid_reg, vperm_fifo.io.deq.bits(1), false.B)
  rdata_vslide_lo_valid := Mux(rvalid_reg, vperm_fifo.io.deq.bits(2), false.B)
  rdata_vslide_hi_valid := Mux(rvalid_reg, vperm_fifo.io.deq.bits(3), false.B)
  rdata_rd_cnt := Mux(rvalid_reg, vperm_fifo.io.deq.bits(7, 4), 0.U)

  when(!rdata_rd_mask_en && rvalid_reg && (rdata_rd_cnt === 0.U)) {
    old_vd := rdata_reg
  }

  when(rvalid_reg && (rdata_rd_cnt === 1.U)) {
    vs2_reg := rdata_reg
  }

  val rs1_bytes = VecInit(Seq.tabulate(8)(i => rs1_reg((i + 1) * 8 - 1, i * 8)))
  val rdata_bytes = VecInit(Seq.tabulate(vlenb)(i => rdata_reg((i + 1) * 8 - 1, i * 8)))
  val old_vd_bytes = VecInit(Seq.tabulate(vlenb)(i => old_vd((i + 1) * 8 - 1, i * 8)))
  val vs2_reg_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2_reg((i + 1) * 8 - 1, i * 8)))

  calc_done := false.B
  when(flush_vld) {
    calc_done := false.B
  }.elsewhen((vd_idx === vlmul_reg) && update_vd_idx) {
    calc_done := true.B
  }

  when(flush_vld) {
    perm_busy := false.B
  }.elsewhen(uop_valid) {
    perm_busy := true.B
  }.elsewhen(calc_done) {
    perm_busy := false.B
  }

  for (i <- 0 until vlenb) {
    // vrgather_byte_sel(i) := 0.U
    // vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    vslideup_vd(i) := old_vd(i * 8 + 7, i * 8)
    vslidedn_vd(i) := old_vd(i * 8 + 7, i * 8)
    vslide1up_vd(i) := old_vd(i * 8 + 7, i * 8)
    vslide1dn_vd(i) := old_vd(i * 8 + 7, i * 8)
  }

  // vslide
  val vslide_offset = vslide_bytes(3, 0)
  // slideoffset, unchange, old_vd
  when(!rdata_vslide_hi_valid && !rdata_vslide_lo_valid) {
    for (i <- 0 until vlenb) {
      vslideup_vd(i) := rdata_bytes(i)
    }
  }.elsewhen(rdata_vslide_hi_valid && !rdata_vslide_lo_valid) { // first old_vd & vs2
    for (i <- 0 until vlenb) {
      when(i.U < vslide_offset) { //old_vd
        vslideup_vd(i) := old_vd((i + 1) * 8 - 1, i * 8)
      }.otherwise { // vs2
        when(vmask_byte_strb(i).asBool) {
          vslideup_vd(i) := rdata_bytes(i.U - vslide_offset)
        }.otherwise {
          vslideup_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
        }
      }
    }
  }.elsewhen(rdata_vslide_hi_valid && rdata_vslide_lo_valid) { // vs2(i) & vs2(i-1)
    for (i <- 0 until vlenb) {
      when(vmask_byte_strb(i).asBool) {
        when(i.U < vslide_offset) { // vs2(i-1)
          vslideup_vd(i) := vs2_reg_bytes(vlenb.U - vslide_offset + i.U)
        }.otherwise { // vs2(i)
          vslideup_vd(i) := rdata_bytes(i.U - vslide_offset)
        }
      }.otherwise { // MA
        vslideup_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
      }
    }
  }

  vslide1up_vd := vslideup_vd
  when((rd_vs_idx === 0.U) && rdata_update_vs_idx) {
    for (i <- 0 until vlenb) {
      when((i.U < vsew_bytes) && vmask_byte_strb(i).asBool) {
        vslide1up_vd(i) := rs1_reg(i)
      }
    }
  }

  val vslidedn_old_vd = Mux(rdata_vslide_hi_valid || rdata_vslide_lo_valid, old_vd_bytes, rdata_bytes)
  val vslidedn_vs2_lo = Mux(rdata_vslide_hi_valid && rdata_vslide_lo_valid, vs2_reg_bytes, rdata_bytes)
  for (i <- 0 until vlenb) {
    when(vmask_byte_strb(i).asBool) {
      when((i.U < (vlenb.U - vslide_offset)) && rdata_vslide_lo_valid) {
        vslidedn_vd(i) := vslidedn_vs2_lo(i.U + vslide_offset)
      }.elsewhen((i.U >= (vlenb.U - vslide_offset)) && rdata_vslide_hi_valid) {
        vslidedn_vd(i) := rdata_bytes(i.U + vslide_offset - vlenb.U)
      }.otherwise(
        vslidedn_vd(i) := 0.U
      )
    }.otherwise {
      vslidedn_vd(i) := Mux(ma_reg, "hff".U, vslidedn_old_vd(i))
    }
  }

  vsew_bytes := 2.U << vsew_reg
  vsew_shift := Cat(0.U(1.W), ~vsew_reg(1,0)) + 1.U
  when(flush_vld) {
    vlRemain := 0.U
  }.elsewhen(uop_valid) {
    vlRemain := Mux(vslideup && (rs1 > vl), Mux(rs1 > VLEN.U, VLEN.U, rs1), vl)
    //  }.elsewhen(reg_vcompress && rd_sent_reg(3) && !rd_wb_reg(3)) {
    //    vlRemain := Mux(vlRemain >= (1.U << vsew_shift), vlRemain - (1.U << vsew_shift), 0.U)
  }.elsewhen(!reg_vrgather16_sew8 && !reg_vcompress && rdata_update_vs_idx) {
    vlRemain := Mux(vlRemain >= (1.U << vsew_shift), vlRemain - (1.U << vsew_shift), 0.U)
    //  }.elsewhen(reg_vrgather16_sew8 && update_vl && update_vl_cnt(0)) {
    //    vlRemain := Mux(vlRemain >= (1.U << vsew_shift), vlRemain - (1.U << vsew_shift), 0.U)
  }

  when(rdata_rd_mask_en && rvalid_reg) {
    mask := rdata_reg
  }.elsewhen(reg_vslide && rdata_update_vs_idx) {
    mask := mask >> (1.U << vsew_shift)
    //  }.elsewhen(reg_vrgather && update_vl) {
    //    when(reg_vrgather16_sew8) {
    //      when(update_vl_cnt(0)) {
    //        mask := mask >> (1.U << vsew_shift)
    //      }
    //    }.otherwise {
    //      mask := mask >> (1.U << vsew_shift)
    //    }
  }


  //  when(rdata_rd_mask_en && rvalid_reg) {
  //    mask := rdata
  //  }.elsewhen((reg_vcompress || reg_vslide) && rd_sent_reg(3) && !rd_wb_reg(3)) {
  //    mask := mask >> (1.U << vsew_shift)
  //  }.elsewhen(reg_vrgather && update_vl) {
  //    when(reg_vrgather16_sew8) {
  //      when(update_vl_cnt(0)) {
  //        mask := mask >> (1.U << vsew_shift)
  //      }
  //    }.otherwise {
  //      mask := mask >> (1.U << vsew_shift)
  //    }
  //  }

  for (i <- 0 until vlenb) {
    when(i.U < vlRemainBytes) {
      vmask_byte_strb(i) := mask(i) | (vm_reg & !reg_vcompress)
      when(vsew_reg === 1.U(3.W)) {
        vmask_byte_strb(i) := mask(i / 2) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 2.U(3.W)) {
        vmask_byte_strb(i) := mask(i / 4) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 3.U(3.W)) {
        vmask_byte_strb(i) := mask(i / 8) | (vm_reg & !reg_vcompress)
      }
    }.otherwise {
      vmask_byte_strb(i) := 0.U
    }
  }

  when(flush_vld) {
    vd_reg := 0.U
    //  }.elsewhen(reg_vcompress && rd_sent_reg(3)) {
    //    vd_reg := Cat(cmprs_vd.reverse)
  }.elsewhen(reg_vslideup && rdata_update_vs_idx) {
    vd_reg := Cat(vslideup_vd.reverse)
  }.elsewhen(reg_vslidedn && rdata_update_vs_idx) {
    vd_reg := Cat(vslidedn_vd.reverse)
  }.elsewhen(reg_vslide1up && rdata_update_vs_idx) {
    vd_reg := Cat(vslide1up_vd.reverse)
  }.elsewhen(reg_vslide1dn && rdata_update_vs_idx) {
    vd_reg := Cat(vslide1dn_vd.reverse)

    //  }.elsewhen(reg_vrgather && vrgather_table_sent_reg(3)) {
    //    vd_reg := Cat(vrgather_vd.reverse)
  }

  val vstartRemain = RegInit(0.U(8.W))
  val vstartRemainBytes = vstartRemain << vsew_reg
  val vstart_bytes = Mux(vstartRemainBytes >= vlenb.U, vlenb.U, vstartRemainBytes)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := vd_mask << vstart_bits
  val vstart_old_vd = old_vd_reg & (~vmask_vstart_bits)

  when(flush_vld) {
    vstartRemain := 0.U
  }.elsewhen(uop_valid) {
    vstartRemain := Mux(vslideup && (rs1 > vstart), Mux(rs1 > VLEN.U, VLEN.U, rs1), vstart)
    //  }.elsewhen(reg_vcompress && rd_sent_reg(4) && !rd_wb_reg(4)) {
    //    vstartRemain := Mux(vstartRemain >= (1.U << vsew_shift), vstartRemain - (1.U << vsew_shift), 0.U)
  }.elsewhen(!reg_vrgather16_sew8 && !reg_vcompress && update_vd_idx) {
    vstartRemain := Mux(vstartRemain >= (1.U << vsew_shift), vstartRemain - (1.U << vsew_shift), 0.U)
  }.elsewhen(reg_vrgather16_sew8 && update_vd_idx && vd_idx(0)) {
    vstartRemain := Mux(vstartRemain >= (1.U << vsew_shift), vstartRemain - (1.U << vsew_shift), 0.U)
  }

  perm_tail_mask_vd := vd_reg
  when(vstart_reg >= vl_reg) {
    perm_tail_mask_vd := old_vd_reg
    // }.elsewhen((reg_vrgather && vrgather_table_sent_reg(4)) || rd_sent_reg(4)) {
  }.elsewhen(update_vd_idx) {
    perm_tail_mask_vd := (vd_reg & vmask_tail_bits & vmask_vstart_bits) | tail_vd | vstart_old_vd
  }

  perm_vd := perm_tail_mask_vd
  // when(reg_vcompress && rd_sent_reg(4) && !rd_wb_resent_reg(4)) {
  //   perm_vd := vd_reg
  // }

  io.out.rd_en := rd_mask_en || rd_vs_en
  io.out.rd_preg_idx := rd_preg_idx
  io.out.wb_vld := wb_vld
  io.out.wb_data := perm_vd
  io.out.perm_busy := perm_busy





  //  update_vs_idx := false.B
  //  when(reg_vrgather && !Cat(table_hi_lo.reverse).orR && (perm_state === rd_vs) && !block_fsm_rd_vld && !rd_done) {
  //    update_vs_idx := true.B
  //  }.elsewhen(reg_vcompress && (perm_state === rd_vs) && !cmprs_rd_vd && !rd_wb && !block_fsm_rd_vld) { // if vcompress into vd, then read this vs_idx again
  //    update_vs_idx := true.B
  //  }.elsewhen(reg_vslide && (perm_state === rd_vs) && !block_fsm_rd_vld) {
  //    update_vs_idx := true.B
  //  }

  //  when((!block_fsm_rd_vld && !rd_wb) || flush_vld) {
  //    fsm_rd_vld_0 := false.B
  //    when((reg_vrgather_vv || reg_vrgather16) && (perm_state === rd_vs) && !rd_done) {
  //      fsm_rd_vld_0 := true.B
  //      when(reg_vrgather16_sew32) {
  //        fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx(2, 1))
  //      }.elsewhen(reg_vrgather16_sew64) {
  //        fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx(2))
  //      }.otherwise {
  //        fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx)
  //      }
  //    }
  //  }
  //  val rd_idx_lo = Wire(UInt(3.W))
  //  val rd_idx_hi = Wire(UInt(3.W))
  //  val reg_table_lo_idx = RegInit(0.U(4.W))
  //  val reg_table_hi_idx = RegInit(0.U(4.W))
  //
  //  when(update_table) {
  //    reg_table_lo_idx := table_lo_idx
  //    reg_table_hi_idx := table_hi_idx
  //  }

  //  rd_idx_lo := 0.U
  //  rd_idx_hi := 0.U
  //  when(reg_vrgather) {
  //    rd_idx_lo := Mux(reg_vrgather16_sew8, Cat(table_lo_idx(1, 0), 0.U(1.W)), table_lo_idx)
  //    rd_idx_hi := Mux(reg_vrgather16_sew8, Cat(table_hi_idx(1, 0), 0.U(1.W)), table_hi_idx)
  //  }.elsewhen(reg_vslideup) {
  //    rd_idx_lo := vs_idx - vslide_bytes(6, 4) - 1.U
  //    rd_idx_hi := vs_idx - vslide_bytes(6, 4)
  //  }.elsewhen(reg_vslidedn) {
  //    rd_idx_lo := vs_idx + vslide_bytes(6, 4)
  //    rd_idx_hi := vs_idx + vslide_bytes(6, 4) + 1.U
  //  }.elsewhen(reg_vcompress) {
  //    rd_idx_lo := vs_idx
  //  }

  //  when((!block_fsm_rd_vld && !rd_wb) || flush_vld) {
  //    fsm_rd_vld_1 := false.B
  //    when(((reg_vrgather && table_lo.orR) || (reg_vcompress && !cmprs_rd_vd) || (reg_vslide && src_lo_valid)) && (perm_state === rd_vs) && !rd_done) {
  //      fsm_rd_vld_1 := true.B
  //      fsm_rd_preg_idx_1 := vs2_preg_idx(rd_idx_lo)
  //    }
  //  }

  //  when((!block_fsm_rd_vld && !rd_wb) || flush_vld) {
  //    fsm_rd_vld_2 := false.B
  //    when(((reg_vrgather && table_hi.orR) || (reg_vslide && src_hi_valid)) && (perm_state === rd_vs) && !rd_done) {
  //      fsm_rd_vld_2 := true.B
  //      fsm_rd_preg_idx_2 := vs2_preg_idx(rd_idx_hi)
  //    }
  //  }

  //  when((!block_fsm_rd_vld && !rd_wb) || flush_vld) {
  //    fsm_rd_vld_3 := false.B
  //    when(((!reg_vcompress && (perm_state === rd_vs)) || (reg_vcompress && cmprs_rd_vd)) && !rd_done) {
  //      fsm_rd_vld_3 := true.B
  //      fsm_rd_preg_idx_3 := old_vd_preg_idx(old_vd_idx)
  //    }
  //  }

  //  when(viq01_valid) {
  //    vs1_preg_idx(viq0_uop_idx) := viq0_vs1_preg_idx
  //    vs2_preg_idx(viq0_uop_idx) := viq0_vs2_preg_idx
  //    old_vd_preg_idx(viq0_uop_idx) := viq0_old_vd_preg_idx
  //    vd_preg_idx(viq0_uop_idx) := viq0_vd_preg_idx
  //    vs1_preg_idx(viq1_uop_idx) := viq1_vs1_preg_idx
  //    vs2_preg_idx(viq1_uop_idx) := viq1_vs2_preg_idx
  //    old_vd_preg_idx(viq1_uop_idx) := viq1_old_vd_preg_idx
  //    vd_preg_idx(viq1_uop_idx) := viq1_vd_preg_idx
  //    when(vrgather_vxi || vslide) {
  //      for (i <- 0 until 8) {
  //        vs1_preg_idx(i) := viq0_vs1((i + 1) * 8 - 1, i * 8)
  //      }
  //    }
  //  }.elsewhen(viq0_valid) {
  //    vs1_preg_idx(viq0_uop_idx) := viq0_vs1_preg_idx
  //    vs2_preg_idx(viq0_uop_idx) := viq0_vs2_preg_idx
  //    old_vd_preg_idx(viq0_uop_idx) := viq0_old_vd_preg_idx
  //    vd_preg_idx(viq0_uop_idx) := viq0_vd_preg_idx
  //    when(vrgather_vxi || vslide) {
  //      for (i <- 0 until 8) {
  //        vs1_preg_idx(i) := viq0_vs1((i + 1) * 8 - 1, i * 8)
  //      }
  //    }
  //  }.elsewhen(viq1_valid) {
  //    vs1_preg_idx(viq1_uop_idx) := viq1_vs1_preg_idx
  //    vs2_preg_idx(viq1_uop_idx) := viq1_vs2_preg_idx
  //    old_vd_preg_idx(viq1_uop_idx) := viq1_old_vd_preg_idx
  //    vd_preg_idx(viq1_uop_idx) := viq1_vd_preg_idx
  //    when(vrgather_vxi || vslide) {
  //      for (i <- 0 until 8) {
  //        vs1_preg_idx(i) := viq1_vs1((i + 1) * 8 - 1, i * 8)
  //      }
  //    }
  //  }


  /////
  /////  when(flush_vld) {
  /////    rd_sent_idx := 0.U
  /////  }.elsewhen(rd_sent && !rd_wb_resent) {
  /////    when(rd_sent_idx === vlmul_reg) {
  /////      rd_sent_idx := 0.U
  /////    }.elsewhen(cmprs_rd_vd) {
  /////      rd_sent_idx := rd_vd_idx
  /////    }.otherwise {
  /////      rd_sent_idx := rd_sent_idx + 1.U
  /////    }
  /////  }
  /////
  /////  wb_vld := rd_sent
  /////  when(reg_vcompress) {
  /////    wb_vld := cmprs_wb_vld
  /////  }.elsewhen(reg_vrgather) {
  /////    wb_vld := vrgather_wb_vld
  /////  }
  /////
  /////
  /////  when(flush_vld) {
  /////    wb_idx := 0.U
  /////  }.elsewhen(wb_vld) {
  /////    when(wb_idx === vlmul_reg) {
  /////      wb_idx := 0.U
  /////    }.otherwise {
  /////      wb_idx := wb_idx + 1.U
  /////    }
  /////  }
  /////
  /////  when(flush_vld) {
  /////    update_vl_cnt := 0.U
  /////  }.elsewhen(update_vl) {
  /////    when(update_vl_cnt === vlmul_reg) {
  /////      update_vl_cnt := 0.U
  /////    }.otherwise {
  /////      update_vl_cnt := update_vl_cnt + 1.U
  /////    }
  /////  }


  //
  //   val vrgather_byte_sel = Wire(Vec(vlenb, UInt(64.W)))
  //   val first_gather = update_table_cnt === 0.U
  //   val vrgather_vd = Wire(Vec(vlenb, UInt(8.W)))
  //   val cmprs_vd = Wire(Vec(vlenb, UInt(8.W)))
  //   val vmask_byte_strb_old_vd = Wire(Vec(vlenb, UInt(1.W)))
  //   val vmask_byte_strb_lmul = Wire(Vec(VLEN, UInt(1.W)))
  //   val cmprs_rd_vlRemain = RegInit(0.U(8.W))
  //   val cmprs_rd_vlRemainBytes = cmprs_rd_vlRemain << vsew_reg
  //   val old_vd_reg = RegNext(old_vd)
  //   val update_vl_cnt_reg = RegNext(update_vl_cnt) //todo
  //   val vmask_byte_strb_reg = RegNext(Cat(vmask_byte_strb.reverse))
  //   val vs2_lo_reg = RegNext(vs2_lo)
  //   val idle :: rec_idx :: rd_vs :: calc_vd :: Nil = Enum(4)
  //   val perm_state = RegInit(idle)
  //   val one_sum = RegInit(0.U(8.W))
  //   val cmprs_update_vd_idx = Wire(Bool())
  //   val res_idx = Wire(Vec(vlenb, UInt(8.W)))
  //   val res_valid = Wire(Vec(vlenb, Bool()))
  //   val current_ones_sum = Wire(Vec(vlenb, UInt(8.W)))
  //   val update_table = reg_vrgather && (perm_state === rd_vs) & !block_fsm_rd_vld
  //   val current_vs_ones_sum = PopCount(Cat(vmask_byte_strb.reverse))
  //   val current_rd_vs_ones_sum = PopCount(Cat(vmask_byte_strb_old_vd.reverse))
  //   val total_one_sum = RegInit(0.U(8.W))
  //
  //   when(flush_vld) {
  //     total_one_sum := 0.U
  //   }.elsewhen(rec_done) {
  //     total_one_sum := PopCount(Cat(vmask_byte_strb_lmul.reverse))
  //   }
  //
  //   val update_vl = Mux(reg_vcompress, cmprs_wb_vld_reg(3), Mux(reg_vrgather, vrgather_wb_vld_reg(3), rd_sent_reg(3)))
  //   val rd_sent = (fsm_rd_vld_0 || fsm_rd_vld_1 || fsm_rd_vld_2 || fsm_rd_vld_3) && !block_fsm_rd_vld
  //   val vrgather_rd_en = RegInit(false.B)
  //   val vrgather_wb_en = RegInit(false.B)
  //   val vrgather_table_sent = vrgather_rd_en && rd_sent
  //   val vrgather_wb_vld = vrgather_wb_en & rd_sent
  //   val rd_wb = Wire(Bool())
  //   val rd_vd_idx = RegInit(0.U(4.W))
  //   val cmprs_rd_vd = RegInit(false.B)
  //   val cmprs_rd_vd_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  //   val rd_one_sum = RegInit(0.U(8.W))
  //   val cmprs_rd_vd_wb_en = RegInit(false.B)
  //   val cmprs_rd_vd_sent = cmprs_rd_vd_wb_en && rd_sent
  //   val cmprs_wb_vld = rd_wb || (cmprs_rd_vd_wb_en && rd_sent) // compress vs2 into vd or read old vd for vd
  //   val rd_wb_resent = RegInit(false.B)
  //
  //   when(flush_vld) {
  //     cmprs_rd_vlRemain := 0.U
  //   }.elsewhen(viq0_valid && (viq0_uop_idx === 0.U)) {
  //     cmprs_rd_vlRemain := viq0_vl
  //   }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
  //     cmprs_rd_vlRemain := viq1_vl
  //   }.elsewhen(reg_vcompress && rd_sent && !rd_wb_resent) {
  //     cmprs_rd_vlRemain := Mux(cmprs_rd_vlRemain >= (1.U << vsew_shift), cmprs_rd_vlRemain - (1.U << vsew_shift), 0.U)
  //   }
  //
  //   for (i <- 0 until vlenb) {
  //     when(i.U < cmprs_rd_vlRemainBytes) {
  //       vmask_byte_strb_old_vd(i) := old_vd(i) | (vm_reg & !reg_vcompress)
  //       when(vsew_reg === 1.U(3.W)) {
  //         vmask_byte_strb_old_vd(i) := old_vd(i / 2) | (vm_reg & !reg_vcompress)
  //       }.elsewhen(vsew_reg === 2.U(3.W)) {
  //         vmask_byte_strb_old_vd(i) := old_vd(i / 4) | (vm_reg & !reg_vcompress)
  //       }.elsewhen(vsew_reg === 3.U(3.W)) {
  //         vmask_byte_strb_old_vd(i) := old_vd(i / 8) | (vm_reg & !reg_vcompress)
  //       }
  //     }.otherwise {
  //       vmask_byte_strb_old_vd(i) := 0.U
  //     }
  //   }
  //
  //   for (i <- 0 until VLEN) {
  //     when(i.U < vl_reg_bytes) {
  //       vmask_byte_strb_lmul(i) := mask(i) | (vm_reg & !reg_vcompress)
  //       when(vsew_reg === 1.U(3.W)) {
  //         vmask_byte_strb_lmul(i) := mask(i / 2) | (vm_reg & !reg_vcompress)
  //       }.elsewhen(vsew_reg === 2.U(3.W)) {
  //         vmask_byte_strb_lmul(i) := mask(i / 4) | (vm_reg & !reg_vcompress)
  //       }.elsewhen(vsew_reg === 3.U(3.W)) {
  //         vmask_byte_strb_lmul(i) := mask(i / 8) | (vm_reg & !reg_vcompress)
  //       }
  //     }.otherwise {
  //       vmask_byte_strb_lmul(i) := 0.U
  //     }
  //   }
  //   when(flush_vld) {
  //     rd_wb_resent := false.B
  //   }.elsewhen(rd_wb) {
  //     rd_wb_resent := true.B
  //   }.elsewhen(rd_sent) {
  //     rd_wb_resent := false.B
  //   }
  //
  //   when(flush_vld) {
  //     cmprs_rd_vd_wb_en := false.B
  //   }.elsewhen(rd_done) {
  //     cmprs_rd_vd_wb_en := false.B
  //   }.elsewhen(reg_vcompress && cmprs_rd_vd && rd_sent && !rd_wb) {
  //     cmprs_rd_vd_wb_en := true.B
  //   }
  //
  //   when(flush_vld) {
  //     rd_one_sum := 0.U
  //   }.elsewhen(rd_done) {
  //     rd_one_sum := 0.U
  //   }.elsewhen(reg_vcompress && (perm_state === rd_vs) && rd_sent && !rd_wb_resent && !cmprs_rd_vd) { // current_rd_vs_ones_sum updated with rd_sent
  //     when((rd_one_sum + current_rd_vs_ones_sum) >= vlenb.U) {
  //       rd_one_sum := rd_one_sum + current_rd_vs_ones_sum - vlenb.U
  //     }.otherwise {
  //       rd_one_sum := rd_one_sum + current_rd_vs_ones_sum
  //     }
  //   }.elsewhen(rd_one_sum >= vlenb.U) {
  //     rd_one_sum := rd_one_sum - vlenb.U
  //   }
  //
  //   rd_wb := false.B
  //   when(reg_vcompress && ((rd_one_sum + current_rd_vs_ones_sum) >= vlenb.U) && rd_sent && !rd_wb_resent) {
  //     rd_wb := true.B
  //   }
  //
  //
  //   when(flush_vld) {
  //     vrgather_rd_en := false.B
  //   }.elsewhen(update_table) {
  //     vrgather_rd_en := true.B
  //   }.elsewhen(rd_sent) {
  //     vrgather_rd_en := false.B
  //   }
  //
  //   when(flush_vld) {
  //     vrgather_wb_en := false.B
  //   }.elsewhen(update_vs_idx) {
  //     vrgather_wb_en := true.B
  //   }.elsewhen(rd_sent) {
  //     vrgather_wb_en := false.B
  //   }
  //
  //   rd_vd_idx := total_one_sum(7, 4)
  //   when(flush_vld) {
  //     rd_vd_idx := 0.U
  //   }.elsewhen(block_fsm_rd_vld) {
  //     rd_vd_idx := rd_vd_idx
  //   }.elsewhen((rd_vd_idx === (Cat(0.U(1.W), vlmul_reg) + 1.U)) && cmprs_rd_vd && !block_fsm_rd_vld) {
  //     rd_vd_idx := 0.U
  //   }.elsewhen(cmprs_rd_vd && !block_fsm_rd_vld && !rd_wb) {
  //     rd_vd_idx := rd_vd_idx + 1.U
  //   }
  //
  //   // read remain old_vd after all vs2 compress into vd
  //   when(flush_vld) {
  //     cmprs_rd_vd := false.B
  //   }.elsewhen((rd_vd_idx === (Cat(0.U(1.W), vlmul_reg) + 1.U)) && !block_fsm_rd_vld) {
  //     cmprs_rd_vd := false.B
  //   }.elsewhen(reg_vcompress && (vs_idx === vlmul_reg) && update_vs_idx) {
  //     when(rd_vd_idx =/= (Cat(0.U(1.W), vlmul_reg) + 1.U)) {
  //       cmprs_rd_vd := true.B
  //     }
  //   }
  //
  //   when(flush_vld) {
  //     perm_state := idle
  //   }.otherwise {
  //     switch(perm_state) {
  //       is(idle) {
  //         when(uop_valid) {
  //           perm_state := rec_idx
  //         }
  //       }
  //
  //       is(rec_idx) {
  //         when(rec_done) {
  //           perm_state := rd_vs
  //         }
  //       }
  //
  //       is(rd_vs) {
  //         when(rd_done) {
  //           perm_state := calc_vd
  //         }
  //       }
  //
  //       is(calc_vd) {
  //         when(calc_done) {
  //           perm_state := idle
  //         }
  //       }
  //     }
  //   }
  //
  //
  //   def access_table_gen(vs1: UInt, vsew: UInt, uop_idx: UInt, vrgather_vxi: Bool, vrgather16: Bool): Seq[UInt] = {
  //     val sew = SewOH(Mux(vrgather16, 1.U(2.W), vsew))
  //     val shift = Wire(UInt(3.W))
  //     val table_idx = Wire(Vec(vlenb, UInt(64.W)))
  //     val access_table = Wire(Vec(8, UInt(1.W)))
  //     val vrgather16_sew8 = vrgather16 && (vsew === 0.U)
  //     val vrgather16_sew32 = vrgather16 && (vsew === 2.U)
  //     val vrgather16_sew64 = vrgather16 && (vsew === 3.U)
  //     access_table := VecInit(Seq.fill(8)(0.U))
  //     shift := Cat(0.U(1.W), ~vsew) + 1.U(3.W)
  //
  //     for (i <- 0 until vlenb) {
  //       table_idx(i) := 0.U
  //     }
  //
  //     when(vrgather16_sew32) {
  //       when(uop_idx(0)) {
  //         for (i <- vlenb / 2 until vlenb) {
  //           table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
  //           when(table_idx(i) <= vlmul) {
  //             access_table(table_idx(i)) := 1.U
  //           }
  //         }
  //       }.otherwise {
  //         for (i <- 0 until vlenb / 2) {
  //           table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
  //           when(table_idx(i) <= vlmul) {
  //             access_table(table_idx(i)) := 1.U
  //           }
  //         }
  //       }
  //     }.elsewhen(vrgather16_sew64) {
  //       when(uop_idx(1, 0) === 0.U) {
  //         for (i <- 0 until vlenb / 4) {
  //           table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
  //           when(table_idx(i) <= vlmul) {
  //             access_table(table_idx(i)) := 1.U
  //           }
  //         }
  //       }.elsewhen(uop_idx(1, 0) === 1.U) {
  //         for (i <- vlenb / 4 until vlenb / 2) {
  //           table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
  //           when(table_idx(i) <= vlmul) {
  //             access_table(table_idx(i)) := 1.U
  //           }
  //         }
  //       }.elsewhen(uop_idx(1, 0) === 2.U) {
  //         for (i <- vlenb / 2 until 3 * vlenb / 4) {
  //           table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
  //           when(table_idx(i) <= vlmul) {
  //             access_table(table_idx(i)) := 1.U
  //           }
  //         }
  //       }.otherwise {
  //         for (i <- 3 * vlenb / 4 until vlenb) {
  //           table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
  //           when(table_idx(i) <= vlmul) {
  //             access_table(table_idx(i)) := 1.U
  //           }
  //         }
  //       }
  //     }.otherwise {
  //       for (i <- 0 until vlenb) {
  //         when(vrgather_vxi) {
  //           table_idx(i) := vs1(63, 0) >> shift
  //         }.otherwise {
  //           table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
  //         }
  //         when(table_idx(i) <= Mux(vrgather16_sew8, 3.U, vlmul)) {
  //           access_table(table_idx(i)) := 1.U
  //         }
  //       }
  //     }
  //     access_table
  //   }
  //
  //   def sof(table: UInt): (UInt, UInt) = {
  //     val flag = Wire(Vec(8, UInt(1.W)))
  //     val sbf = Wire(Vec(8, UInt(1.W)))
  //     val index = Wire(Vec(8, UInt(4.W)))
  //     val sif = Cat(Cat(sbf.reverse)(6, 0), 1.U)
  //     val sof = ~Cat(sbf.reverse) & sif
  //
  //     for (i <- 0 until 8) {
  //       if (i == 0) {
  //         flag(i) := table(i)
  //         sbf(i) := Mux(table(0).asBool, 0.U, 1.U)
  //       } else {
  //         flag(i) := table(i) | flag(i - 1)
  //         sbf(i) := Mux(flag(i).asBool, 0.U, 1.U)
  //       }
  //     }
  //
  //     for (i <- 0 until 8 reverse) {
  //       if (i == 7)
  //         index(i) := Mux(table(i).asBool, i.U, "hF".U)
  //       else
  //         index(i) := Mux(table(i).asBool, i.U, index(i + 1))
  //     }
  //
  //     (index(0), sof)
  //   }
  //
  //   when(flush_vld) {
  //     for (i <- 0 until 8) {
  //       for (j <- 0 until 8) {
  //         access_table(i)(j) := 0.U
  //       }
  //     }
  //   }.otherwise {
  //     when(viq01_valid) {
  //       access_table(viq0_uop_idx) := access_table_gen(viq0_vs1, viq0_sew, viq0_uop_idx, vrgather_vxi, vrgather16)
  //       access_table(viq1_uop_idx) := access_table_gen(viq1_vs1, viq1_sew, viq1_uop_idx, vrgather_vxi, vrgather16)
  //     }.elsewhen(viq0_valid) {
  //       access_table(viq0_uop_idx) := access_table_gen(viq0_vs1, viq0_sew, viq0_uop_idx, vrgather_vxi, vrgather16)
  //     }.elsewhen(viq1_valid) {
  //       access_table(viq1_uop_idx) := access_table_gen(viq1_vs1, viq1_sew, viq1_uop_idx, vrgather_vxi, vrgather16)
  //     }.elsewhen(update_table) {
  //       access_table(vs_idx) := table_hi_lo
  //     }
  //   }
  //
  //   for (i <- 0 until 8) {
  //     table_hi_lo(i) := ~table_lo(i) & ~table_hi(i) & access_table(vs_idx)(i)
  //   }
  //
  //   when(viq0_valid && (viq0_uop_idx === 0.U)) {
  //     old_vd := Mux(vcompress, viq0_vs1, viq0_old_vd)
  //   }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
  //     old_vd := Mux(vcompress, viq1_vs1, viq1_old_vd)
  //   }.elsewhen(reg_vslide && rd_sent_reg(3)) {
  //     old_vd := io.in.fsm_rd_data_3
  //     //}.elsewhen(reg_vrgather && !reg_vrgather16_sew8 && vrgather_wb_vld_reg(3)) {
  //   }.elsewhen(reg_vrgather && vrgather_wb_vld_reg(3)) { //todo
  //     old_vd := io.in.fsm_rd_data_3
  //     //  }.elsewhen(reg_vrgather16_sew8 && vrgather_wb_vld_reg(3) && update_vl_cnt(0)) {
  //     //    old_vd := io.in.fsm_rd_data_3
  //   }.elsewhen(reg_vcompress && rd_sent && !rd_wb) {
  //     old_vd := old_vd >> (1.U << vsew_shift)
  //     //  }.elsewhen(reg_vrgather && vrgather_table_sent_reg(3)) {
  //     //    old_vd := Cat(vrgather_vd.reverse)
  //   }
  //
  //   when(flush_vld) {
  //     uop_cnt := 0.U
  //   }.elsewhen(rec_done) {
  //     uop_cnt := 0.U
  //   }.elsewhen(viq01_valid) {
  //     uop_cnt := uop_cnt + 2.U
  //   }.elsewhen(viq0_valid) {
  //     uop_cnt := uop_cnt + 1.U
  //   }.elsewhen(viq1_valid) {
  //     uop_cnt := uop_cnt + 1.U
  //   }
  //
  //   when(flush_vld) {
  //     rd_sent_idx_reg(4) := 0.U
  //     rd_sent_idx_reg(3) := 0.U
  //     rd_sent_idx_reg(2) := 0.U
  //     rd_sent_idx_reg(1) := 0.U
  //     rd_sent_idx_reg(0) := 0.U
  //
  //     table_lo_idx_reg(4) := 0.U
  //     table_lo_idx_reg(3) := 0.U
  //     table_lo_idx_reg(2) := 0.U
  //     table_lo_idx_reg(1) := 0.U
  //     table_lo_idx_reg(0) := 0.U
  //
  //     table_hi_idx_reg(4) := 0.U
  //     table_hi_idx_reg(3) := 0.U
  //     table_hi_idx_reg(2) := 0.U
  //     table_hi_idx_reg(1) := 0.U
  //     table_hi_idx_reg(0) := 0.U
  //
  //     update_table_reg(4) := false.B
  //     update_table_reg(3) := false.B
  //     update_table_reg(2) := false.B
  //     update_table_reg(1) := false.B
  //     update_table_reg(0) := false.B
  //
  //     vrgather_table_sent_reg(4) := false.B
  //     vrgather_table_sent_reg(3) := false.B
  //     vrgather_table_sent_reg(2) := false.B
  //     vrgather_table_sent_reg(1) := false.B
  //     vrgather_table_sent_reg(0) := false.B
  //
  //     vrgather_wb_vld_reg(4) := false.B
  //     vrgather_wb_vld_reg(3) := false.B
  //     vrgather_wb_vld_reg(2) := false.B
  //     vrgather_wb_vld_reg(1) := false.B
  //     vrgather_wb_vld_reg(0) := false.B
  //
  //     rd_sent_reg(4) := false.B
  //     rd_sent_reg(3) := false.B
  //     rd_sent_reg(2) := false.B
  //     rd_sent_reg(1) := false.B
  //     rd_sent_reg(0) := false.B
  //
  //     rd_wb_resent_reg(4) := false.B
  //     rd_wb_resent_reg(3) := false.B
  //     rd_wb_resent_reg(2) := false.B
  //     rd_wb_resent_reg(1) := false.B
  //     rd_wb_resent_reg(0) := false.B
  //
  //     rd_wb_reg(4) := false.B
  //     rd_wb_reg(3) := false.B
  //     rd_wb_reg(2) := false.B
  //     rd_wb_reg(1) := false.B
  //     rd_wb_reg(0) := false.B
  //
  //     cmprs_wb_vld_reg(4) := false.B
  //     cmprs_wb_vld_reg(3) := false.B
  //     cmprs_wb_vld_reg(2) := false.B
  //     cmprs_wb_vld_reg(1) := false.B
  //     cmprs_wb_vld_reg(0) := false.B
  //
  //     cmprs_rd_vd_sent_reg(4) := false.B
  //     cmprs_rd_vd_sent_reg(3) := false.B
  //     cmprs_rd_vd_sent_reg(2) := false.B
  //     cmprs_rd_vd_sent_reg(1) := false.B
  //     cmprs_rd_vd_sent_reg(0) := false.B
  //
  //     src_lo_valid_reg(4) := false.B
  //     src_lo_valid_reg(3) := false.B
  //     src_lo_valid_reg(2) := false.B
  //     src_lo_valid_reg(1) := false.B
  //     src_lo_valid_reg(0) := false.B
  //
  //     src_hi_valid_reg(4) := false.B
  //     rdata_vslide_hi_valid := false.B
  //     src_hi_valid_reg(2) := false.B
  //     src_hi_valid_reg(1) := false.B
  //     src_hi_valid_reg(0) := false.B
  //
  //     cmprs_rd_vd_reg(4) := false.B
  //     cmprs_rd_vd_reg(3) := false.B
  //     cmprs_rd_vd_reg(2) := false.B
  //     cmprs_rd_vd_reg(1) := false.B
  //     cmprs_rd_vd_reg(0) := false.B
  //   }.otherwise {
  //     rd_sent_idx_reg(4) := rd_sent_idx_reg(3)
  //     rd_sent_idx_reg(3) := rd_sent_idx_reg(2)
  //     rd_sent_idx_reg(2) := rd_sent_idx_reg(1)
  //     rd_sent_idx_reg(1) := rd_sent_idx_reg(0)
  //     rd_sent_idx_reg(0) := rd_sent_idx
  //
  //     table_lo_idx_reg(4) := table_lo_idx_reg(3)
  //     table_lo_idx_reg(3) := table_lo_idx_reg(2)
  //     table_lo_idx_reg(2) := table_lo_idx_reg(1)
  //     table_lo_idx_reg(1) := table_lo_idx_reg(0)
  //     table_lo_idx_reg(0) := reg_table_lo_idx
  //
  //     table_hi_idx_reg(4) := table_hi_idx_reg(3)
  //     table_hi_idx_reg(3) := table_hi_idx_reg(2)
  //     table_hi_idx_reg(2) := table_hi_idx_reg(1)
  //     table_hi_idx_reg(1) := table_hi_idx_reg(0)
  //     table_hi_idx_reg(0) := reg_table_hi_idx
  //
  //     update_table_reg(4) := update_table_reg(3)
  //     update_table_reg(3) := update_table_reg(2)
  //     update_table_reg(2) := update_table_reg(1)
  //     update_table_reg(1) := update_table_reg(0)
  //     update_table_reg(0) := update_table
  //
  //     vrgather_table_sent_reg(4) := vrgather_table_sent_reg(3)
  //     vrgather_table_sent_reg(3) := vrgather_table_sent_reg(2)
  //     vrgather_table_sent_reg(2) := vrgather_table_sent_reg(1)
  //     vrgather_table_sent_reg(1) := vrgather_table_sent_reg(0)
  //     vrgather_table_sent_reg(0) := vrgather_table_sent
  //
  //     vrgather_wb_vld_reg(4) := vrgather_wb_vld_reg(3)
  //     vrgather_wb_vld_reg(3) := vrgather_wb_vld_reg(2)
  //     vrgather_wb_vld_reg(2) := vrgather_wb_vld_reg(1)
  //     vrgather_wb_vld_reg(1) := vrgather_wb_vld_reg(0)
  //     vrgather_wb_vld_reg(0) := vrgather_wb_vld
  //
  //     rd_sent_reg(4) := rd_sent_reg(3)
  //     rd_sent_reg(3) := rd_sent_reg(2)
  //     rd_sent_reg(2) := rd_sent_reg(1)
  //     rd_sent_reg(1) := rd_sent_reg(0)
  //     rd_sent_reg(0) := rd_sent
  //
  //     rd_wb_resent_reg(4) := rd_wb_resent_reg(3)
  //     rd_wb_resent_reg(3) := rd_wb_resent_reg(2)
  //     rd_wb_resent_reg(2) := rd_wb_resent_reg(1)
  //     rd_wb_resent_reg(1) := rd_wb_resent_reg(0)
  //     rd_wb_resent_reg(0) := rd_wb_resent
  //
  //     rd_wb_reg(4) := rd_wb_reg(3)
  //     rd_wb_reg(3) := rd_wb_reg(2)
  //     rd_wb_reg(2) := rd_wb_reg(1)
  //     rd_wb_reg(1) := rd_wb_reg(0)
  //     rd_wb_reg(0) := rd_wb
  //
  //     cmprs_wb_vld_reg(4) := cmprs_wb_vld_reg(3)
  //     cmprs_wb_vld_reg(3) := cmprs_wb_vld_reg(2)
  //     cmprs_wb_vld_reg(2) := cmprs_wb_vld_reg(1)
  //     cmprs_wb_vld_reg(1) := cmprs_wb_vld_reg(0)
  //     cmprs_wb_vld_reg(0) := cmprs_wb_vld
  //
  //     cmprs_rd_vd_sent_reg(4) := cmprs_rd_vd_sent_reg(3)
  //     cmprs_rd_vd_sent_reg(3) := cmprs_rd_vd_sent_reg(2)
  //     cmprs_rd_vd_sent_reg(2) := cmprs_rd_vd_sent_reg(1)
  //     cmprs_rd_vd_sent_reg(1) := cmprs_rd_vd_sent_reg(0)
  //     cmprs_rd_vd_sent_reg(0) := cmprs_rd_vd_sent
  //
  //     src_lo_valid_reg(4) := src_lo_valid_reg(3)
  //     src_lo_valid_reg(3) := src_lo_valid_reg(2)
  //     src_lo_valid_reg(2) := src_lo_valid_reg(1)
  //     src_lo_valid_reg(1) := src_lo_valid_reg(0)
  //     src_lo_valid_reg(0) := rd_sent_src_lo_valid
  //
  //     src_hi_valid_reg(4) := rdata_vslide_hi_valid
  //     rdata_vslide_hi_valid := src_hi_valid_reg(2)
  //     src_hi_valid_reg(2) := src_hi_valid_reg(1)
  //     src_hi_valid_reg(1) := src_hi_valid_reg(0)
  //     src_hi_valid_reg(0) := rd_sent_src_hi_valid
  //
  //     cmprs_rd_vd_reg(4) := cmprs_rd_vd_reg(3)
  //     cmprs_rd_vd_reg(3) := cmprs_rd_vd_reg(2)
  //     cmprs_rd_vd_reg(2) := cmprs_rd_vd_reg(1)
  //     cmprs_rd_vd_reg(1) := cmprs_rd_vd_reg(0)
  //     cmprs_rd_vd_reg(0) := cmprs_rd_vd
  //   }
  //
  //   val lo_min = Mux(table_lo_idx_reg(3) === "hf".U, "hff".U, Cat(table_lo_idx_reg(3), 0.U(4.W)))
  //   val lo_max = Mux(table_lo_idx_reg(3) === "hf".U, "hff".U, Cat((table_lo_idx_reg(3) + 1.U), 0.U(4.W)))
  //   val hi_min = Mux(table_hi_idx_reg(3) === "hf".U, "hff".U, Cat(table_hi_idx_reg(3), 0.U(4.W)))
  //   val hi_max = Mux(table_hi_idx_reg(3) === "hf".U, "hff".U, Cat((table_hi_idx_reg(3) + 1.U), 0.U(4.W)))
  //
  //   when(flush_vld) {
  //     update_table_cnt := 0.U
  //   }.elsewhen(vrgather_table_sent_reg(3) && vrgather_wb_vld_reg(3)) {
  //     update_table_cnt := 0.U
  //   }.elsewhen(vrgather_table_sent_reg(3)) {
  //     update_table_cnt := update_table_cnt + 1.U
  //   }
  //
  //
  //   for (i <- 0 until vlenb) {
  //     vrgather_byte_sel(i) := 0.U
  //     vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
  //     // vslideup_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
  //     vslideup_vd(i) := old_vd(i * 8 + 7, i * 8)
  //     //vslidedn_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
  //     vslidedn_vd(i) := old_vd(i * 8 + 7, i * 8)
  //   }
  //
  //   for (i <- 0 until vlenb / 2) {
  //     vrgather_byte_sel(i) := 0.U
  //     when(reg_vrgather_vxi) {
  //       vrgather_byte_sel(i) := Cat(vs1_preg_idx.reverse)
  //       when(vsew_reg === 1.U) {
  //         vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(1.W)) + i.U % 2.U
  //       }.elsewhen(vsew_reg === 2.U) {
  //         vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(2.W)) + i.U % 4.U
  //       }.elsewhen(vsew_reg === 3.U) {
  //         vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(3.W)) + i.U % 8.U
  //       }
  //     }.otherwise {
  //       when(vs1_type === 0.U) {
  //         vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
  //       }.elsewhen(vs1_type === 1.U) {
  //         when((vsew_reg === 0.U) && !update_vl_cnt(0)) {
  //           vrgather_byte_sel(i) := vs1((i + 1) * 16 - 1, i * 16)
  //         }.elsewhen(vsew_reg === 1.U) {
  //           vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
  //         }.elsewhen(vsew_reg === 2.U) {
  //           when(update_vl_cnt(0).asBool) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
  //           }.otherwise {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
  //           }
  //         }.elsewhen(vsew_reg === 3.U) {
  //           when(update_vl_cnt(1, 0) === 0.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
  //           }.elsewhen(update_vl_cnt(1, 0) === 1.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
  //           }.elsewhen(update_vl_cnt(1, 0) === 2.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
  //           }.elsewhen(update_vl_cnt(1, 0) === 3.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + i.U % 8.U
  //           }
  //         }
  //       }.elsewhen(vs1_type === 2.U) {
  //         vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
  //       }.elsewhen(vs1_type === 3.U) {
  //         vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
  //       }
  //     }
  //   }
  //
  //   for (i <- (vlenb / 2) until vlenb) {
  //     vrgather_byte_sel(i) := 0.U
  //     when(reg_vrgather_vxi) {
  //       vrgather_byte_sel(i) := Cat(vs1_preg_idx.reverse)
  //       when(vsew_reg === 1.U) {
  //         vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(1.W)) + i.U % 2.U
  //       }.elsewhen(vsew_reg === 2.U) {
  //         vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(2.W)) + i.U % 4.U
  //       }.elsewhen(vsew_reg === 3.U) {
  //         vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(3.W)) + i.U % 8.U
  //       }
  //     }.otherwise {
  //       when(vs1_type === 0.U) {
  //         vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
  //       }.elsewhen(vs1_type === 1.U) {
  //         when((vsew_reg === 0.U) && update_vl_cnt(0)) {
  //           vrgather_byte_sel(i) := vs1((i + 1 - vlenb / 2) * 16 - 1, (i - vlenb / 2) * 16)
  //         }.elsewhen(vsew_reg === 1.U) {
  //           vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
  //         }.elsewhen(vsew_reg === 2.U) {
  //           when(update_vl_cnt(0).asBool) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
  //           }.otherwise {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
  //           }
  //         }.elsewhen(vsew_reg === 3.U) {
  //           when(update_vl_cnt(1, 0) === 0.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
  //           }.elsewhen(update_vl_cnt(1, 0) === 1.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
  //           }.elsewhen(update_vl_cnt(1, 0) === 2.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
  //           }.elsewhen(update_vl_cnt(1, 0) === 3.U) {
  //             vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + i.U % 8.U
  //           }
  //         }
  //       }.elsewhen(vs1_type === 2.U) {
  //         vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
  //       }.elsewhen(vs1_type === 3.U) {
  //         vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
  //       }
  //     }
  //   }
  //
  //   when(reg_vrgather && !reg_vrgather16_sew8 && vrgather_table_sent_reg(3)) {
  //     for (i <- 0 until vlenb) {
  //       vrgather_vd(i) := Mux(first_gather, old_vd((i + 1) * 8 - 1, i * 8), vd_reg((i + 1) * 8 - 1, i * 8))
  //       when(vmask_byte_strb(i).asBool) {
  //         when((vrgather_byte_sel(i) >= lo_min) && (vrgather_byte_sel(i) < lo_max)) {
  //           vrgather_vd(i) := vs2_lo_bytes(vrgather_byte_sel(i) - lo_min)
  //         }.elsewhen((vrgather_byte_sel(i) >= hi_min) && (vrgather_byte_sel(i) < hi_max)) {
  //           vrgather_vd(i) := vs2_hi_bytes(vrgather_byte_sel(i) - hi_min)
  //         }.elsewhen(first_gather) {
  //           vrgather_vd(i) := 0.U
  //         }
  //       }.otherwise {
  //         vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
  //       }
  //     }
  //   }.elsewhen(reg_vrgather16_sew8 && vrgather_table_sent_reg(3) && !update_vl_cnt(0)) {
  //     for (i <- 0 until vlenb) {
  //       vrgather_vd(i) := Mux(first_gather, old_vd((i + 1) * 8 - 1, i * 8), vd_reg((i + 1) * 8 - 1, i * 8))
  //     }
  //
  //     for (i <- 0 until vlenb / 2) {
  //       when(vmask_byte_strb(i).asBool) {
  //         when((vrgather_byte_sel(i) >= lo_min) && (vrgather_byte_sel(i) < lo_max)) {
  //           vrgather_vd(i) := vs2_lo_bytes(vrgather_byte_sel(i) - lo_min)
  //         }.elsewhen((vrgather_byte_sel(i) >= hi_min) && (vrgather_byte_sel(i) < hi_max)) {
  //           vrgather_vd(i) := vs2_hi_bytes(vrgather_byte_sel(i) - hi_min)
  //         }.elsewhen(first_gather) {
  //           vrgather_vd(i) := 0.U
  //         }
  //       }.otherwise {
  //         vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
  //       }
  //     }
  //   }.elsewhen(reg_vrgather16_sew8 && vrgather_table_sent_reg(3) && update_vl_cnt(0)) {
  //     for (i <- 0 until vlenb) {
  //       vrgather_vd(i) := vd_reg((i + 1) * 8 - 1, i * 8)
  //     }
  //
  //     for (i <- vlenb / 2 until vlenb) {
  //       when(vmask_byte_strb(i).asBool) {
  //         when((vrgather_byte_sel(i) >= lo_min) && (vrgather_byte_sel(i) < lo_max)) {
  //           vrgather_vd(i) := vs2_lo_bytes(vrgather_byte_sel(i) - lo_min)
  //         }.elsewhen((vrgather_byte_sel(i) >= hi_min) && (vrgather_byte_sel(i) < hi_max)) {
  //           vrgather_vd(i) := vs2_hi_bytes(vrgather_byte_sel(i) - hi_min)
  //         }.elsewhen(first_gather) {
  //           vrgather_vd(i) := 0.U
  //         }
  //       }.otherwise {
  //         vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
  //       }
  //     }
  //   }
  //
  //   // vcompress
  //   val vd_idx_plus1 = Wire(UInt(4.W))
  //   vd_idx_plus1 := Cat(0.U(1.W), vd_idx) + 1.U
  //
  //   when(flush_vld) {
  //     one_sum := 0.U
  //   }.elsewhen(cmprs_rd_vd_sent_reg(3) || calc_done || !perm_busy) {
  //     one_sum := 0.U
  //     // }.elsewhen(rd_sent_reg(3) && !rd_wb_resent_reg(3)) { //todo
  //   }.elsewhen(rd_sent_reg(3) && !rd_wb_reg(3)) {
  //     one_sum := one_sum + current_vs_ones_sum
  //   }
  //
  //   cmprs_update_vd_idx := cmprs_wb_vld_reg(4)
  //
  //
  //   base := Cat(update_vl_cnt, 0.U(4.W))
  //
  //   val current_uop_ones_sum = Wire(Vec(vlenb, UInt(5.W)))
  //
  //   for (i <- 0 until vlenb) {
  //     current_uop_ones_sum(i) := 0.U
  //     current_ones_sum(i) := one_sum
  //     when(rd_sent_reg(3)) {
  //       current_uop_ones_sum(i) := PopCount(Cat(vmask_byte_strb.reverse)(i, 0))
  //       current_ones_sum(i) := one_sum + current_uop_ones_sum(i)
  //     }
  //   }
  //
  //   for (i <- 0 until vlenb) {
  //     cmprs_vd(i) := vd_reg(i * 8 + 7, i * 8)
  //     res_idx(i) := 0.U
  //     res_valid(i) := false.B
  //   }
  //
  //   for (i <- 0 until vlenb) {
  //     when(rd_sent_reg(3)) {
  //       when(cmprs_rd_vd_sent_reg(3) && (i.U >= one_sum(3, 0))) {
  //         cmprs_vd(i) := Mux(ta_reg, "hff".U, io.in.fsm_rd_data_3(i * 8 + 7, i * 8))
  //       }.otherwise {
  //         res_idx(i) := current_ones_sum(i) - base - 1.U
  //         res_valid(i) := current_ones_sum(i) >= base + 1.U
  //         when((vmask_byte_strb(i) === 1.U) && res_valid(i) && (res_idx(i) < vlenb.U)) {
  //           cmprs_vd(res_idx(i)) := vs2_lo_bytes(i)
  //         }
  //       }
  //     }
  //   }

}

object VerilogPer extends App {
  println("Generating the VPU Permutation FSM hardware")
  emitVerilog(new permutation(), Array("--target-dir", "build/vifu"))
}


