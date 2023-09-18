/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
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
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-07-13
 ****************************************************************************************/
package xiangshan.vector.vbackend.vissue.vprs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{MicroOp, Redirect, SrcType, XSModule}
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
sealed class TwoEntryQueuePtr extends CircularQueuePtr[TwoEntryQueuePtr](entries = 2) with HasCircularQueuePtrHelper

class VprsDecoupledPipeline(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val enq = Flipped(DecoupledIO(new VprsIssueBundle))
    val deq = DecoupledIO(new VprsIssueBundle)
  })
    val mem = Reg(Vec(2, new VprsIssueBundle))
    val enqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val deqPtr = RegInit(0.U.asTypeOf(new TwoEntryQueuePtr))
    val full = enqPtr.value === deqPtr.value && enqPtr.flag =/= deqPtr.flag
    val empty = enqPtr.value === deqPtr.value && enqPtr.flag === deqPtr.flag
    val enqFire = io.enq.fire

  private val kills = Wire(Vec(2, Bool()))
  kills.zip(mem).foreach({case(k,u) => k := u.uop.robIdx.needFlush(io.redirect)})

  io.enq.ready := !full
  private val s1_valid = !empty && !kills(deqPtr.value)
  private val s1_data = mem(deqPtr.value)
  private val s1_ready = Wire(Bool())
  private val deqFire = s1_valid && s1_ready

  when(full && kills((enqPtr - 1.U).value)) {
    enqPtr := enqPtr - 1.U
  }.elsewhen(enqFire) {
    mem(enqPtr.value) := io.enq.bits
    enqPtr := enqPtr + 1.U
  }
  when(deqFire || (!empty && kills(deqPtr.value))) {
    deqPtr := deqPtr + 1.U
  }

  private val deqValidReg = RegInit(false.B)
  private val deqBitsReg = Reg(new MicroOp)
  s1_ready := !deqValidReg || io.deq.ready || (deqValidReg && deqBitsReg.robIdx.needFlush(io.redirect))
  when(s1_ready) {
    deqValidReg := s1_valid && !s1_data.uop.robIdx.needFlush(io.redirect)
  }
  when(deqFire) {
    deqBitsReg := s1_data
  }
  io.deq.valid := deqValidReg
  io.deq.bits := deqBitsReg
  io.deq.bits.prs := s1_data.prs
  io.deq.bits.prsType := s1_data.prsType
  io.deq.bits.rsRen := s1_valid
}