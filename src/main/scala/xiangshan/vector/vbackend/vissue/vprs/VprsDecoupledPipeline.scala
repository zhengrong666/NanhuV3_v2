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

import org.chipsalliance.cde.config.Parameters
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
  private val s1Valid = RegInit(false.B)
  private val s1Bits = Reg(new VprsIssueBundle)

  private val s2Valid = RegInit(false.B)
  private val s2Bits = Reg(new VprsIssueBundle)
  private val s2Ready = Wire(Bool())

  s2Ready := !s2Valid | io.deq.ready | (s2Valid & s2Bits.uop.robIdx.needFlush(io.redirect))
  io.enq.ready := !s1Valid

  when(s1Valid) {
    s1Valid := Mux(s1Bits.uop.robIdx.needFlush(io.redirect), false.B, !s2Ready)
  }.otherwise {
    s1Valid := io.enq.valid && !io.enq.bits.uop.robIdx.needFlush(io.redirect)
  }
  when(io.enq.fire) {
    s1Bits := io.enq.bits
  }

  when(s2Ready) {
    s2Valid := s1Valid && !s1Bits.uop.robIdx.needFlush(io.redirect)
  }
  when(s1Valid & s2Ready) {
    s2Bits := s1Bits
  }

  io.deq.valid := s2Valid
  io.deq.bits := s2Bits
  io.deq.bits.prs := s1Bits.prs
  io.deq.bits.prsType := s1Bits.prsType
  io.deq.bits.rsRen := s1Valid
}