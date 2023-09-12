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
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import firrtl.annotations.Annotation
import firrtl.transforms.NoDedupAnnotation
import xiangshan.{MicroOp, Redirect, XSModule}
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, LogicShiftRight}
sealed class TwoEntryQueuePtr extends CircularQueuePtr[TwoEntryQueuePtr](entries = 2) with HasCircularQueuePtrHelper
sealed class PipelineEnqBundle(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val uop: MicroOp = new MicroOp
  val selectResp: SelectResp = new SelectResp(bankIdxWidth, entryIdxWidth)
}
sealed class PipelineDeqBundle(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends Bundle{
  val uop: MicroOp = new MicroOp
  val bankIdxOH: UInt = UInt(bankIdxWidth.W)
  val entryIdxOH: UInt = UInt(entryIdxWidth.W)
}
class DecoupledPipeline(bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val enq = Flipped(DecoupledIO(new PipelineEnqBundle(bankIdxWidth, entryIdxWidth)))
    val deq = DecoupledIO(new PipelineDeqBundle(bankIdxWidth, entryIdxWidth))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })
  //ready should be true all the time
  io.enq.ready := io.deq.ready

  private val deqValidDriverReg = RegNext(io.enq.valid, false.B)
  private val deqDataDriverReg = RegEnable(io.enq.bits.selectResp, io.enq.valid)
  private val shouldBeCanceled = deqDataDriverReg.info.lpv.zip(io.earlyWakeUpCancel).map({case(l,c) => l(0) && c}).reduce(_||_)
  io.deq.valid := deqValidDriverReg && !shouldBeCanceled
  io.deq.bits.uop := io.enq.bits.uop
  io.deq.bits.bankIdxOH := deqDataDriverReg.bankIdxOH
  io.deq.bits.entryIdxOH := deqDataDriverReg.entryIdxOH
  io.deq.bits.uop.robIdx := deqDataDriverReg.info.robPtr
  io.deq.bits.uop.psrc := deqDataDriverReg.info.psrc
  io.deq.bits.uop.lpv.zip(deqDataDriverReg.info.lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
  when(io.deq.valid){assert(io.deq.ready)}
  private val mySelf = this
  chisel3.experimental.annotate(new ChiselAnnotation {
    override def toFirrtl: Annotation = NoDedupAnnotation(mySelf.toTarget)
  })
}