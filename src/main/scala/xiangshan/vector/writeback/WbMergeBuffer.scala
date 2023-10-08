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

/*--------------------------------------------------------------------------------------
    Author: GMX
    Date: 2023-06-28
    email: guanmingxing@bosc.ac.cn

    Vector Instruction writeback merge
    if mask = 8'b1111_1111  => can write back
    else                    => store it in here
---------------------------------------------------------------------------------------*/

package xiangshan.vector.writeback

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._

import utils._
import xs.utils._
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.rob._
import freechips.rocketchip.diplomacy._
import os.stat

class WbMergeBufferPtr(size: Int) extends CircularQueuePtr[WbMergeBufferPtr](size) with HasCircularQueuePtrHelper
object WbMergeBufferPtr {
  def apply(f: Boolean, v: Int, size: Int): WbMergeBufferPtr = {
    val ptr = Wire(new WbMergeBufferPtr(size))
    ptr.flag := f.B
    ptr.value := v.U
    ptr
  }
}

class WbMergeBuffer(size: Int = 64, allocateWidth: Int = 4, mergeWidth: Int = 4, wbWidth: Int = 4)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val waitqueueAlloc = Vec(allocateWidth, DecoupledIO(new WbMergeBufferPtr(size)))
    val rob = Vec(wbWidth, ValidIO(new ExuOutput))
    val exu = Vec(mergeWidth, Flipped(ValidIO(new ExuOutput)))
    //from WaitQueue
    val vmbInit = Flipped(ValidIO(new MicroOp))
    val wbExceptionGen = Flipped(Valid(new ExuOutput))
  })

  val allocatePtr = RegInit(WbMergeBufferPtr(false, 0, size))
  val writebackPtr  = RegInit(WbMergeBufferPtr(false, 0, size))

  val mergeTable = Reg(Vec(size, new ExuOutput))

  val s_free :: s_alloc :: s_wb :: Nil = Enum(3)
  val stateVec = RegInit(VecInit(Seq.fill(size)(s_free)))
  
  val mergeCnt = RegInit(VecInit(Seq.fill(size)(0.U(8.W))))

  val exceptionHappen = RegInit(false.B)
  val exceptionPtr = RegInit(WbMergeBufferPtr(false, 0, size))

  //allocate, connect with WaitQueue
  val cancelVec = Wire(Vec(size, Bool()))

  io.waitqueueAlloc.zipWithIndex.foreach {
    case (port, i) => {
      val allocPtr = allocatePtr + i.U
      val allocPtrOH = allocPtr.toOH
      port.bits := allocPtr
      val allocState = Mux1H(allocPtrOH, stateVec)
      port.valid := allocState === s_free
    }
  }

  val exceptionHandle = RegNext(RegNext((io.vmbInit.valid && io.vmbInit.bits.cf.exceptionVec.asUInt =/= 0.U))) || io.wbExceptionGen.valid
  val vmException_s1 = RegNext(io.vmbInit)
  val s1_low = vmException_s1.valid && (vmException_s1.bits.mergeIdx < exceptionPtr)
  val exceptionHere = Wire(ValidIO(new MicroOp))
  val exceptionRead = Mux1H(exceptionPtr.toOH, mergeTable)
  exceptionHere.bits := exceptionRead.uop
  exceptionHere.valid := exceptionHappen
  val s1_res = RegNext(Mux(s1_low, vmException_s1, exceptionHere))

  when(exceptionHandle) {
    exceptionHappen := exceptionHappen || exceptionHandle
    exceptionPtr := Mux(io.wbExceptionGen.valid && io.wbExceptionGen.bits.uop.mergeIdx < s1_res.bits.mergeIdx, io.vmbInit.bits.mergeIdx, s1_res.bits.mergeIdx)
  }

  mergeTable.zip(cancelVec).zip(stateVec).zipWithIndex.foreach {
    case (((e, cancel), s), i) => {
      val flushValid = (e.uop.robIdx < io.redirect.bits.robIdx) || (e.uop.robIdx === io.redirect.bits.robIdx && io.redirect.bits.flushItself)
      cancel := s =/= s_free && flushValid && io.redirect.valid
      val wen = io.vmbInit.valid && (io.vmbInit.bits.mergeIdx.value === i.U)
      when(wen) {
        e.uop := io.vmbInit.bits
        mergeCnt(i) := 0.U
        s := s_alloc
      }
    }
  }

  val allocNum = PopCount(io.waitqueueAlloc.map(_.fire))
  val cancelNum = PopCount(cancelVec)
  allocatePtr := Mux(io.redirect.valid, allocatePtr - cancelNum, allocatePtr + allocNum)

  for((e, i) <- mergeTable.zipWithIndex) {
    val needMerge = io.exu.map(wb => wb.bits.uop.mergeIdx.value === i.U && wb.valid && (wb.bits.uop.cf.exceptionVec.asUInt =/= 0.U))
    val cntNext = mergeCnt(i) + PopCount(needMerge)
    mergeCnt(i) := cntNext
    when(stateVec(i) === s_alloc && cntNext === e.uop.uopNum) {
      stateVec(i) := s_wb
    }
  }

  //select writeback, connect with ROB, and release mergeIdx to freeIdxQueue
  val wbPtrVec = Wire(Vec(wbWidth, new WbMergeBufferPtr(size)))
  wbPtrVec.zipWithIndex.foreach {
    case (wb, i) => {
      wb := writebackPtr + i.U
    }
  }

  val wbVec = Wire(Vec(wbWidth, Bool()))

  wbVec.zip(wbPtrVec).zipWithIndex.foreach {
    case ((wb, ptr), i) => {
      val ptrOH = ptr.toOH
      val s = Mux1H(ptrOH, stateVec)
      if(i == 0) {
        wb := (s === s_wb) || (exceptionHappen && exceptionPtr === ptr)
      } else {
        val frontWb = WireInit(VecInit(wbVec.take(i)))
        wb := (s === s_wb) && frontWb.asUInt.andR
      }
    }
  }

  for((port, i) <- io.rob.zipWithIndex) {
    val entry = mergeTable(writebackPtr.value + i.U)
    port.bits   := entry
    port.valid  := wbVec(i)
  }

  stateVec.zipWithIndex.foreach {
    case (s, i) => {
      val wbHit = WireInit(VecInit(io.rob.map(wb => wb.bits.uop.mergeIdx.value === i.U && wb.valid)))
      when(wbHit.asUInt.orR) {
        s := s_free
      }
    }
  }

  writebackPtr := writebackPtr + PopCount(wbVec)
}
