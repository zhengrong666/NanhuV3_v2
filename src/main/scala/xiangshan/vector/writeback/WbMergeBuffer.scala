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
    Date: 2023-10-10
    email: guanmingxing@bosc.ac.cn

    Vector Instruction writeback merge
    1.writeback
    ---------------------------------------------------
    if uopNum == mergeCnt   => writeback this instr to ROB
    else                    => store it in here, and mergeCnt += this cycle merge Num

    2.vector instr exception
    ---------------------------------------------------


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
import xiangshan.ExceptionVec
import xiangshan.ExceptionVec
import xiangshan.ExceptionVec

class WbMergeBufferPtr(size: Int) extends CircularQueuePtr[WbMergeBufferPtr](size) with HasCircularQueuePtrHelper
object WbMergeBufferPtr {
  def apply(f: Boolean, v: Int, size: Int): WbMergeBufferPtr = {
    val ptr = Wire(new WbMergeBufferPtr(size))
    ptr.flag := f.B
    ptr.value := v.U
    ptr
  }
}

class VecExceptionInfo(size: Int)(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val robPtr = new RobPtr
  val mergePtr = new WbMergeBufferPtr(size)
  val exceptionVec = ExceptionVec()
  val trigger = new TriggerCf
}

class WbMergeBuffer(size: Int = 64, allocateWidth: Int = 4, mergeWidth: Int = 4, wbWidth: Int = 4)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val waitqueueAlloc = Vec(allocateWidth, new Bundle {
      val mergePtr = DecoupledIO(new WbMergeBufferPtr(size))
      val robPtr = Input(new RobPtr)
    })
    val rob = Vec(wbWidth, ValidIO(new ExuOutput))
    val exu = Vec(mergeWidth, Flipped(ValidIO(new ExuOutput)))
    //from WaitQueue
    val vmbInit = Flipped(ValidIO(new MicroOp))
    val wbExceptionGen = Flipped(Valid(new ExuOutput))
  })

  val s_free :: s_alloc :: s_wb :: Nil = Enum(3)
  val stateVec = RegInit(VecInit(Seq.fill(size)(s_free)))
  val mergeTable = Reg(Vec(size, new ExuOutput))
  val mergeCnt = RegInit(VecInit(Seq.fill(size)(0.U(8.W))))

  val allocatePtr = RegInit(WbMergeBufferPtr(false, 0, size))
  val writebackPtr  = RegInit(WbMergeBufferPtr(false, 0, size))

  val exception_info = RegInit(0.U.asTypeOf(new VecExceptionInfo(size)))

  // allocate, connect with WaitQueue
  io.waitqueueAlloc.zipWithIndex.foreach {
    case (port, i) => {
      val allocPtr = (allocatePtr + i.U)
      port.mergePtr.bits := allocPtr
      val allocPtrOH = allocPtr.toOH
      val allocState = Mux1H(allocPtrOH, stateVec)
      port.mergePtr.valid := (allocState === s_free)
    }
  }

  val cancelVec = Wire(Vec(size, Bool()))
  val allocCancel = io.vmbInit.bits.robIdx.needFlush(io.redirect)
  mergeTable.zip(cancelVec).zip(stateVec).zipWithIndex.foreach {
    case (((e, cancel), s), i) => {
      val initEn = io.vmbInit.valid && (io.vmbInit.bits.mergeIdx.value === i.U) && (!allocCancel) && (s === s_free)
      cancel := (s =/= s_free) && e.uop.robIdx.needFlush(io.redirect)
      when(initEn) {
        e.uop := io.vmbInit.bits
        mergeCnt(i) := 0.U
        s := s_alloc
      }.elsewhen(cancel) {
        s := s_free
      }

      val robIdxWenVec = io.waitqueueAlloc.map(alloc => alloc.mergePtr.fire && alloc.mergePtr.bits.value === i.U)
      val robIdxWen = WireInit(VecInit(robIdxWenVec))
      val robIdxData = Mux1H(robIdxWen, io.waitqueueAlloc.map(_.robPtr))
      when(robIdxWen.asUInt.orR) {
        e.uop.robIdx := robIdxData
      }

    }
  }

  val allocNum = PopCount(io.waitqueueAlloc.map(alloc => alloc.mergePtr.fire && !alloc.robPtr.needFlush(io.redirect)))
  val cancelNum = PopCount(cancelVec)
  allocatePtr := allocatePtr + allocNum - cancelNum

  // Exception
  val exceptionFromVmbInit_delay1 = RegNext(io.vmbInit)
  val exceptionFromVmbInitVec = RegEnable(exceptionFromVmbInit_delay1.bits.cf.exceptionVec, VecInit(0.U(16.W).asBools), exceptionFromVmbInit_delay1.valid)

  val exceptionHandle = (RegNext(exceptionFromVmbInit_delay1.valid) && exceptionFromVmbInitVec.asUInt =/= 0.U) || io.wbExceptionGen.valid

  val s1_low = exceptionFromVmbInit_delay1.valid && (exceptionFromVmbInit_delay1.bits.mergeIdx < exception_info.mergePtr)

  val s1_res = Wire(new Bundle {
    val valid = Bool()
    val mergeIdx = new WbMergeBufferPtr(size)
    val exceptionVec = ExceptionVec()
    val trigger = new TriggerCf
    //val uopIdx = UInt(7.W)
  })
  s1_res.valid := exceptionFromVmbInit_delay1.valid || exception_info.valid
  when(exceptionFromVmbInit_delay1.valid && exception_info.valid) {
    s1_res.mergeIdx := Mux(exceptionFromVmbInit_delay1.bits.mergeIdx < exception_info.mergePtr, exceptionFromVmbInit_delay1.bits.mergeIdx, exception_info.mergePtr)
    s1_res.exceptionVec := Mux(exceptionFromVmbInit_delay1.bits.mergeIdx < exception_info.mergePtr, exceptionFromVmbInit_delay1.bits.cf.exceptionVec, exception_info.exceptionVec)
    s1_res.trigger := Mux(exceptionFromVmbInit_delay1.bits.mergeIdx < exception_info.mergePtr, exceptionFromVmbInit_delay1.bits.cf.trigger, exception_info.trigger)
  }.elsewhen(exceptionFromVmbInit_delay1.valid) {
    s1_res.mergeIdx := exceptionFromVmbInit_delay1.bits.mergeIdx
    s1_res.exceptionVec := exceptionFromVmbInit_delay1.bits.cf.exceptionVec
    s1_res.trigger := exceptionFromVmbInit_delay1.bits.cf.trigger
  }.otherwise {
    s1_res.mergeIdx := exception_info.mergePtr
    s1_res.exceptionVec := exception_info.exceptionVec
    s1_res.trigger := exception_info.trigger
  }

  val s1_res_delay1 = RegNext(s1_res)

  when(exceptionHandle) {
    exception_info.valid := exception_info.valid || exceptionHandle
    exception_info.mergePtr := Mux(io.wbExceptionGen.valid && (io.wbExceptionGen.bits.uop.mergeIdx < s1_res_delay1.mergeIdx), io.vmbInit.bits.mergeIdx, s1_res_delay1.mergeIdx)
    when(io.wbExceptionGen.valid && (io.wbExceptionGen.bits.uop.mergeIdx < s1_res_delay1.mergeIdx)) {
      exception_info.exceptionVec := io.wbExceptionGen.bits.uop.cf.exceptionVec
      exception_info.trigger := io.wbExceptionGen.bits.uop.cf.trigger
    }.otherwise {
      exception_info.exceptionVec :=  s1_res_delay1.exceptionVec
      exception_info.trigger :=  s1_res_delay1.trigger
    }
  }

  // Writeback
  for((e, i) <- mergeTable.zipWithIndex) {
    val needMerge = io.exu.map(wb => wb.valid && (wb.bits.uop.mergeIdx.value === i.U) && (wb.bits.uop.cf.exceptionVec.asUInt === 0.U) && wb.bits.uop.robIdx.needFlush(io.redirect))
    val cntNext = mergeCnt(i) + PopCount(needMerge)
    mergeCnt(i) := cntNext
    when(stateVec(i) === s_alloc && cntNext === e.uop.uopNum) {
      stateVec(i) := s_wb
    }
    val wenVec = io.exu.map(wb => wb.bits.uop.mergeIdx.value === i.U && wb.valid)
    val wen = WireInit(VecInit(wenVec)).asUInt.orR
    val wd = Mux1H(wenVec, io.exu)
    when(wen) {
      e.vxsat := wd.bits.vxsat | e.vxsat
    }
  }

  // select writeback, connect with ROB, and release mergeIdx to freeIdxQueue
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
        wb := (s === s_wb) || (exception_info.valid && exception_info.mergePtr === ptr)
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
