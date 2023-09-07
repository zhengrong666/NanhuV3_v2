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
package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import utils.{HasPerfEvents, XSPerfHistogram}
import xiangshan._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper, ParallelPriorityEncoder, ParallelPriorityMux, UIntToMask}

class DispatchQueueIO(enqNum: Int, deqNum: Int)(implicit p: Parameters) extends XSBundle {
  val enq = new Bundle {
    // output: dispatch queue can accept new requests
    val canAccept = Output(Bool())
    // input: need to allocate new entries (for address computing)
    val needAlloc = Vec(enqNum, Input(Bool()))
    // input: actually do the allocation (for write enable)
    val req = Vec(enqNum, Flipped(ValidIO(new MicroOp)))
  }
  val deq = Vec(deqNum, DecoupledIO(new MicroOp))
  val redirect = Flipped(ValidIO(new Redirect))
  val dqFull = Output(Bool())
}

class DispatchQueuePayload(entryNum:Int, enqNum:Int, deqNum:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val w = Input(Vec(enqNum, new Bundle{
      val en: Bool = Bool()
      val addr: UInt = UInt(log2Ceil(entryNum).W)
      val data: MicroOp = new MicroOp
    }))
    val r = Vec(deqNum, new Bundle{
      val addr: UInt = Input(UInt(entryNum.W))
      val data: MicroOp = Output(new MicroOp)
    })
    val redirect: Valid[Redirect] = Input(Valid(new Redirect))
    val flushVec: UInt = Output(UInt(entryNum.W))
  })

  private val array = Reg(Vec(entryNum, new MicroOp))
  for ((mem, i) <- array.zipWithIndex) {
    val valids = io.w.map(wreq => wreq.en && wreq.addr === i.U)
    val wdata = io.w.map(_.data)
    val data = Mux1H(valids, wdata)
    when(valids.reduce(_ | _)) {
      mem := data
    }
  }

  for(r <- io.r){
    val rSel = array.indices.map(_.U === r.addr)
    r.data := Mux1H(rSel, array)
  }

  private val redirectHits = array.map(_.robIdx.needFlush(io.redirect))
  io.flushVec := Cat(redirectHits.reverse)
}

class DispatchQueue (size: Int, enqNum: Int, deqNum: Int)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val io: DispatchQueueIO = IO(new DispatchQueueIO(enqNum, deqNum))

  private class DispatchQueuePtr extends CircularQueuePtr[DispatchQueuePtr](size)

  private val payloadArray = Module(new DispatchQueuePayload(size, enqNum, deqNum))
  private val enqPtr = RegInit(0.U.asTypeOf(new DispatchQueuePtr)) //Fanout to enq logics and payloads
  private val enqPtrAux = RegInit(0.U.asTypeOf(new DispatchQueuePtr)) //Fanout to other logics
  private val deqPtrVec = Seq.tabulate(deqNum)(i => RegInit(i.U.asTypeOf(new DispatchQueuePtr)))
  private val deqPtr = deqPtrVec.head
  private val validEntriesNum = distanceBetween(enqPtr, deqPtr)
  private val emptyEntriesNum = size.U - validEntriesNum
  io.dqFull := validEntriesNum === size.U

  payloadArray.io.redirect := io.redirect
  private val enqMask = UIntToMask(enqPtrAux.value, size)
  private val deqMask = UIntToMask(deqPtr.value, size)
  private val enqXorDeq = enqMask ^ deqMask
  private val validsMask = Mux(deqPtr.value <= enqPtrAux.value, enqXorDeq, (~enqXorDeq).asUInt)
  private val redirectMask = validsMask & payloadArray.io.flushVec
  private val flushNum = PopCount(redirectMask)

  io.enq.canAccept := (PopCount(io.enq.needAlloc) <= emptyEntriesNum) && !io.redirect.valid

  private val enqAddrDelta = Wire(Vec(enqNum, UInt(log2Ceil(enqNum).W)))
  for((e,i) <- enqAddrDelta.zipWithIndex){
    if(i == 0) {
      e := 0.U
    } else {
      e := PopCount(io.enq.needAlloc.take(i))
    }
  }

  for(idx <- 0 until enqNum){
    payloadArray.io.w(idx).en := io.enq.req(idx).valid && io.enq.canAccept
    payloadArray.io.w(idx).addr := (enqPtr + enqAddrDelta(idx)).value
    payloadArray.io.w(idx).data := io.enq.req(idx).bits
  }
  private val actualEnqNum = Mux(io.enq.canAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  when(io.redirect.valid){
    enqPtr := enqPtr - flushNum
    enqPtrAux := enqPtr - flushNum
  }.elsewhen(actualEnqNum =/= 0.U){
    enqPtr := enqPtr + actualEnqNum
    enqPtrAux := enqPtr + actualEnqNum
  }

  io.deq.zipWithIndex.foreach({case(deq, idx) =>
    deq.valid := (deqPtrVec(idx) < enqPtrAux) && !io.redirect.valid
    payloadArray.io.r(idx).addr := deqPtrVec(idx).value
    deq.bits := payloadArray.io.r(idx).data
  })
  private val actualDeqNum = PopCount(io.deq.map(_.fire))
  when(actualDeqNum =/= 0.U && !io.redirect.valid){
    deqPtrVec.foreach(d => d := d + actualDeqNum)
  }

  assert(deqPtr <= enqPtrAux)
  assert(actualEnqNum <= emptyEntriesNum)
  assert(flushNum <= validEntriesNum)
  private val enqFlushNextMask = UIntToMask((enqPtr - flushNum).value, size)
  private val flushXorPresentMask = enqFlushNextMask ^ enqMask
  private val enqRollbackMask = Mux(enqPtr.value >= (enqPtr - flushNum).value, flushXorPresentMask, ~flushXorPresentMask)
  when(io.redirect.valid){assert(enqRollbackMask === redirectMask, "Redirect mask should be continuous.")}
  private val readyNum = PopCount(io.deq.map(_.ready))
  for (i <- 1 until io.deq.length) {
    when(PopCount(io.deq.map(_.valid)) > 0.U) {assert(Mux(i.U < readyNum, io.deq(i).ready === true.B, io.deq(i).ready === false.B))}
  }

  XSPerfHistogram("valid_entries_num", validEntriesNum, true.B, 0, size, size / 4)

  val perfEvents = Seq(
    ("dispatchq_in", actualEnqNum),
    ("dispatchq_out", actualDeqNum),
    ("dispatchq_out_try", PopCount(io.deq.map(_.valid))),
    ("dispatchq_1_4_valid ", validEntriesNum < (size / 4).U),
    ("dispatchq_2_4_valid ", validEntriesNum >= (size / 4).U && validEntriesNum <= (size / 2).U),
    ("dispatchq_3_4_valid ", validEntriesNum >= (size / 2).U && validEntriesNum <= (size * 3 / 4).U),
    ("dispatchq_4_4_valid ", validEntriesNum >= (size * 3 / 4).U)
  )
  generatePerfEvent()

}
