/** *************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * ************************************************************************************* */

/*--------------------------------------------------------------------------------------
    Author: GMX
    Date: 2023-07-05
    email: guanmingxing@bosc.ac.cn

    VectorDispatchWrapper contained dispatch and 3 dispatchQueues
---------------------------------------------------------------------------------------*/

package xiangshan.vector.dispatch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import xiangshan._
import utils._

import xiangshan.vector._
import xiangshan.backend.dispatch.DispatchQueue
import xiangshan.backend.dispatch.MemDispatch2Rs

class VectorDispatchReq(implicit p: Parameters) extends VectorBaseBundle {
  val canDispatch = Output(Bool())
  val uop = Vec(VIRenameWidth, Flipped(ValidIO(new MicroOp)))
}

class VectorDispatchWrapper(vecDeqNum: Int, vpDeqNum: Int, memDeqNum: Int)(implicit p: Parameters) extends VectorBaseModule {
  val io = IO(new Bundle {
    //req, from Rename
    val req = new VectorDispatchReq
    //dispatch port, connect with RS
    val toVectorCommonRS = Vec(vecDeqNum, DecoupledIO(new MicroOp))
    val toVectorPermuRS = Vec(vpDeqNum, DecoupledIO(new MicroOp))
    val toMem2RS = Vec(memDeqNum, DecoupledIO(new MicroOp))

    val redirect = Flipped(ValidIO(new Redirect))
  })

  val dispatchNetwork = Module(new VectorDispatchNetwork)
  //TODO: RS Input Width align
  val dqCommon = Module(new DispatchQueue(VectorDispatchCommonWidth, VIRenameWidth, vecDeqNum))
  val dqPermu = Module(new DispatchQueue(VectorDispatchPermuWidth, VIRenameWidth, vpDeqNum))
  val dqMem = Module(new DispatchQueue(VectorDispatchMemWidth, VIRenameWidth, memDeqNum))
  private val renameQueue = Module(new DispatchQueue(VIRenameWidth * 3, VIRenameWidth, VIRenameWidth))

  for(idx <- 0 until VIRenameWidth){
    renameQueue.io.enq.needAlloc(idx) := io.req.uop(idx).valid
    renameQueue.io.enq.req(idx) := io.req.uop(idx)
  }
  io.req.canDispatch := renameQueue.io.enq.canAccept


  //dispatch
  for ((dpNetPort, req) <- dispatchNetwork.io.fromRename.zip(renameQueue.io.deq)) {
    dpNetPort.bits := req.bits
    dpNetPort.valid := req.valid
  }

  //handshake
  val dqCommonCanAccept = dqCommon.io.enq.canAccept
  val dqPermuCanAccept = dqPermu.io.enq.canAccept
  val dqMemCanAccept = dqMem.io.enq.canAccept

  renameQueue.io.deq.foreach(_.ready := dqCommonCanAccept & dqPermuCanAccept & dqMemCanAccept)

  val dqCommonMask = dispatchNetwork.io.commonMask.asBools
  val dqPermuMask = dispatchNetwork.io.permutationMask.asBools
  val dqMemMask = dispatchNetwork.io.memMask.asBools

  dqCommon.io.enq.needAlloc := dqCommonMask
  dqPermu.io.enq.needAlloc := dqPermuMask
  dqMem.io.enq.needAlloc := dqMemMask

  dqCommon.io.redirect := io.redirect
  dqPermu.io.redirect := io.redirect
  dqMem.io.redirect := io.redirect
  renameQueue.io.redirect := io.redirect

  for ((uop, i) <- renameQueue.io.deq.zipWithIndex) {
    dqCommon.io.enq.req(i).bits := uop.bits
    dqMem.io.enq.req(i).bits := uop.bits
    dqPermu.io.enq.req(i).bits := uop.bits

    dqCommon.io.enq.req(i).valid := dqCommonMask(i) && dqPermuCanAccept && dqMemCanAccept
    dqMem.io.enq.req(i).valid := dqMemMask(i) && dqPermuCanAccept && dqCommonCanAccept
    dqPermu.io.enq.req(i).valid := dqPermuMask(i) && dqMemCanAccept && dqCommonCanAccept
  }

  io.toVectorCommonRS <> dqCommon.io.deq
  io.toVectorPermuRS <> dqPermu.io.deq
  io.toMem2RS <> dqMem.io.deq
}
