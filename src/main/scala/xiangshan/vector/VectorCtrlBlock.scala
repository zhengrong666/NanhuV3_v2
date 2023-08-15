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
    Date: 2023-08-11
    email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.backend.issue.DqDispatchNode
import xiangshan.backend.rob._
import xiangshan.vector._
import xs.utils._
import xiangshan.vector.videcode._
import xiangshan.vector.vtyperename._
import xiangshan.vector.viwaitqueue._
import xiangshan.vector.virename._
import xiangshan.vector.dispatch._
import xiangshan.vector.writeback._

class SIRenameInfo(implicit p: Parameters) extends VectorBaseBundle  {
    val psrc = Vec(3, UInt(PhyRegIdxWidth.W))
    val pdest = UInt(PhyRegIdxWidth.W)
    val old_pdest = UInt(PhyRegIdxWidth.W)
}

class VectorCtrlBlock(vecDpWidth: Int, vpDpWidth: Int, memDpWidth: Int)(implicit p: Parameters) extends VectorBaseModule with HasXSParameter {
    val io = IO(new Bundle {
        //val hartId = Input(UInt(8.W))
        //from ctrl decode
        val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
        //from ctrl rename
        val vtypein = Vec(VIDecodeWidth, Flipped(ValidIO(new VtypeReg))) //to waitqueue
        val SIRenameIn = Vec(VIDecodeWidth, Flipped(ValidIO(new SIRenameInfo)))//to waitqueue
        //from ctrl rob
        val robPtr = Vec(VIDecodeWidth, Flipped(ValidIO(new RobPtr))) //to wait queue
        val vtypewriteback = Vec(VIDecodeWidth, Flipped(ValidIO(new ExuOutput))) //to wait queue
        val mergeIdAllocate = Vec(VIDecodeWidth, Flipped(DecoupledIO(new WbMergeBufferPtr(VectorMergeBufferDepth)))) //to wait queue
        val commit = Flipped(DecoupledIO(new VIRobIdxQueueEnqIO)) // to rename
        val redirect = Flipped(ValidIO(new Redirect))
        //from csr vstart
        val vstart = Input(UInt(7.W))

        val vDispatch = Vec(vecDpWidth, DecoupledIO(new MicroOp))
        val vpDispatch = Vec(vpDpWidth, DecoupledIO(new MicroOp))
        val vmemDispath = Vec(memDpWidth, DecoupledIO(new MicroOp))
    })

    val videcode    = Module(new VIDecodeUnit)
    val waitqueue   = Module(new VIWaitQueue)
    val virename    = Module(new VIRenameWrapper)
    val dispatch    = Module(new VectorDispatchWrapper(vecDpWidth, vpDpWidth, memDpWidth))

    for (i <- 0 until VIDecodeWidth) {
        videcode.io.in(i).ready := waitqueue.io.enq.canAccept
        val DecodePipe = PipelineNext(io.in(i), videcode.io.in(i).ready,
        io.redirect.valid)
        DecodePipe.ready := videcode.io.in(i).ready
        videcode.io.in(i).valid := DecodePipe.valid
        videcode.io.in(i).bits := DecodePipe.bits
    }

    waitqueue.io.enq <> DontCare

    videcode.io.canOut := waitqueue.io.enq.canAccept
    for (i <- 0 until VIDecodeWidth) {
        when(io.vtypein(i).valid && videcode.io.out(i).valid && io.SIRenameIn(i).valid) {
            waitqueue.io.enq.req(i).valid := videcode.io.out(i).valid
            waitqueue.io.enq.needAlloc(i) := videcode.io.out(i).valid
            val currentData = Wire(new VIMop)
            currentData.MicroOp <> videcode.io.out(i).bits
            currentData.MicroOp.pdest <> io.SIRenameIn(i).bits.pdest
            currentData.MicroOp.psrc <> io.SIRenameIn(i).bits.psrc
            currentData.MicroOp.old_pdest <> io.SIRenameIn(i).bits.old_pdest
            currentData.MicroOp.vCsrInfo <> io.vtypein(i).bits.uop.vCsrInfo
            currentData.MicroOp.robIdx := io.vtypein(i).bits.uop.robIdx
            currentData.state := io.vtypein(i).bits.state
            waitqueue.io.enq.req(i).bits := currentData
        }
    }

    
    waitqueue.io.vstart         <> io.vstart
    waitqueue.io.vtypeWbData    <> io.vtypewriteback
    waitqueue.io.robin          <> io.robPtr
    waitqueue.io.mergeId        <> io.mergeIdAllocate
    waitqueue.io.canRename      <> virename.io.canAccept
    waitqueue.io.redirect       <> io.redirect

    virename.io.redirect    <> io.redirect
    virename.io.uopIn       <> waitqueue.io.out
    virename.io.commit      <> io.commit
    
    for((rp, dp) <- virename.io.uopOut zip dispatch.io.req.uop) {
        rp.ready := dispatch.io.req.canDispatch
        dp.bits := rp.bits
        dp.valid := rp.valid
    }
    dispatch.io.redirect <> io.redirect

    io.vDispatch <> dispatch.io.toVectorCommonRS
    io.vpDispatch <> dispatch.io.toVectorPermuRS
    io.vmemDispath <> dispatch.io.toMem2RS
}
