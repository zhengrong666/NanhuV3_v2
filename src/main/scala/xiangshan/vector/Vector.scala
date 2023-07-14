/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package Vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.backend.rob._
import xiangshan.vector._
import xs.utils._
import xiangshan.vector.videcode._
import xiangshan.vector.vtyperename._
import xiangshan.vector.viwaitqueue._
import xiangshan.vector.virename._

class Vector(implicit p: Parameters) extends LazyModule {

  lazy val module = new VectorImp(this)

}

class VectorImp(outer: Vector)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasVectorParameters
  with HasXSParameter
{

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val cpu_halt = Output(Bool())
    //in
    //from ctrl decode
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    //from ctrl rename
    val vtypein = Vec(VIDecodeWidth, Flipped(DecoupledIO(new MicroOp))) //from rename to vtyperename
    //from ctrl rob
    val allowdeq = Vec(VIDecodeWidth, Flipped(ValidIO(new RobPtr))) //to wait queue
    val vtypewriteback = Vec(VIDecodeWidth, Flipped(ValidIO(new ExuOutput))) //to wait queue

    val redirect = Flipped(ValidIO(new Redirect))

    //out
    //to exu
    val dispatch = Vec(VIRenameWidth, DecoupledIO(new MicroOp))

  })

  val videcode = Module(new VIDecodeUnit)
  val vtyperename = Module(new VtypeRename(VIVtypeRegsNum, VIDecodeWidth, VIDecodeWidth, VIDecodeWidth))
  val waitqueue = Module(new VIWaitQueue)
  val virename = Module(new VIRenameWrapper)

  for (i <- 0 until VIDecodeWidth) {
    val DecodePipe = PipelineNext(io.in(i), videcode.io.in(i).ready,
      io.redirect.valid)
    DecodePipe.ready := videcode.io.in(i).ready
    videcode.io.in(i).valid := DecodePipe.valid
    videcode.io.in(i).bits := DecodePipe.bits
  }

  vtyperename.io.in <> io.vtypein

  videcode.io.canOut := waitqueue.io.enq.canAccept
  for (i <- 0 until VIDecodeWidth) {
    when(vtyperename.io.out(i).valid && videcode.io.out(i).valid) {
      waitqueue.io.enq.req(i).valid := videcode.io.out(i).valid
      waitqueue.io.enq.needAlloc(i) := videcode.io.out(i).valid
      val CurrentData = new VIMop
      CurrentData.MicroOp.vCsrInfo.vsew := vtyperename.io.out(i).bits.ESEW
      CurrentData.MicroOp.vCsrInfo.vlmul := vtyperename.io.out(i).bits.ELMUL
      CurrentData.MicroOp.vCsrInfo.vl := vtyperename.io.out(i).bits.VL
      CurrentData.MicroOp.robIdx := vtyperename.io.out(i).bits.robIdx
      if (vtyperename.io.out(i).bits.state == 1.U) {
        CurrentData.state := vtyperename.io.out(i).bits.state - 1.U
      } else {
        CurrentData.state := vtyperename.io.out(i).bits.state
      }
      waitqueue.io.enq.req(i).bits := CurrentData
    }
  }

  waitqueue.io.vtypeWbData <> io.vtypewriteback
  waitqueue.io.robin <> io.allowdeq
  waitqueue.io.canRename <> virename.io.canAccept

  virename.io.uopIn <> waitqueue.io.out


}

