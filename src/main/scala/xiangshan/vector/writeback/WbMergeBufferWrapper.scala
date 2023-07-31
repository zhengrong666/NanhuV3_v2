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
    imp with diplomacy
---------------------------------------------------------------------------------------*/

package xiangshan.vector.writeback

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import utils._
import xs.utils._
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.rob._
import freechips.rocketchip.diplomacy._
import xiangshan.backend.execute.exu.ExuType

class WbMergeBufferWrapper(implicit p: Parameters) extends LazyModule with HasXSParameter {
    val wbNodeParam = WriteBackSinkParam(name = "ROB", sinkType = WriteBackSinkType.rob)
    val writebackNode = new WriteBackSinkNode(wbNodeParam)
    lazy val module = new WbMergeBufferWrapperImp(this)
}

class WbMergeBufferWrapperImp(outer: WbMergeBufferWrapper)(implicit p: Parameters) extends LazyModuleImp(outer) {
    val writebackIn = outer.writebackNode.in.head._2._1 zip outer.writebackNode.in.head._1
    val vectorWbNodes = writebackIn.filter(ExuType.vecTypes exists (e => (e == _._1.exuType)))
    val vectorWriteBack = vectorWbNodes.map(_._2)

    val bufferImp = Module(new WbMergeBuffer(vMergeBufferDepth, vMergeBufferAllocateWidth, vMergeWidth, vMergeWbWdith))

}
