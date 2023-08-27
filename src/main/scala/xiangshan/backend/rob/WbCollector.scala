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

package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

import difftest._
import utils._
import xs.utils._
import xiangshan._
import xiangshan.frontend.FtqPtr
import xiangshan.backend.execute.exu.{ExuConfig, ExuType}
import xiangshan.backend.writeback._
import xiangshan.vector._

class WbCollector(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val wbNodeParamCommon = WriteBackSinkParam(name = "WbCommon", sinkType = WriteBackSinkType.rob)
  val wbNodeParamMerge  = WriteBackSinkParam(name = "WbMerge", sinkType = WriteBackSinkType.vecMs)
  val wbNodeCommon = new WriteBackSinkNode(wbNodeParamCommon)
  val wbNodeMerge = new WriteBackSinkNode(wbNodeParamMerge)
  lazy val module = new WbCollectorImp(this)
}

class WbCollectorImp(outer: WbCollector)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasVectorParameters {
    val commonWbInBundles = outer.wbNodeCommon.in.head._1
    val commonWbInParams = outer.wbNodeCommon.in.head._2._1
    val commonWbIn = commonWbInParams zip commonWbInBundles

    val mergeWbInBundles = outer.wbNodeMerge.in.head._1
    val mergeWbInParams = outer.wbNodeMerge.in.head._2._1
    val mergeWbIn = mergeWbInParams zip mergeWbInBundles

    val wbCommonNums = commonWbIn.length
    val wbMergeNums = mergeWbIn.length
}