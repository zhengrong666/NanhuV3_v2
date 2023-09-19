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

package top

import circt.stage._
import firrtl.AnnotationSeq
import firrtl.options.Shell

trait XiangShanCli { this: Shell =>
  parser.note("XiangShan Options")
}

class XiangShanShell extends Shell("xiangshan") with CLI with XiangShanCli
class XiangShanStage extends ChiselStage {
  override val shell: XiangShanShell = new XiangShanShell
}

object XiangShanStage {
  def execute
  (
    args: Array[String],
    annotations: AnnotationSeq
  ): AnnotationSeq = {
    (new XiangShanStage).execute(args, annotations)
  }
}
