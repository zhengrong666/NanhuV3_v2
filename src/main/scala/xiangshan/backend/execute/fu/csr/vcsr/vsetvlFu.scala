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
    Date: 2023-07-27
    email: guanmingxing@bosc.ac.cn

---------------------------------------------------------------------------------------*/

package xiangshan.backend.execute.fu.csr.vcsr

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import utils._
import xiangshan._

import xiangshan.vector._
import utility.MaskedRegMap

class VSetVlFu(implicit p: Parameters) extends VectorBaseModule with HasVCSRConst {
    val io = IO(new Bundle {
        val in = Flipped(ValidIO(new ExuInput))
        val out = ValidIO(ExuOutput)
    })

    val src1 = io.in.src(0)
    val src2 = io.in.src(1)
    val rd  = io.in.uop.pdest

    //vsetvli
    val AVL = src1
    

}
