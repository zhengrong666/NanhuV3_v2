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

package xiangshan.vector.vbackend.vcsr

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import utils._
import xiangshan._

import xiangshan.vector._

class VCSRIO(implicit p: Parameters) extends VectorBaseBundle with HasVCSRConst {
    
}

class VSETVLIFU(implicit p: Parameters) extends VectorBaseModule with HasVCSRConst {
    val io = IO(new Bundle {
        val avl = Input(UInt(XLEN.W))
        val zimm = Input(UInt(11.W))
        val rd = Output(UInt(5.W))
    })
}

class VSETIVLIFU(implicit p: Parameters) extends VectorBaseModule with HasVCSRConst {
    val io = IO(new Bundle {
        
    })
}


class VSETVLFU(implicit p: Parameters) extends VectorBaseModule with HasVCSRConst {
    val io = IO(new Bundle {
        
    })
}

class VCSR(implicit p: Parameters) extends VectorBaseModule with HasVCSRConst {
    val io = IO(new Bundle{

    })

    //csr define
    val vstart  = RegInit(UInt(XLEN.W), 0.U)
    val vxsat   = RegInit(UInt(XLEN.W), 0.U)
    val vxrm    = RegInit(UInt(XLEN.W), 0.U)
    val vcsr    = RegInit(UInt(XLEN.W), 0.U)
    val vl      = RegInit(UInt(XLEN.W), 0.U)
    val vlenb   = RegInit(UInt(XLEN.W), 0.U)

    
}
