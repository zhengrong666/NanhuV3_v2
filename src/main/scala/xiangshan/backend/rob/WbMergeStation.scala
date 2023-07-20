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
    if mask = 8'b1111_1111  => can write back
    else                    => store it in here
---------------------------------------------------------------------------------------*/

package xiangshan.backend.rob

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import utils._
import xs.utils._
import xiangshan._

class WbMergeStationEntry(implicit p: Parameters) extends XSBundle {
    val wbmask = UInt(8.W)
    val robIdx = UInt(log2Up(RobSize).W)

    def matchRobIdx(id: UInt): Bool = {
        robIdx === id
    }

    def mergeReq(mask: UInt): UInt = {
        wbmask := wbmask | mask
        wbmask
    }
}

class WbMergeStation(val size: Int, val widthIn: Int, val widthOut: Int)(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle {
        val redirect = new Redirect
        val in = Vec(widthIn, Input(new ExuOutput))
        val out = Vec(widthOut, Output(new ExuOutput))
    })

    class WbMergeStationPtr extends CircularQueuePtr[WbMergeStationPtr](size)
    object WbMergeStationPtr {
        def apply(f: Boolean, v: Int): WbMergeStationPtr = {
            val ptr = Wire(new WbMergeStationPtr)
            ptr.flag := f.B
            ptr.value := v.U
            ptr
        }
    }

    val wbEntry = Mem(size, new WbMergeStationEntry)

    val headPtr = RegInit(WbMergeStationPtr(false, 0))
    val tailPtr = RegInit(WbMergeStationPtr(false, 0))

    //match
    
    //
}