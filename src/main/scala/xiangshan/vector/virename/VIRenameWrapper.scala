// /***************************************************************************************
//  * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
//  *
//  * XiangShan is licensed under Mulan PSL v2.
//  * You can use this software according to the terms and conditions of the Mulan PSL v2.
//  * You may obtain a copy of Mulan PSL v2 at:
//  *          http://license.coscl.org.cn/MulanPSL2
//  *
//  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
//  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
//  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//  *
//  * See the Mulan PSL v2 for more details.
//  ***************************************************************************************/

// /*--------------------------------------------------------------------------------------
//     Author: GMX
//     Date: 2023-07-05
//     email: guanmingxing@bosc.ac.cn

// ---------------------------------------------------------------------------------------*/

// package xiangshan.vector.virename

// import chisel3._
// import chisel3.util._
// import chipsalliance.rocketchip.config.Parameters

// import xiangshan._
// import utils._

// import xiangshan.vector._
// import xiangshan.backend.rob._

// class VIRenameWrapper(implicit p: Parameters) extends VectorBaseModule {
//     val io = IO(new Bundle {
//         val redirect = Flipped(ValidIO(new Redirect))

//         //rename in, from WaitQueue
//         val canAccept   = Output(Bool())
//         val uopIn       = Vec(VIRenameWidth, Flipped(DecoupledIO(new MicroOp)))

//         //rename out, to Dispatch
//         val uopOut = Vec(VIRenameWidth, DecoupledIO(new MicroOp))
        
//         //commit, from ROB via RobIdxQueue
//         val commit = Flipped(DecoupledIO(new VIRobIdxQueueEnqIO))
//     })

//     val rename = Module(new VIRename)

//     //when vector pipeline is walking, block all pipeline
//     val hasWalk = rename.io.hasWalk

//     //rename
//     val reqMask = Wire(UInt(VIRenameWidth.W))
//     val reqValidVec = Wire(Vec(VIRenameWidth, Bool()))
//     reqValidVec := io.uopIn.map(req => req.valid)
//     reqMask := reqValidVec.asUInt

//     val robIdxForRename = Wire(Vec(VIRenameWidth, UInt(log2Up(RobSize).W)))
//     robIdxForRename := io.uopIn.map(req => req.bits.robIdx.value)

//     val reqForRename = Wire(Vec(VIRenameWidth, new VIRenameReqLr))

//     for((req, i) <- reqForRename.zipWithIndex) {
//         req.lvs1 := io.uopIn(i).bits.ctrl.lsrc(0)
//         req.lvs2 := io.uopIn(i).bits.ctrl.lsrc(1)
//         req.lvd  := io.uopIn(i).bits.ctrl.lsrc(2)
//         req.robIdx      := io.uopIn(i).bits.robIdx.value
//         req.needRename  := io.uopIn(i).bits.canRename
//     }
//     for((port, i) <- rename.io.renameReq.zipWithIndex) {
//         port.bits := reqForRename(i)
//         port.valid := reqMask(i)
//     }


//     //io.canAccept := VecInit(rename.io.renameReq.map(_.ready)).asUInt.orR && (!hasWalk) && (!io.redirect.valid)
//     val canReanmeVec = Wire(Vec(VIRenameWidth, Bool()))
//     canReanmeVec := rename.io.renameReq.map(_.ready)
//     val canToDispatch = Wire(Vec(VIRenameWidth, Bool()))
//     canToDispatch := io.uopOut.map(_.ready)
//     io.canAccept := (!hasWalk) && (!io.redirect.valid) && canReanmeVec.asUInt.andR && canToDispatch.asUInt.andR
//     //io.canAccept := (!hasWalk) && (!io.redirect.valid)

//     (0 until VIRenameWidth).map(i => io.uopOut(i).bits  := io.uopIn(i).bits)
//     (0 until VIRenameWidth).map(i => io.uopOut(i).valid := reqMask(i))
//     val reqSrcType = Wire(Vec(VIRenameWidth, Vec(3, SrcType())))
//     reqSrcType := io.uopIn.map(req => req.bits.ctrl.srcType)

//     for((port, i) <- io.uopOut.zipWithIndex) {
//         port.bits.psrc(0) := Mux(reqSrcType(i)(0) === SrcType.vec, rename.io.renameResp(i).bits.pvs1, io.uopIn(i).bits.psrc(0))
//         port.bits.psrc(1) := Mux(reqSrcType(i)(1) === SrcType.vec, rename.io.renameResp(i).bits.pvs2, io.uopIn(i).bits.psrc(1))
//         port.bits.psrc(2) := Mux(reqSrcType(i)(2) === SrcType.vec, rename.io.renameResp(i).bits.pvd, io.uopIn(i).bits.psrc(2))
//         port.bits.vm := rename.io.renameResp(i).bits.pmask;
//     }
//     for((port, i) <- rename.io.renameResp.zipWithIndex) {
//         port.ready := io.uopOut(i).ready
//     }

//     (0 until VIRenameWidth).map(i => io.uopIn(i).ready := io.uopOut(i).ready)
//     (0 until VIRenameWidth).map(i => rename.io.renameResp(i).ready := io.uopOut(i).ready)
    

//     //commit
//     rename.io.commitReq <> io.commit
//     rename.io.redirect <> io.redirect
// }
