package xiangshan.vector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import xiangshan._
import utils._

//TODO: Vector Micro OP interface align
//fake interface
class VectorMicroOP(implicit p: Parameters) extends VectorBaseBundle {
    def typeJudge(sel_type: UInt): Bool = sel_type === 1.U
}

// package xiangshan.vector.virename

// import xiangshan.vector._
// import chipsalliance.rocketchip.config.Parameters
// import chisel3._
// import chisel3.util._
// import xiangshan._
// import utils._

// class VIRenameReq(implicit p: Parameters) extends Bundle {
//     val lvs1    = UInt(5.W)
//     val lvs2    = UInt(5.W)
//     val lvd     = UInt(5.W)
//     //TODO: WaitQueue port aligning...
//     val en      = Bool()
// }

// class VIWaitQueueToRenameBundle(implicit p: Parameters) extends VIRenameBundle {
//     val renameReq   = Input(Vec(VIRenameWidth, new VIRenameReq))
//     val vtypeValue  = Input(UInt(64.W))
//     val robIdx      = Input(UInt(log2Up(RobSize).W))
// }
