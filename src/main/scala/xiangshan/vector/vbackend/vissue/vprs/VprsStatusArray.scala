package xiangshan.vector.vbackend.vissue.vprs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{FuType, Redirect, SrcState, SrcType, XSBundle, XSModule}
import xiangshan.backend.issue._
import xiangshan.backend.rob.RobPtr
class VprsStatusArray(implicit p: Parameters) extends XSBundle{
  val robIdx: RobPtr = new RobPtr
  val pvs1: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pvs2: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val old_vd: Vec[UInt] = Vec(8, UInt(PhyRegIdxWidth.W))
  val pvm: UInt = UInt(PhyRegIdxWidth.W)
  val srcType: Vec[UInt] = Vec(4, SrcType())
  val srcState: Vec[UInt] = Vec(4, SrcState())
}
class VPRSStatusArray {

}
