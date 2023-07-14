package xiangshan.vector.vbackend.vregfile
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{ExuOutput, XSBundle, XSModule}

class MoveReq(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val srcAddr = UInt(log2Up(VRFSize).W)
  val moveMask = UInt((VLEN/8).W)
  val agnostic = Bool()
  val dstAddr = UInt(log2Up(VRFSize).W)
}

class VrfReadPort(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val addr = Input(UInt(log2Up(VRFSize).W))
  val data = Output(UInt(VLEN.W))
}

class VRegfile(wbWkpNum:Int, wbNoWkpNum:Int, readPortNum:Int)(implicit p: Parameters) extends XSModule {
  private val size = coreParams.vectorParameters.vPhyRegsNum
  private val dataWidth = VLEN
  private val maskWidth = VLEN / 8
  val io = IO(new Bundle{
    val wbWakeup = Input(Vec(wbWkpNum, Valid(new ExuOutput)))
    val wbNoWakeup = Input(Vec(wbNoWkpNum, Valid(new ExuOutput)))
    val wakeups = Output(Vec(wbWkpNum, Valid(new ExuOutput)))
    val moveOldValReqs = Input(Vec(loadUnitNum, Valid(new MoveReq)))
    val readPorts = Vec(readPortNum, new VrfReadPort)
  })
}
