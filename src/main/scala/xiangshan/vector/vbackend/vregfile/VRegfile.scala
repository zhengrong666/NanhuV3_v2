package xiangshan.vector.vbackend.vregfile
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{ExuOutput, XSBundle, XSModule}

class MoveReq(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val srcAddr = UInt(log2Up(VRFSize).W)
  val dstAddr = UInt(log2Up(VRFSize).W)
  val ma = Bool()
  val ta = Bool()
  val tailMask = UInt((VLEN/8).W)
  val prestartMask = UInt((VLEN/8).W)
}

class VrfReadPort(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val addr = Input(UInt(log2Up(VRFSize).W))
  val data = Output(UInt(VLEN.W))
}

class VrfWriteSignals(implicit p: Parameters) extends XSBundle{
  val addr = UInt(PhyRegIdxWidth.W)
  val data = Vec(VLEN / 8, UInt(8.W))
  val mask = UInt((VLEN / 8).W)
}

class MrfWriteSignals(implicit p: Parameters) extends XSBundle{
  val addr = UInt(PhyRegIdxWidth.W)
  val data = UInt((VLEN / 8).W)
}

class VRegfile(wbWkpNum:Int, wbNoWkpNum:Int, readPortNum:Int)(implicit p: Parameters) extends XSModule {
  private val size = coreParams.vectorParameters.vPhyRegsNum
  private val dataWidth = VLEN
  private val maskWidth = VLEN / 8
  val io = IO(new Bundle{
    val loadWb = Input(Vec(loadUnitNum, Valid(new ExuOutput)))
    val wbWakeup = Input(Vec(wbWkpNum, Valid(new ExuOutput)))
    val wbNoWakeup = Input(Vec(wbNoWkpNum, Valid(new ExuOutput)))
    val wakeups = Output(Vec(wbWkpNum, Valid(new ExuOutput)))
    val moveOldValReqs = Input(Vec(loadUnitNum, Valid(new MoveReq)))
    val readPorts = Vec(readPortNum, new VrfReadPort)
    val clear = Vec(vectorParameters.vRenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
  })
  
  
  private val mrf = Mem(size, UInt(maskWidth.W))
  private val vrf = Mem(size, Vec(maskWidth, UInt(8.W)))

  // read vector register file
  for (r <- io.readPorts) {
    r.data := vrf(r.addr)
  }
  // write vector register file
  for (i <- 0 until wbWkpNum) {
    val addr = io.wbWakeup(i).bits.uop.pdest
    val data = io.wbWakeup(i).bits.data.asTypeOf(Vec(dataWidth / 8, UInt(8.W)))
    val dataMask = io.wbWakeup(i).bits.writeDataMask.asBools
    val wkpMask = mrf(addr) | io.wbWakeup(i).bits.writeDataMask
    when (io.wbWakeup(i).valid) {
      vrf.write(addr, data, dataMask)
      mrf(addr) := Mux(wkpMask.andR, 0.U, wkpMask)
    }
    // wakeup
    io.wakeups(i).valid := io.wbWakeup(i).valid && wkpMask.andR
    io.wakeups(i).bits := io.wbWakeup(i).bits
  }
  for(c <- io.clear){
    when(c.valid){
      mrf(c.bits) := 0.U
    }
  }
  // not wakeup
  for (i <- 0 until wbNoWkpNum) {
    val addr = io.wbNoWakeup(i).bits.uop.pdest
    val data = io.wbNoWakeup(i).bits.data.asTypeOf(Vec(dataWidth / 8, UInt(8.W)))
    val dataMask = ((1 << maskWidth) - 1).U(maskWidth.W).asBools
    when(io.wbNoWakeup(i).valid) {
      vrf.write(addr, data, dataMask)
    }
  }
  // Move request
  for (i <- 0 until loadUnitNum) {
    val mReq = io.moveOldValReqs(i)
    when (mReq.valid) {
      val dst = io.moveOldValReqs(i).bits.dstAddr
      val srcData = WireInit(vrf.read(mReq.bits.srcAddr))
      val dstMask = mrf(io.moveOldValReqs(i).bits.dstAddr)
      val ma = mReq.bits.ma
      val ta = mReq.bits.ta
      val tm = mReq.bits.tailMask
      val mm = (~dstMask).asUInt | (~mReq.bits.tailMask).asUInt | (~mReq.bits.prestartMask).asUInt
      for((d,i) <- srcData.zipWithIndex){
        when(ta && tm(i) || ma && mm(i)){
          d := ((1 << d.getWidth ) - 1).U
        }
      }
      val wm = (~dstMask).asUInt | mReq.bits.tailMask | mReq.bits.prestartMask
      val new_mask = dstMask | mReq.bits.tailMask | mReq.bits.prestartMask
      vrf.write(dst, srcData, wm.asBools)
      mrf.write(dst, new_mask)
    }
  }
}