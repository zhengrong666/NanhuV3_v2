package xiangshan.vector.vbackend.vregfile
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.vector.vbackend.vregfile.VRegfileTopUtil.GenLoadVrfMask
import xiangshan.{ExuOutput, XSBundle, XSModule}
import xs.utils.{UIntToMask, ZeroExt}

class MoveReq(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  private val addrBits = coreParams.vectorParameters.vPhyRegIdxWidth
  val srcAddr = UInt(addrBits.W)
  val dstAddr = UInt(addrBits.W)
  val agnostic = Bool()
  val enable = Bool()
  val sew = UInt(2.W)
  val segIdx = UInt(log2Ceil(VLEN).W)
}

class VrfReadPort(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val addr = Input(UInt(log2Up(VRFSize).W))
  val data = Output(UInt(VLEN.W))
}

class VRegfile(wbWkpNum:Int, wbNoWkpNum:Int, readPortNum:Int)(implicit p: Parameters) extends XSModule {
  private val size = coreParams.vectorParameters.vPhyRegsNum
  private val addrBits = coreParams.vectorParameters.vPhyRegIdxWidth
  private val maskWidth = VLEN / 8
  val io = IO(new Bundle{
    val wbWakeup = Input(Vec(wbWkpNum, Valid(new ExuOutput)))
    val wbNoWakeup = Input(Vec(wbNoWkpNum, Valid(new ExuOutput)))
    val wakeupMask = Output(Vec(wbWkpNum, UInt(maskWidth.W)))
    val moveOldValReqs = Input(Vec(loadUnitNum, Valid(new MoveReq)))
    val readPorts = Vec(readPortNum, new VrfReadPort)
    val vecAllocPregs = Vec(vectorParameters.vRenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))

    val debug = if(env.EnableDifftest || env.AlwaysBasicDiff) {Some(Vec(32, new Bundle {
      val addr = Input(UInt(PhyRegIdxWidth.W))
      val data = Output(UInt(VLEN.W))
    }))} else {
      None
    }
  })
  private val mrfReadNum = wbWkpNum
  private val mrfWriteNum = wbWkpNum + loadUnitNum + vectorParameters.vRenameWidth
  private val vrfReadNum = if(env.EnableDifftest || env.AlwaysBasicDiff) {
    readPortNum + loadUnitNum + 32
  } else {
    readPortNum + loadUnitNum
  }
  private val vrfWriteNum = wbWkpNum + wbNoWkpNum + loadUnitNum
  private val mrf = Module(new MaskRegfile(size, maskWidth, maskWidth, mrfReadNum, mrfWriteNum))
  private val vrf = Module(new MaskRegfile(size, VLEN, maskWidth, vrfReadNum, vrfWriteNum))
  private val fullMask = Fill(maskWidth, 1.U(1.W))
  private val emptyMask = Fill(maskWidth, 0.U(1.W))

  // read vector register file
  for (r <- io.readPorts) {
    r.data := vrf(r.addr)
  }

  // write vector register file
  for (i <- 0 until wbWkpNum) {
    val addr = io.wbWakeup(i).bits.uop.pdest(addrBits - 1 ,0)
    val data = io.wbWakeup(i).bits.data
    val wbMask = io.wbWakeup(i).bits.writeDataMask
    val wkpMask = io.wbWakeup(i).bits.wakeupMask
    val wen = io.wbWakeup(i).valid && io.wbWakeup(i).bits.uop.ctrl.vdWen
    vrf.write(addr, data, wbMask, wen)
    mrf.write(addr, fullMask, wkpMask, wen)
    // wakeup
    val wbAddrReg = RegEnable(addr, io.wbWakeup(i).valid)
    io.wakeupMask(i) := mrf(wbAddrReg)
    when(io.wbWakeup(i).valid){assert(io.wbWakeup(i).bits.uop.pdest < size.U)}
  }
  // not wakeup
  for (i <- 0 until wbNoWkpNum) {
    val addr = io.wbNoWakeup(i).bits.uop.pdest
    val data = io.wbNoWakeup(i).bits.data
    vrf.write(addr, data, fullMask, io.wbNoWakeup(i).valid)
    when(io.wbNoWakeup(i).valid){assert(io.wbNoWakeup(i).bits.uop.pdest < size.U)}
  }

  private def GenMoveMask(in:MoveReq):UInt = {
    val width = VLEN / 8
    val vlenBytes = log2Ceil(VLEN / 8)
    val sew = in.sew
    val movIdx = VrfHelper.getElementIdx(in.segIdx, in.sew, VLEN)
    val mask = MuxCase(0.U, Seq(
      (sew === 0.U) -> ("h01".U << Cat(movIdx(vlenBytes - 1, 0), 0.U(0.W))),
      (sew === 1.U) -> ("h03".U << Cat(movIdx(vlenBytes - 2, 0), 0.U(1.W))),
      (sew === 2.U) -> ("h0f".U << Cat(movIdx(vlenBytes - 3, 0), 0.U(2.W))),
      (sew === 3.U) -> ("hff".U << Cat(movIdx(vlenBytes - 4, 0), 0.U(3.W))),
    ))
    mask(width - 1, 0).asUInt
  }
  // Move request
  for (i <- 0 until loadUnitNum) {
    val mReq = io.moveOldValReqs(i)
    val mask = Mux(mReq.bits.enable, emptyMask, fullMask)
    val dst = mReq.bits.dstAddr
    val srcData = Mux(mReq.bits.agnostic, Fill(VLEN, 1.U(1.W)), vrf(mReq.bits.srcAddr))
    val wm = Wire(UInt(maskWidth.W))
    wm := GenMoveMask(mReq.bits)
    vrf.write(dst, srcData, wm, mReq.valid)
    mrf.write(dst, mask, wm, mReq.valid)
  }

  io.vecAllocPregs.foreach(va => {
    mrf.write(va.bits, emptyMask, fullMask, va.valid)
  })

  //difftest read
  if (io.debug.isDefined) {
    io.debug.get.foreach(r => {
      r.data := vrf(r.addr)
    })
  }
}