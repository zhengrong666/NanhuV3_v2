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
  val nf = UInt(3.W)
  val uopIdx = UInt(7.W)
}

class VrfReadPort(implicit p: Parameters) extends XSBundle{
  private val VRFSize = coreParams.vectorParameters.vPhyRegsNum
  val addr = Input(UInt(log2Up(VRFSize).W))
  val data = Output(UInt(VLEN.W))
  val en = Input(Bool())
}

class VRegfile(wbWkpNum:Int, wbNoWkpNum:Int, readPortNum:Int)(implicit p: Parameters) extends XSModule {
  private val size = coreParams.vectorParameters.vPhyRegsNum
  private val addrBits = coreParams.vectorParameters.vPhyRegIdxWidth
  private val dataWidth = VLEN
  private val maskWidth = VLEN / 8
  val io = IO(new Bundle{
    val wbWakeup = Input(Vec(wbWkpNum, Valid(new ExuOutput)))
    val wbNoWakeup = Input(Vec(wbNoWkpNum, Valid(new ExuOutput)))
    val wakeups = Output(Vec(wbWkpNum, Valid(new ExuOutput)))
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

  private val mrf = Mem(size, Vec(maskWidth, Bool()))
  private val vrf = Mem(size, Vec(maskWidth, UInt(8.W)))
  private val fullMaskVec = VecInit(Seq.fill(maskWidth)(true.B))
  private val emptyMaskVec = VecInit(Seq.fill(maskWidth)(false.B))

  // read vector register file
  for (r <- io.readPorts) {
    r.data := RegEnable(vrf(r.addr).asUInt, r.en)
  }

  //difftest read
  if(io.debug.isDefined){
    io.debug.get.foreach(r => {
      r.data := vrf(r.addr).asUInt
    })
  }

  // write vector register file
  for (i <- 0 until wbWkpNum) {
    val addr = io.wbWakeup(i).bits.uop.pdest(addrBits - 1 ,0)
    val data = io.wbWakeup(i).bits.data.asTypeOf(Vec(dataWidth / 8, UInt(8.W)))
    val wbMask = io.wbWakeup(i).bits.writeDataMask.asBools
    val wkpMask = io.wbWakeup(i).bits.wakeupMask.asBools
    when (io.wbWakeup(i).valid && io.wbWakeup(i).bits.uop.ctrl.vdWen) {
      vrf.write(addr, data, wbMask)
      mrf.write(addr, fullMaskVec, wkpMask)
    }
    // wakeup
    val wbBitsReg = RegEnable(io.wbWakeup(i).bits, io.wbWakeup(i).valid)
    val wbAddrReg = RegEnable(addr, io.wbWakeup(i).valid)
    val maskRead = mrf(wbAddrReg)
    io.wakeups(i).valid := maskRead.reduce(_&_) || wbBitsReg.uop.ctrl.rfWen || wbBitsReg.uop.ctrl.fpWen
    io.wakeups(i).bits := wbBitsReg
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

  private def GenMoveMask(in:MoveReq):UInt = {
    val width = VLEN / 8
    val vlenShiftBits = log2Ceil(VLEN / 8)
    val sew = in.sew
    val nf = in.nf
    val uopIdx = MuxCase(in.uopIdx, Seq(
      (nf === 2.U) -> in.uopIdx / 2.U,
      (nf === 3.U) -> in.uopIdx / 3.U,
      (nf === 4.U) -> in.uopIdx / 4.U,
      (nf === 5.U) -> in.uopIdx / 5.U,
      (nf === 6.U) -> in.uopIdx / 6.U,
      (nf === 7.U) -> in.uopIdx / 7.U,
      (nf === 8.U) -> in.uopIdx / 8.U,
    ))
    val mask = MuxCase(0.U, Seq(
      (sew === 0.U) -> ("h01".U << Cat(uopIdx(vlenShiftBits - 1, 0), 0.U(0.W))),
      (sew === 1.U) -> ("h03".U << Cat(uopIdx(vlenShiftBits - 2, 0), 0.U(1.W))),
      (sew === 2.U) -> ("h0f".U << Cat(uopIdx(vlenShiftBits - 3, 0), 0.U(2.W))),
      (sew === 3.U) -> ("hff".U << Cat(uopIdx(vlenShiftBits - 4, 0), 0.U(3.W))),
    ))
    mask(width - 1, 0).asUInt
  }
  // Move request
  for (i <- 0 until loadUnitNum) {
    val mReq = io.moveOldValReqs(i)
    when (mReq.valid) {
      val dst = io.moveOldValReqs(i).bits.dstAddr
      val srcData = WireInit(vrf.read(mReq.bits.srcAddr))
      val wm = Wire(UInt(maskWidth.W))
      wm := GenMoveMask(mReq.bits)
      when(mReq.bits.agnostic){
        srcData.foreach(_ := 0xffff.U)
      }
      vrf.write(dst, srcData, wm.asBools)
      val mask = Mux(io.moveOldValReqs(i).bits.enable, emptyMaskVec, fullMaskVec)
      mrf.write(dst, mask, wm.asBools)
    }
  }
  io.vecAllocPregs.foreach(va => {
    when(va.valid){
      mrf.write(va.bits, emptyMaskVec, fullMaskVec)
    }
  })
}