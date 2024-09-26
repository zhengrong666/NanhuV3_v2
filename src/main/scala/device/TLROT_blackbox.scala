package device_rot

import chisel3._
import chisel3.RawModule
// import Chisel._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import chisel3.experimental._
import freechips.rocketchip.regmapper.{RegField, RegFieldAccessType, RegFieldDesc, RegFieldGroup}

object RoTparam {
   val RoTIntrNum = 18
   val RoTBusWith = 32
}


class TLROT_top extends BlackBox with HasBlackBoxResource {
  val bundleParams  = TLBundleParameters(
  addressBits = 32,
  dataBits = 32,
  sourceBits = 8,
  sinkBits = 8,
  sizeBits = 3,
  echoFields = Seq(),
  requestFields = Seq(),
  responseFields = Seq(),
  hasBCE = false 
)
  val TL_AW = UInt(32.W)
  val TL_DW = UInt(32.W)
  val TL_AIW = UInt(8.W)
  val TL_DIW = UInt(1.W)
  val TL_DBW = UInt(4.W)
  val TL_SZW = UInt(2.W)

  val TL_DW64 = UInt(64.W)
  val TL_DBW64 = UInt(8.W)
  val TL_SZW64 = UInt(3.W)

  val io = IO(new Bundle {
    val clk_i = Input(Clock())
    // val rst_ni = Input(AsyncReset())
    val rst_ni = Input(Bool())
    val ROMInitEn = Output(Bool())
    val scan_mode = Input(Bool()) 
    val intr_rot_o = Output(UInt(RoTparam.RoTIntrNum.W))   

    val a_valid = Input(Bool())
    val a_bits_opcode = Input(UInt(3.W))
    val a_bits_param = Input(UInt(3.W))
    val a_bits_size = Input(TL_SZW)
    val a_bits_source = Input(TL_AIW)
    val a_bits_address = Input(TL_AW)
    val a_bits_mask = Input(TL_DBW)
    val a_bits_data = Input(TL_DW)
    val a_ready = Output(Bool())

    val d_valid = Output(Bool())
    val d_bits_opcode = Output(UInt(3.W))
    val d_bits_param = Output(UInt(3.W))
    val d_bits_size = Output(TL_SZW)
    val d_bits_source = Output(TL_AIW)
    val d_bits_sink = Output(TL_DIW)
    val d_bits_data = Output(TL_DW)
    val d_bits_denied = Output(Bool())
    val d_ready = Input(Bool())

  })

  addResource("/TLROT/TLROT_top.sv")
  // addResource("/tlul_pkg.sv")
}



class TLROT_blackbox(implicit p: Parameters) extends LazyModule {
  // val device = new SimpleDevice("tlrot", Seq("sifive,dtim0"))
  val beatBytes = 4

  val tlrotDevice = new SimpleDevice("tlrot", Seq("sifive,tlrot0"))
  val tlrotResource = Resource(tlrotDevice, "tlrot")


  // Create a TLManagerNode
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0x3b100000, 0x2fffff)), 
    // resources = device.reg("mem"),
    resources = Seq(
      // tlrotAddr, 
      tlrotResource
    ),
    regionType         = RegionType.IDEMPOTENT,
    supportsGet        = TransferSizes(1, beatBytes),
    supportsPutFull    = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    fifoId             = Some(0))),
     beatBytes)))

  //  // Create a TLManagerNode
  // val beatBytes_rom = 8
  // val node_rom = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
  //   address = Seq(AddressSet(0x3b200000, 0x07ffff)), 
  //   // resources = device.reg("mem"),
  //   resources = Seq(
  //     // tlrotAddr, 
  //     tlrotResource_rom
  //   ),
  //   regionType         = RegionType.IDEMPOTENT,
  //   supportsGet        = TransferSizes(beatBytes_rom, beatBytes_rom),
  //   // supportsPutFull    = TransferSizes(1, beatBytes_rom),
  //   // supportsPutPartial = TransferSizes(1, beatBytes_rom),
  //   fifoId             = Some(0))),
  //    beatBytes_rom))) 

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
  // lazy val module = new LazyModuleImp(this) {
    val io_rot = IO(new Bundle { 
      val clock = Input(Clock())
      // val reset = Input(AsyncReset())
      val reset = Input(Bool())
      val intr = Output(UInt(RoTparam.RoTIntrNum.W))
      val ROMInitEn = Output(Bool())
      val scan_mode = Input(Bool())
      // val intr = Output(new interruptIO)

  })
    val (in, edge) = node.in(0)
    dontTouch(in)

    dontTouch(io_rot.reset)

    // val rot = withClock(clock) {
    //   // val tlrot = Module(new TLROT)
    //    Module(new TLROT)
    //  }    
    val tlrot = Module(new TLROT_top)
    in.a.ready := tlrot.io.a_ready
    // tlrot.io.a_ready := in.a.ready
    tlrot.io.a_valid := in.a.valid
    tlrot.io.a_bits_opcode := in.a.bits.opcode
    tlrot.io.a_bits_param := in.a.bits.param
    tlrot.io.a_bits_size := in.a.bits.size
    tlrot.io.a_bits_source := in.a.bits.source
    tlrot.io.a_bits_address := in.a.bits.address
    tlrot.io.a_bits_mask := in.a.bits.mask
    tlrot.io.a_bits_data := in.a.bits.data

    // dontTouch(in.d.ready)
    // in.d.ready :=  tlrot.io.d_ready
    // tlrot.io.d_valid :=  in.d.valid
    in.d.valid := tlrot.io.d_valid
    tlrot.io.d_ready := in.d.ready
    in.d.bits.opcode := tlrot.io.d_bits_opcode
    in.d.bits.param := tlrot.io.d_bits_param
    in.d.bits.size := tlrot.io.d_bits_size
    in.d.bits.source := tlrot.io.d_bits_source
    in.d.bits.sink := tlrot.io.d_bits_sink
    in.d.bits.data := tlrot.io.d_bits_data
    in.d.bits.denied := tlrot.io.d_bits_denied

    io_rot.intr := tlrot.io.intr_rot_o
    

    // in.a <> tlrot.io.tl_i
    // in.d <> tlrot.io.tl_o
    // in.d.ready := tlrot.io.tl_o.ready 
    tlrot.io.clk_i := io_rot.clock
    io_rot.ROMInitEn := tlrot.io.ROMInitEn
    tlrot.io.scan_mode := io_rot.scan_mode

    tlrot.io.rst_ni := io_rot.reset
  }
}

