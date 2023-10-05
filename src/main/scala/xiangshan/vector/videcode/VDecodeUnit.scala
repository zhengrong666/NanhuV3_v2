package xiangshan.vector.videcode
import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.vector._
import xiangshan._
trait VDecodeUnitConstants
{
  // abstract out instruction decode magic numbers
  val VD_MSB = 11
  val VD_LSB = 7
  val VS1_MSB = 19
  val VS1_LSB = 15
  val VS2_MSB = 24
  val VS2_LSB = 20
  val F6_MSB = 31
  val F6_LSB = 26
  val F3_MSB = 14
  val F3_LSB = 12
  val VM_LSB = 25
  val NF_MSB = 31
  val NF_LSB = 29
}
class VDecodeUnit(implicit p: Parameters) extends XSModule with VDecodeUnitConstants {
  val io = IO(new Bundle{
    val uopIn = Input(new MicroOp)
    val uopOut = Output(new MicroOp)
  })

  private val decodeTable = VLDecode.table ++ VSDecode.table ++ VADecode.table ++
    VWDecode.table ++ VNDecode

  private val uop = WireInit(io.uopIn)
  uop.vctrl := Wire(new CtrlSignals()).decodev(io.uopIn.cf.instr, decodeTable)
  uop.ctrl.lsrc(2) := io.uopIn.cf.instr(VD_MSB, VD_LSB)
  uop.ctrl.ldest := io.uopIn.cf.instr(VD_MSB, VD_LSB)
  uop.ctrl.srcType(2) := Mux(io.uopIn.ctrl.vdWen, SrcType.vec, SrcType.DC)
  uop.vctrl.funct6 := io.uopIn.cf.instr(F6_MSB, F6_LSB)
  uop.vctrl.funct3 := io.uopIn.cf.instr(F3_MSB, F3_LSB)
  uop.vctrl.nf := io.uopIn.cf.instr(NF_MSB, NF_LSB)
  uop.vctrl.vm := !io.uopIn.cf.instr(VM_LSB)

  when(io.uopIn.ctrl.selImm =/= SelImm.X) {
    uop.ctrl.imm := io.uopIn.cf.instr(VS1_MSB, VS1_LSB)
  }
  io.uopOut := uop
}