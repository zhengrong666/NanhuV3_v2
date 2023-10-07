package xiangshan.vector.vbackend.vexecute.vexu
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import darecreek.exu.fu2.mac.VMac
import xiangshan.HasXSParameter
import xiangshan.backend.execute.exu.{BasicExu, BasicExuImpl, ExuConfig, ExuInputNode, ExuOutputNode, ExuType}
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.vector.HasVectorParameters
import xiangshan.vector.vbackend.vexecute.vfu.uopToVuop
class VMacExu(id:Int, complexName:String)(implicit p: Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "VMacExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.vmacCfg),
    exuType = ExuType.vmac,
    writebackToRob = false,
    writebackToVms = true
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new Impl
  class Impl extends BasicExuImpl(this) with HasXSParameter with HasVectorParameters {
    require(issueNode.in.length == 1)
    require(writebackNode.out.length == 1)
    val io = IO(new Bundle {
      val vstart = Input(UInt(log2Up(VLEN).W))
      val vcsr = Input(UInt(3.W))
      val frm = Input(UInt(3.W))
    })
    def latency = 3
    private val iss = issueNode.in.head._1.issue
    private val wb = writebackNode.out.head._1
    iss.ready := true.B

    private val vmac = Module(new VMac)
    private val uopShiftQueue = Module(new MicroOpShiftQueue(latency))

    private val vuop = uopToVuop(iss.bits.uop, iss.valid, io.vstart, io.vcsr(2,1), io.frm, p)
    private val src0 = iss.bits.src(0)
    private val src1 = iss.bits.src(1)
    private val src2 = iss.bits.src(2)
    private val mask = iss.bits.vm

    uopShiftQueue.io.in.valid := iss.valid && cfg.fuConfigs.map(_.fuType === iss.bits.uop.ctrl.fuType).reduce(_|_)
    uopShiftQueue.io.in.bits := iss.bits.uop
    uopShiftQueue.io.redirect := redirectIn

    vmac.io.in.valid := iss.valid && iss.bits.uop.ctrl.fuType === cfg.fuConfigs.head.fuType
    vmac.io.in.bits.uop := vuop
    vmac.io.in.bits.vs1 := src0
    vmac.io.in.bits.vs2 := src1
    vmac.io.in.bits.rs1 := src0(XLEN - 1, 0)
    vmac.io.in.bits.oldVd := src2
    vmac.io.in.bits.mask := mask

    wb.valid := uopShiftQueue.io.out.valid
    wb.bits := DontCare
    wb.bits.uop := uopShiftQueue.io.out.bits
    wb.bits.data := vmac.io.out.bits.vd
    wb.bits.vxsat := vmac.io.out.bits.vxsat
    wb.bits.wakeupMask := ((1 << (VLEN / 8)) - 1).U((VLEN / 8).W)
    wb.bits.writeDataMask := ((1 << (VLEN / 8)) - 1).U((VLEN / 8).W)
  }
}
