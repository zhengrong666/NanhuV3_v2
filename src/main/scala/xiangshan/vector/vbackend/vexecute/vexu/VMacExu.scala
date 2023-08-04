package xiangshan.vector.vbackend.vexecute.vexu
import chipsalliance.rocketchip.config.Parameters
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
    exuType = ExuType.vmac
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new BasicExuImpl(this) with HasXSParameter with HasVectorParameters {
    require(issueNode.in.length == 1)
    require(writebackNode.out.length == 1)
    def latency = 3
    private val iss = issueNode.in.head._1.issue
    private val wb = writebackNode.out.head._1
    iss.ready := true.B

    private val vmac = Module(new VMac)
    private val uopShiftQueue = Module(new MicroOpShiftQueue(latency))

    private val vuop = uopToVuop(iss.bits.uop, p)
    private val src0 = iss.bits.src(0)
    private val src1 = iss.bits.src(1)
    private val src2 = iss.bits.src(2)
    private val mask = iss.bits.vm

    uopShiftQueue.io.in.valid := iss.valid
    uopShiftQueue.io.in.bits := iss.bits.uop
    uopShiftQueue.io.redirect := redirectIn

    vmac.io.in.valid := iss.valid && iss.bits.uop.ctrl.fuType === cfg.fuConfigs.head.fuType
    vmac.io.in.bits.uop := vuop
    vmac.io.in.bits.vs1 := src0
    vmac.io.in.bits.vs2 := src1
    vmac.io.in.bits.rs1 := src0(XLEN - 1, 0)
    vmac.io.in.bits.oldVd := src2
    vmac.io.in.bits.mask := mask

    when(iss.valid){assert(vmac.io.in.valid)}

    wb.valid := uopShiftQueue.io.out.valid
    wb.bits := DontCare
    wb.bits.uop := uopShiftQueue.io.out.bits
    wb.bits.data := vmac.io.out.bits.vd
    wb.bits.vxsat := vmac.io.out.bits.vxsat
    wb.bits.wakeupMask := ((1 << (VLEN / 8)) - 1).U((VLEN / 8).W)
    wb.bits.writeDataMask := ((1 << (VLEN / 8)) - 1).U((VLEN / 8).W)
  }
}
