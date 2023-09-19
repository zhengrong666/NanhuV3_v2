package xiangshan.vector.vbackend.vexecute.vexu
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import darecreek.exu.fu2.div.VDiv
import xiangshan.HasXSParameter
import xiangshan.backend.execute.exu.{BasicExu, BasicExuImpl, ExuConfig, ExuInputNode, ExuOutputNode, ExuType}
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.vector.HasVectorParameters
import xiangshan.vector.vbackend.vexecute.vfu.uopToVuop
class VDivExu(id:Int, complexName:String)(implicit p: Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "VDivExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.vdivCfg),
    exuType = ExuType.vdiv,
    writebackToRob = false,
    writebackToVms = true
  )
  val issueNode = new ExuInputNode(cfg)
  val writebackNode = new ExuOutputNode(cfg)

  lazy val module = new BasicExuImpl(this) with HasXSParameter with HasVectorParameters {
    require(issueNode.in.length == 1)
    require(writebackNode.out.length == 1)

    private val iss = issueNode.in.head._1.issue
    private val wb = writebackNode.out.head._1

    private val vdiv = Module(new VDiv)

    private val vuop = uopToVuop(iss.bits.uop, iss.valid, p)
    private val src0 = iss.bits.src(0)
    private val src1 = iss.bits.src(1)
    private val src2 = iss.bits.src(2)
    private val mask = iss.bits.vm

    iss.ready := vdiv.io.in.ready
    vdiv.io.in.valid := iss.valid && iss.bits.uop.ctrl.fuType === cfg.fuConfigs.head.fuType
    vdiv.io.in.bits.uop := vuop
    vdiv.io.in.bits.vs1 := src0
    vdiv.io.in.bits.vs2 := src1
    vdiv.io.in.bits.rs1 := src0(XLEN - 1, 0)
    vdiv.io.in.bits.oldVd := src2
    vdiv.io.in.bits.mask := mask
    vdiv.io.out.ready := true.B
    vdiv.io.redirect := redirectIn

    when(iss.valid){assert(vdiv.io.in.valid)}

    wb.valid := vdiv.io.out.valid
    wb.bits := DontCare
    wb.bits.uop := vdiv.io.out.bits.uop.sysUop
    wb.bits.data := vdiv.io.out.bits.vd
    wb.bits.fflags := vdiv.io.out.bits.fflags
    wb.bits.wakeupMask := ((1 << (VLEN / 8)) - 1).U((VLEN / 8).W)
    wb.bits.writeDataMask := ((1 << (VLEN / 8)) - 1).U((VLEN / 8).W)
  }
}
