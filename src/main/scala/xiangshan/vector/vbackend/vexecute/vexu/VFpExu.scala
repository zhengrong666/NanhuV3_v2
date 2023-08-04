package xiangshan.vector.vbackend.vexecute.vexu
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import darecreek.exu.fu2.fp.VFPUWrapper
import xiangshan.{HasXSParameter, Narrow}
import xiangshan.backend.execute.exu.{BasicExu, BasicExuImpl, ExuConfig, ExuInputNode, ExuOutputNode, ExuType}
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.vector.HasVectorParameters
import xiangshan.vector.vbackend.vexecute.vfu.uopToVuop
class VFpExu(id:Int, complexName:String)(implicit p: Parameters) extends BasicExu{
  private val cfg = ExuConfig(
    name = "VFpExu",
    id = id,
    complexName = complexName,
    fuConfigs = Seq(FuConfigs.vfpCfg),
    exuType = ExuType.vfp,
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

    private val vfp = Module(new VFPUWrapper)

    private val vuop = uopToVuop(iss.bits.uop, p)
    private val src0 = iss.bits.src(0)
    private val src1 = iss.bits.src(1)
    private val src2 = iss.bits.src(2)
    private val mask = iss.bits.vm

    iss.ready := vfp.io.in.ready
    vfp.io.in.valid := iss.valid && iss.bits.uop.ctrl.fuType === cfg.fuConfigs.head.fuType
    vfp.io.in.bits.uop := vuop
    vfp.io.in.bits.vs1 := src0
    vfp.io.in.bits.vs2 := src1
    vfp.io.in.bits.rs1 := src0(XLEN - 1, 0)
    vfp.io.in.bits.oldVd := src2
    vfp.io.in.bits.mask := mask
    vfp.io.out.ready := true.B

    when(iss.valid){assert(vfp.io.in.valid)}

    wb.valid := vfp.io.out.valid
    wb.bits := DontCare
    wb.bits.uop := vfp.io.out.bits.uop.sysUop
    wb.bits.data := vfp.io.out.bits.vd
    wb.bits.fflags := vfp.io.out.bits.fflags

    private val uopOut = vfp.io.out.bits.uop.sysUop
    private val isNarrow = uopOut.ctrl.narrow === Narrow.Narrow
    private val lowHalf = !uopOut.uopIdx(0)
    private val highHalf = uopOut.uopIdx(0)
    private val maskLen = VLEN / 8
    private val halfMaskLen = maskLen / 2

    private def ones(in: Int): UInt = ((1 << in) - 1).U(in.W)

    private val lowHalfMask = Cat(0.U(halfMaskLen.W), ones(halfMaskLen))
    private val highHalfMask = Cat(ones(halfMaskLen), 0.U(halfMaskLen.W))
    private val fullMask = ones(maskLen)
    private val finalMask = MuxCase(fullMask, Seq(
      (isNarrow && lowHalf) -> lowHalfMask,
      (isNarrow && highHalf) -> highHalfMask,
    ))
    wb.bits.wakeupMask := finalMask
    wb.bits.writeDataMask := finalMask
  }
}
