package xiangshan.vector.vbackend.vexecute
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, LazyModuleImpLike}
import xiangshan.{HasXSParameter, Redirect, SrcType, XSBundle, XSModule}
import xiangshan.backend.execute.exu.{ExuConfig, ExuOutputNode, ExuType}
import xiangshan.backend.execute.fu.FuConfigs
import xiangshan.vector.vbackend.vexecute.vfu.permutation.Permutation
import xiangshan.vector.vbackend.vissue.vprs.{VpReservationStation, VprsIssueBundle}

class PermutationRegfileReadPort(implicit p: Parameters) extends XSBundle{
  val vreq = Output(Valid(UInt(PhyRegIdxWidth.W)))
  val vresp = Input(Valid(UInt(VLEN.W)))

  val sreq = Output(new Bundle{
    val addr = UInt(PhyRegIdxWidth.W)
    val isReg = Bool()
    val isFp = Bool()
  })
  val sresp = Input(UInt(XLEN.W))
}

class VectorPermutationBlock(implicit p: Parameters) extends LazyModule{
  private val cfg = ExuConfig(
    name = "VectorPermutationExu",
    id = 0,
    complexName = "VectorPermuationComplex",
    fuConfigs = Seq(FuConfigs.vpermCfg),
    exuType = ExuType.vperm
  )
  val writebackNode = new ExuOutputNode(cfg)
  val vprs = LazyModule(new VpReservationStation)
  lazy val module = new LazyModuleImp(this) with HasXSParameter {
    val io = IO(new Bundle{
      val redirect = Input(Valid(new Redirect))
      val intAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
      val fpAllocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
      val vecAllocPregs = Vec(vectorParameters.vRenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
      val rfReadPort = new PermutationRegfileReadPort
    })

    vprs.module.io.redirect := io.redirect
    vprs.module.io.intAllocPregs := io.intAllocPregs
    vprs.module.io.fpAllocPregs := io.fpAllocPregs
    vprs.module.io.vecAllocPregs := io.vecAllocPregs

    private val permutationFu = Module(new Permutation)
    io.rfReadPort.sreq.addr := vprs.module.io.issue.bits.prs
    io.rfReadPort.sreq.isReg := vprs.module.io.issue.bits.prsType === SrcType.reg
    io.rfReadPort.sreq.isFp := vprs.module.io.issue.bits.prsType === SrcType.fp

    private val issueDataReg = Reg(new VprsIssueBundle)
    private val issueScalarDataReg = Reg(UInt(XLEN.W))
    private val issueValidReg = RegInit(false.B)
    private val allowPipe = !issueValidReg || permutationFu.io.out.perm_busy
    when(issueValidReg && issueDataReg.uop.robIdx.needFlush(io.redirect)){
      issueValidReg := false.B
    }.elsewhen(allowPipe){
      issueValidReg := vprs.module.io.issue.valid
    }
    when(allowPipe && vprs.module.io.issue.valid){
      issueDataReg := vprs.module.io.issue.bits
      issueScalarDataReg := io.rfReadPort.sresp
    }
    vprs.module.io.issue.ready := allowPipe
  }
}
