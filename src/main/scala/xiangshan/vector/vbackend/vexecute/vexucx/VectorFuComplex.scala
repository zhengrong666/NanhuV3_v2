package xiangshan.vector.vbackend.vexecute.vexucx
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.FuType
import xiangshan.backend.execute.exu.ExuType
import xiangshan.backend.execute.exucx.{BasicExuComplex, BasicExuComplexImp}
import xiangshan.vector.vbackend.vexecute.vexu.{VAluExu, VDivExu, VFpExu, VMacExu}

class VectorFuComplex(id: Int)(implicit p:Parameters) extends BasicExuComplex{
  private val valu = LazyModule(new VAluExu(id, "VectorFuComplex"))
  private val vdiv = LazyModule(new VDivExu(id, "VectorFuComplex"))
  private val vfp = LazyModule(new VFpExu(id, "VectorFuComplex"))
  private val vmac = LazyModule(new VMacExu(id, "VectorFuComplex"))

  private val fuSeq = Seq(valu, vdiv, vfp, vmac)

  for(fu <- fuSeq){
    fu.issueNode :*= issueNode
    writebackNode :=* fu.writebackNode
  }

  lazy val module = new BasicExuComplexImp(this, 0){
    require(issueNode.in.length == 1)
    require(issueNode.out.length == fuSeq.length)
    private val issueIn = issueNode.in.head._1
    private val fuIssPorts = issueNode.out.map(_._1)
    private val vfpIss = issueNode.out.filter(_._2._2.exuType == ExuType.vfp).head._1
    private val vdivIss = issueNode.out.filter(_._2._2.exuType == ExuType.vdiv).head._1

    fuIssPorts.foreach(_ <> issueIn)
    fuSeq.foreach(_.module.redirectIn := Pipe(redirectIn))

    issueIn.issue.ready := MuxCase(true.B, Seq(
      (issueIn.issue.bits.uop.ctrl.fuType === FuType.vfp) -> vfpIss.issue.ready,
      (issueIn.issue.bits.uop.ctrl.fuType === FuType.vdiv) -> vdivIss.issue.ready,
    ))

    private val issueFuHit = issueNode.in.head._2._2.exuConfigs.flatMap(_.fuConfigs).map(_.fuType === issueIn.issue.bits.uop.ctrl.fuType).reduce(_|_)
    when(issueIn.issue.valid){assert(issueFuHit)}
  }
}
