package xiangshan.vector.vbackend.vexecute
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.execute.exublock.{BasicExuBlock, BasicExuBlockImp}
import freechips.rocketchip.diplomacy.LazyModule
import chisel3._
import chisel3.util._
import xiangshan.vector.vbackend.vexecute.vexucx.VectorFuComplex

class VectorBlock(implicit p:Parameters) extends BasicExuBlock{
  private val vfuCx = LazyModule(new VectorFuComplex(0))
  vfuCx.issueNode :*= issueNode
  writebackNode :=* vfuCx.writebackNode
  lazy val module = new BasicExuBlockImp(this){
    vfuCx.module.redirectIn := redirectIn
  }
}
