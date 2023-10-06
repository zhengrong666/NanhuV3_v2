package xiangshan.vector.vbackend.vexecute
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.execute.exublock.{BasicExuBlock, BasicExuBlockImp}
import freechips.rocketchip.diplomacy.LazyModule
import chisel3._
import chisel3.util._
import xiangshan.vector.HasVectorParameters
import xiangshan.vector.vbackend.vexecute.vexucx.VectorFuComplex

class VectorBlock(implicit p:Parameters) extends BasicExuBlock{
  private val vfuCx = LazyModule(new VectorFuComplex(0))
  vfuCx.issueNode :*= issueNode
  writebackNode :=* vfuCx.writebackNode
  lazy val module = new Impl
  class Impl extends BasicExuBlockImp(this) with HasVectorParameters{
    val io = IO(new Bundle {
      val vstart = Input(UInt(log2Up(VLEN).W))
      val vcsr = Input(UInt(3.W))
      val frm = Input(UInt(3.W))
    })
    vfuCx.module.redirectIn := redirectIn
    vfuCx.module.io.vstart := io.vstart
    vfuCx.module.io.vcsr := io.vcsr
    vfuCx.module.io.frm := io.frm
  }
}
