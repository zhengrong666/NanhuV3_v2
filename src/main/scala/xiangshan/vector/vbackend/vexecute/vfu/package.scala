package xiangshan.vector.vbackend.vexecute
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import darecreek.exu.fu2.VUop
import xiangshan.{MicroOp, Narrow, Widen}
package object vfu {
  def uopToVuop(src:MicroOp, valid:Bool, p:Parameters):VUop = {
    val res = Wire(new VUop()(p))
    res.ctrl.funct6 := src.ctrl.funct6
    res.ctrl.funct3 := src.ctrl.funct3
    res.ctrl.vm := src.ctrl.vm
    res.ctrl.vs1_imm := src.ctrl.imm(4, 0)
    res.ctrl.widen := src.ctrl.widen === Widen.Widen
    res.ctrl.widen2 := src.ctrl.widen === Widen.Widen2
    res.ctrl.narrow := src.ctrl.narrow === Narrow.Narrow
    res.ctrl.narrow_to_1 := src.ctrl.narrow === Narrow.Narrow2
    res.info.ma := src.vCsrInfo.vma(0)
    res.info.ta := src.vCsrInfo.vta(0)
    res.info.vsew := src.vCsrInfo.vsew
    res.info.vlmul := src.vCsrInfo.vlmul
    res.info.vl := src.vCsrInfo.vl
    res.info.vstart := DontCare
    res.info.vxrm := DontCare
    res.info.frm := DontCare
    res.uopIdx := src.uopIdx(2,0)
    res.uopEnd := src.uopIdx(2,0) === src.uopNum
    res.sysUop := src
    when(valid){assert(src.uopIdx <= 7.U)}
    when(valid){assert(src.uopNum <= 7.U)}
    res
  }
}
