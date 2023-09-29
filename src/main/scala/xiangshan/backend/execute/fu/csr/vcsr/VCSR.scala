package xiangshan.backend.execute.fu.csr.vcsr

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.vector._
import xiangshan.backend.execute.fu.FuOutput
import xiangshan.backend.rob.RobPtr
import xs.utils.ZeroExt

class VtypeWbIO(implicit p: Parameters) extends VectorBaseBundle {
    val vtype = UInt(9.W)
    val vl = UInt(8.W)
    val vtypeRegIdx = UInt(log2Ceil(VIVtypeRegsNum).W)
}
class VCSRWithVtypeRenameIO(implicit p: Parameters) extends VectorBaseBundle {
  val vtypeWbToRename = ValidIO(new VtypeWbIO)
  val vtypeRead = new Bundle {
    val readEn = Output(Bool())
    val data = Flipped(ValidIO(UInt(XLEN.W)))
  }
  val vlRead = new Bundle {
    val readEn = Output(Bool())
    val data = Flipped(ValidIO(UInt(XLEN.W)))
  }
  val debug_vtype = Input(UInt(XLEN.W))
  val debug_vl = Input(UInt(XLEN.W))
}

class VCSRWithRobIO(implicit p: Parameters) extends VectorBaseBundle {
  val vstart = Flipped(ValidIO(UInt(XLEN.W)))
  val vxsat  = Flipped(ValidIO(UInt(XLEN.W)))
}

class VCsrIO(implicit p: Parameters) extends VectorBaseBundle {
  val vtype = new VCSRWithVtypeRenameIO
  val robWb = new VCSRWithRobIO
  val vstart = Output(UInt(7.W))
  val vcsr    = Output(UInt(3.W))
}

class VtypeStruct(implicit p: Parameters) extends XSBundle {
  val vill      = Bool()
  val reserved  = UInt((XLEN-9).W)
  val vma       = Bool()
  val vta       = Bool()
  val vsew      = UInt(3.W)
  val vlmul     = UInt(3.W)
  require(this.getWidth == XLEN)
}

class VSetFu(implicit p: Parameters) extends XSModule with HasXSParameter {
  val io = IO(new Bundle {
    val src         = Vec(2, Input(UInt(XLEN.W)))
    val vsetType    = Input(UInt(3.W)) //one-hot, 001,010,100->vsetivli, vsetvli, vsetvl
    val vtypeNew    = Output(UInt(XLEN.W))
    val vlNew       = Output(UInt(XLEN.W))
  })

  val (src1, src2) = (io.src(0), io.src(1))
  val uimm    = src2(4, 0)
  val zimm_ii = src2(14, 5)
  val zimm_i  = src2(10, 0)

  val avl   = Wire(UInt((log2Up(VLEN) + 1).W))
  avl := Mux(io.vsetType === "b001".asUInt, ZeroExt(uimm, 9), src1(8, 0))

  private def GenVtype(in:UInt):VtypeStruct = {
    val res = Wire(new VtypeStruct)
    res.vill := in(2, 0) === 4.U || in(5).asBool
    res.reserved := 0.U
    res.vma := Mux(res.vill, 0.U, in(7))
    res.vta := Mux(res.vill, 0.U, in(6))
    res.vsew := Mux(res.vill, 0.U, in(5, 3))
    res.vlmul := Mux(res.vill, 0.U, in(2, 0))
    res
  }
  private val iiVtype = GenVtype(zimm_ii)
  private val riVtype = GenVtype(zimm_i)
  private val rrVtype = GenVtype(src2)

  private val vtype = Mux1H(io.vsetType, Seq(iiVtype, riVtype, rrVtype))

  private val vlmul = vtype.vlmul
  private val vsew  = vtype.vsew

  private val vlenBytes = VLEN / 8
  private val vlmax = Wire(UInt(log2Ceil(vlenBytes * 8 + 1).W))
  vlmax := MuxCase(0.U, Seq(
    (vlmul === 0.U) -> ((vlenBytes * 1).U >> vsew),
    (vlmul === 1.U) -> ((vlenBytes * 2).U >> vsew),
    (vlmul === 2.U) -> ((vlenBytes * 4).U >> vsew),
    (vlmul === 3.U) -> ((vlenBytes * 8).U >> vsew),
    (vlmul === 5.U) -> ((vlenBytes / 8).U >> vsew),
    (vlmul === 6.U) -> ((vlenBytes / 4).U >> vsew),
    (vlmul === 7.U) -> ((vlenBytes / 2).U >> vsew)
  ))
  private val vl = Wire(UInt(log2Ceil(vlenBytes * 8 + 1).W))
  when(vtype.vill){
    vl := 0.U
  }.elsewhen(avl < vlmax){
    vl := avl
  }.otherwise{
    vl := vlmax
  }

  io.vtypeNew := Cat(vtype.vill, vtype.vma, vtype.vta, vtype.vsew, vtype.vlmul)
  io.vlNew := vl
}
