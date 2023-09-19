package xiangshan.vector.vbackend.vissue.vprs

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.rob.RobPtr
import xiangshan.{FuType, Redirect, XSBundle, XSModule}
import xs.utils.ParallelOperation

class VprsSelectResp(entryIdxWidth:Int)(implicit p: Parameters) extends XSBundle {
  val info = new VprsStatusArrayEntry
  val entryIdxOH = UInt(entryIdxWidth.W)
}

class VprsSelectMux(entryIdxWidth:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in0 = Input(Valid(new VprsSelectResp(entryIdxWidth)))
    val in1 = Input(Valid(new VprsSelectResp(entryIdxWidth)))
    val out = Output(Valid(new VprsSelectResp(entryIdxWidth)))
  })
  private val valid0 = io.in0.valid
  private val valid1 = io.in1.valid
  private val ptr0 = io.in0.bits.info.robPtr
  private val ptr1 = io.in1.bits.info.robPtr
  private val validVec = Cat(valid1, valid0)
  private val sel = WireInit(true.B)
  when(validVec === "b01".U) {
      sel := true.B
    }.elsewhen(validVec === "b10".U) {
      sel := false.B
    }.elsewhen(validVec === "b11".U) {
    when(ptr0 <= ptr1) {
      sel := true.B
    }.otherwise {
      sel := false.B
    }
  }

  private val res = Mux(sel, io.in0, io.in1)
  io.out := res
}

object VprsSelectMux{
  def apply(in0: Valid[VprsSelectResp], in1: Valid[VprsSelectResp], entryIdxWidth:Int, p: Parameters):Valid[VprsSelectResp] = {
    val smux = Module(new VprsSelectMux(entryIdxWidth)(p))
    smux.io.in0 := in0
    smux.io.in1 := in1
    smux.io.out
  }
}

class VprsSelector(entryNum:Int)(implicit p: Parameters) extends Module{
  private val entryIdxWidth = entryNum
  val io = IO(new Bundle{
    val in = Input(Vec(entryNum, Valid(new VprsSelectResp(entryIdxWidth))))
    val out = Output(Valid(new VprsSelectResp(entryIdxWidth)))
  })
  private val operationFunction = VprsSelectMux(_, _, entryIdxWidth, p)
  private val res  = ParallelOperation(io.in, operationFunction)
  io.out := res
}

class VprsSelectNetwork(entryNum:Int, name:Option[String] = None)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val selectInfo = Input(Vec(entryNum, Valid(new VprsStatusArrayEntry)))
    val issueInfo = Decoupled(new VprsSelectResp(entryNum))
  })
  override val desiredName:String = name.getOrElse("VprsSelectNetwork")

  private val selector = Module(new VprsSelector(entryNum))
  for(((inPort, driver), idx) <- selector.io.in.zip(io.selectInfo).zipWithIndex){
    inPort.valid := driver.valid
    inPort.bits.info := driver.bits
    inPort.bits.entryIdxOH := (1 << idx).U
  }
  io.issueInfo.valid := selector.io.out.valid && !selector.io.out.bits.info.robPtr.needFlush(io.redirect)
  io.issueInfo.bits := selector.io.out.bits
}
