package xiangshan.vector.vbackend.vissue.vrs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSModule
class VrsIssueArbiter(val bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val unorderedIn = Flipped(Decoupled(new VRSSelectResp(bankIdxWidth, entryIdxWidth)))
    val orderedIn = Flipped(Decoupled(new VRSSelectResp(bankIdxWidth, entryIdxWidth)))
    val orderedCtrl = Input(Valid(new OIQEntry))
    val orderedChosen = Output(Bool())
    val out = Decoupled(new VRSSelectResp(bankIdxWidth, entryIdxWidth))
  })
  private val finalOrderedIn = Wire(Valid(new VRSSelectResp(bankIdxWidth, entryIdxWidth)))
  finalOrderedIn.valid := io.orderedIn.valid && io.orderedCtrl.valid &&
    io.orderedIn.bits.info.robPtr === io.orderedCtrl.bits.robIdx &&
    io.orderedIn.bits.info.uopIdx === io.orderedCtrl.bits.uopIdx

  finalOrderedIn.bits := io.orderedIn.bits

  private val validVec = Cat(finalOrderedIn.valid, io.unorderedIn.valid)
  private val sel = WireInit(false.B)
  when(validVec === "b01".U){
    sel := true.B
  }.elsewhen(validVec === "b10".U){
    sel := false.B
  }.elsewhen(validVec === "b11".U){
    sel := io.unorderedIn.bits.info.robPtr < finalOrderedIn.bits.info.robPtr
  }

  io.out.valid := validVec.orR
  io.out.bits := Mux(sel, io.unorderedIn.bits, finalOrderedIn.bits)
  io.orderedChosen := io.out.fire && !sel

  io.unorderedIn.ready := io.out.ready && sel
  io.orderedIn.ready := io.out.ready && !sel
}
