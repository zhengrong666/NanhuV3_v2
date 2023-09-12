package xiangshan.backend.issue.FpRs
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.issue.{SelectResp, WakeUpInfo}
import xiangshan.{FuType, Redirect, SrcType, XSModule}
import xs.utils.LogicShiftRight

class FmaWakeupQueue(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))
    val in = Flipped(Decoupled(new Bundle{
      val info = new WakeUpInfo
      val fpv = UInt(4.W)
    }))
    val out = Output(Valid(new WakeUpInfo))
    val earlyWakeUpCancel = Input(Vec(loadUnitNum, Bool()))
  })

  private val validVec = RegInit(VecInit(Seq.fill(4)(false.B)))
  private val bitsVec = Reg(Vec(4, new WakeUpInfo))

  private val nextValidVec = LogicShiftRight(Cat(validVec), 1)
  private val conflict = nextValidVec & io.in.bits.fpv
  io.in.ready := Mux(io.in.valid, conflict.orR, true.B)
  when(io.in.valid){assert(io.in.bits.fpv === "b1000".U || io.in.bits.fpv === "b0010".U)}

  validVec.zip(bitsVec).zipWithIndex.foreach({case((v, b), i) =>
    if(i == 0){
      val fmaEnqValid = WireInit(io.in.fire && io.in.bits.fpv === "b1000".U)
      v := fmaEnqValid
      when(fmaEnqValid){
        b := io.in.bits.info
      }
    } else {
      val shouldBeFlushed = bitsVec(i - 1).robPtr.needFlush(io.redirect)
      val shouldBeCanceled = bitsVec(i - 1).lpv.zip(io.earlyWakeUpCancel).map({ case (l, c) => l(0) && c }).reduce(_ || _)
      val shiftValid = validVec(i - 1) && !shouldBeFlushed && !shouldBeCanceled
      v := shiftValid
      when(shiftValid){
        b := bitsVec(i - 1)
        b.lpv.zip(bitsVec(i - 1).lpv).foreach({case(a,b) => a := LogicShiftRight(b, 1)})
      }
    }
  })

  private val otherEnqValid = WireInit(io.in.fire && io.in.bits.fpv === "b0010".U)
  when(otherEnqValid){
    validVec(2) := true.B
    bitsVec(2) := io.in.bits.info
  }

  io.out.valid := validVec.last
  io.out.bits := bitsVec.last

  def inputConnect(input:DecoupledIO[SelectResp], allowIn:Bool):Unit = {
    io.in.valid := input.valid && input.bits.info.fuType === FuType.fmac && allowIn
    io.in.bits.info.lpv := input.bits.info.lpv
    io.in.bits.info.destType := SrcType.fp
    io.in.bits.info.pdest := input.bits.info.pdest
    io.in.bits.info.robPtr := input.bits.info.robPtr
    io.in.bits.fpv := Mux(input.bits.info.isFma, "b1000".U, "b0010".U)
  }
}
