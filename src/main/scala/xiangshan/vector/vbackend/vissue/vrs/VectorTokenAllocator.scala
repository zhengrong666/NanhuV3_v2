package xiangshan.vector.vbackend.vissue.vrs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{ExuOutput, Redirect, XSBundle, XSModule}
import xiangshan.backend.rob.RobPtr
import xs.utils.PickOneLow

class VectorTokenAllocatorEntry(implicit p: Parameters) extends XSBundle{
  val robPtr = new RobPtr
  val uopIdx = UInt(3.W)
}
class VectorTokenAllocator(tokenNum:Int)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val alloc = Input(Valid(new VectorTokenAllocatorEntry))
    val allow = Output(Bool())
    val release = Input(Valid(new ExuOutput))
    val redirect = Input(Valid(new Redirect))
  })
  private val valids = RegInit(VecInit(Seq.fill(tokenNum)(false.B)))
  private val payload = Reg(Vec(tokenNum, new VectorTokenAllocatorEntry))

  private val emptyToken = PickOneLow(valids)
  io.allow := emptyToken.valid

  private val allocEnables = Mux(emptyToken.valid, emptyToken.bits, 0.U)
  valids.zip(payload).zip(allocEnables.asBools).foreach({
    case((v, d), en) =>
      val releaseCond0 = d.robPtr.needFlush(io.redirect)
      val releaseCond1 = io.release.valid && d.uopIdx === io.release.bits.uop.uopIdx && d.robPtr === io.release.bits.uop.robIdx
      val shouldBeReleased = v && (releaseCond0 || releaseCond1)
      when(shouldBeReleased){
        v := false.B
      }.elsewhen(io.alloc.valid && en){
        v := true.B
      }

      when(io.alloc.valid && en){
        d := io.alloc.bits
      }

      when(en){assert(!v)}
  })
}
