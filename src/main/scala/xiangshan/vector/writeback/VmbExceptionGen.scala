package xiangshan.vector.writeback
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.FtqPtr
class VmbExceptionInfo(implicit p: Parameters) extends XSBundle {
  val vmbIdx = new VmbPtr
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val uopIdx = UInt(log2Ceil(VLEN).W)
  val exceptionVec = ExceptionVec()
  val trigger = new TriggerCf
  val ff = Bool()
  def has_exception: Bool = exceptionVec.asUInt.orR || trigger.canFire
}
class VmbExceptionSelectPolicy(width:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(width, Valid(new VmbExceptionInfo)))
    val out = Output(Valid(new VmbExceptionInfo))
  })
  private def isOlder(self: VmbExceptionInfo, other:VmbExceptionInfo):Bool = {
    (self.vmbIdx < other.vmbIdx) || (self.vmbIdx === other.vmbIdx && self.uopIdx < other.uopIdx)
  }
  private val onlyOne = PopCount(io.in.map(_.valid)) === 1.U
  private val oldestOHMatrix = io.in.zipWithIndex.map({ case (self, idx) =>
    io.in.zipWithIndex.filterNot(_._2 == idx).map(i => (i._1.valid && self.valid && isOlder(self.bits, i._1.bits)) ^ i._1.valid)
  })
  private val oldestOHSeq = oldestOHMatrix.map(_.reduce(_|_)).map(!_)
  private val oldestOH = Cat(oldestOHSeq.reverse)
  private val defaultValue = Cat(io.in.map(_.valid).reverse)
  io.out.valid := io.in.map(_.valid).reduce(_ | _)
  private val sel = Mux(onlyOne, defaultValue, oldestOH)
  io.out.bits := Mux1H(sel, io.in.map(_.bits))

  when(io.out.valid) {
    assert(PopCount(sel) === 1.U)
  }
}
class VmbExceptionGen(wbNum:Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle{
    val wb = Input(Vec(wbNum, Valid(new ExuOutput)))
    val vmbInit = Input(Valid(new MicroOp))
    val clean = Input(Bool())
    val redirect = Input(Valid(new Redirect))
    val current = Output(Valid(new VmbExceptionInfo))
  })
  private val currentValid = RegInit(false.B)
  private val current = Reg(new VmbExceptionInfo)
  private val wb = Wire(Vec(wbNum, Valid(new VmbExceptionInfo)))
  wb.zip(io.wb).foreach({case(w, in) =>
    w.bits.vmbIdx := in.bits.uop.mergeIdx
    w.bits.robIdx := in.bits.uop.robIdx
    w.bits.uopIdx := in.bits.uop.uopIdx
    w.bits.ftqIdx := in.bits.uop.cf.ftqPtr
    w.bits.ftqOffset := in.bits.uop.cf.ftqOffset
    w.bits.ff := in.bits.uop.vctrl.ff
    w.bits.exceptionVec := in.bits.uop.cf.exceptionVec
    w.bits.trigger.clear()
    w.bits.trigger.backendCanFire := in.bits.uop.cf.trigger.backendCanFire
    w.valid := in.valid && w.bits.has_exception
  })
  private val vmbInit = Wire(Valid(new VmbExceptionInfo))
  vmbInit.bits.vmbIdx := io.vmbInit.bits.mergeIdx
  vmbInit.bits.robIdx := io.vmbInit.bits.robIdx
  vmbInit.bits.uopIdx := 0.U
  vmbInit.bits.ftqIdx := io.vmbInit.bits.cf.ftqPtr
  vmbInit.bits.ftqOffset := io.vmbInit.bits.cf.ftqOffset
  vmbInit.bits.ff := io.vmbInit.bits.vctrl.ff
  vmbInit.bits.exceptionVec := io.vmbInit.bits.cf.exceptionVec
  vmbInit.bits.trigger.clear()
  vmbInit.valid := io.vmbInit.valid && vmbInit.bits.has_exception && !vmbInit.bits.robIdx.needFlush(io.redirect)

  private val state = Wire(Valid(new VmbExceptionInfo))
  state.valid := currentValid
  state.bits := current

  private val candidates = (wb :+ vmbInit :+ state).map(e => {
    val res = Wire(Valid(new VmbExceptionInfo))
    val validCond = e.valid && !e.bits.robIdx.needFlush(io.redirect)
    res.valid := RegNext(validCond, false.B)
    res.bits := RegEnable(e.bits, validCond)
    res
  })
  private val selectPolicy = Module(new VmbExceptionSelectPolicy(candidates.length))
  selectPolicy.io.in.zip(candidates).foreach({case(a, b) =>
    a.valid := b.valid && !b.bits.robIdx.needFlush(io.redirect)
    a.bits := b.bits
  })

  private val updateCond = selectPolicy.io.out.valid
  private val flushCurrent = current.robIdx.needFlush(io.redirect) || io.clean
  when(flushCurrent & !updateCond){
    currentValid := false.B
  }.elsewhen(updateCond){
    currentValid := true.B
  }
  when(updateCond){
    current := selectPolicy.io.out.bits
  }

  io.current.valid := currentValid
  io.current.bits := current
}
