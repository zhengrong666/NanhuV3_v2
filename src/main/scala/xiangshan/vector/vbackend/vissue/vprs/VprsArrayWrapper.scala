package xiangshan.vector.vbackend.vissue.vprs
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.issue.{PayloadArray, WakeUpInfo}
import xiangshan.{MicroOp, Redirect, XSModule}

class VprsArrayWrapper(sWkpWidth:Int, vWkpWidth:Int)(implicit p: Parameters) extends XSModule{
  private val size = vectorParameters.vPRsDepth
  val io = IO(new Bundle{
    val enq = Flipped(Decoupled(new MicroOp))
    val selectInfo = Output(Vec(size, Valid(new VprsStatusArrayEntry)))
    val issueOH = Input(Valid(UInt(size.W)))
    val issueUop = Output(new MicroOp)
    val scalarWakeUps = Input(Vec(sWkpWidth, Valid(new WakeUpInfo)))
    val vectorWakeUps = Input(Vec(vWkpWidth, Valid(new WakeUpInfo)))
    val redirect = Input(Valid(new Redirect))
  })

  private val statusArray = Module(new VprsStatusArray(sWkpWidth, vWkpWidth))
  private val payloadArray = Module(new PayloadArray(new MicroOp, size, 1, "VprsPayloadArray"))

  statusArray.io.enq <> io.enq
  statusArray.io.issueOH := io.issueOH
  statusArray.io.scalarWakeUps := io.scalarWakeUps
  statusArray.io.vectorWakeUps := io.vectorWakeUps
  statusArray.io.redirect := io.redirect
  io.selectInfo := statusArray.io.selectInfo

  payloadArray.io.write.en := statusArray.io.enqToPayload.valid
  payloadArray.io.write.addr := statusArray.io.enqToPayload.bits
  payloadArray.io.write.data := io.enq.bits

  payloadArray.io.read.head.addr := io.issueOH.bits
  io.issueUop := payloadArray.io.read.head.data
}
