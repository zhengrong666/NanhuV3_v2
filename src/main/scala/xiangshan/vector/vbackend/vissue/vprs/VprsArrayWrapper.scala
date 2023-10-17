package xiangshan.vector.vbackend.vissue.vprs
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.issue.{PayloadArray, WakeUpInfo}
import xiangshan.{MicroOp, Redirect, XSModule}

class VprsPdestArray(implicit p: Parameters) extends XSModule{
  private val size = vectorParameters.vPRsDepth
  val io = IO(new Bundle{
    val enq = Input(Valid(new Bundle{
      val pdest = UInt(PhyRegIdxWidth.W)
      val uopIdx = UInt(3.W)
      val addrOH = UInt(size.W)
    }))
    val read = new Bundle{
      val addrOH = Input(UInt(size.W))
      val pdest = Vec(8, UInt(PhyRegIdxWidth.W))
    }
  })
  private val array = Reg(Vec(size, Vec(8, UInt(PhyRegIdxWidth.W))))

  array.zipWithIndex.foreach({case(e, i) =>
    when(io.enq.valid && io.enq.bits.addrOH(i)){
      e(io.enq.bits.uopIdx) := io.enq.bits.pdest
    }
  })
  io.read.pdest := Mux1H(io.read.addrOH, array)
}

class VprsArrayWrapper(sWkpWidth:Int, vWkpWidth:Int)(implicit p: Parameters) extends XSModule{
  private val size = vectorParameters.vPRsDepth
  val io = IO(new Bundle{
    val enq = Flipped(Decoupled(new MicroOp))
    val selectInfo = Output(Vec(size, Valid(new VprsStatusArrayEntry)))
    val issueOH = Input(Valid(UInt(size.W)))
    val issueUop = Output(new MicroOp)
    val issuePdest = Output(Vec(8, UInt(PhyRegIdxWidth.W)))
    val scalarWakeUps = Input(Vec(sWkpWidth, Valid(new WakeUpInfo)))
    val vectorWakeUps = Input(Vec(vWkpWidth, Valid(new WakeUpInfo)))
    val redirect = Input(Valid(new Redirect))
  })

  private val statusArray = Module(new VprsStatusArray(sWkpWidth, vWkpWidth))
  private val payloadArray = Module(new PayloadArray(new MicroOp, size, 1, "VprsPayloadArray"))
  private val pdestArray = Module(new VprsPdestArray)

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

  pdestArray.io.enq.valid := statusArray.io.pdestUpdate.valid
  pdestArray.io.enq.bits.addrOH := statusArray.io.pdestUpdate.bits
  pdestArray.io.enq.bits.uopIdx := io.enq.bits.uopIdx
  pdestArray.io.enq.bits.pdest := io.enq.bits.pdest
  pdestArray.io.read.addrOH := io.issueOH.bits
  io.issuePdest := pdestArray.io.read.pdest
}
