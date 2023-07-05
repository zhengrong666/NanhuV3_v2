package xiangshan.vector.vexecute.vissue.vrs

import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.issue._
import chisel3._
import chisel3.util._
import xiangshan.{MicroOp, Redirect, SrcState, SrcType}

class VRSBank(entryNum:Int, issueWidth:Int, wakeupWidth:Int, loadUnitNum:Int)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle {
    val redirect = Input(Valid(new Redirect))

    val selectInfo = Output(Vec(entryNum, Valid(new VRSSelectInfo)))
    val allocateInfo = Output(UInt(entryNum.W))

    val enq = Input(Valid(new Bundle {
      val addrOH = UInt(entryNum.W)
      val data = new MicroOp
    }))

    val issueAddr = Input(Vec(issueWidth, Valid(UInt(entryNum.W))))
    val issueUop = Output(Vec(issueWidth, Valid(new MicroOp)))
    val wakeup = Input(Vec(wakeupWidth, Valid(new WakeUpInfo)))
  })

  private val statusArray = Module(new VRSStatusArray(entryNum, issueWidth, wakeupWidth, loadUnitNum))
  private val payloadArray = Module(new PayloadArray(new MicroOp, entryNum, issueWidth, "VRSPayloadArray"))

  private def EnqToEntry(in: MicroOp): VRSStatusArrayEntry = {
    val enqEntry = Wire(new VRSStatusArrayEntry)
    enqEntry.psrc.take(2).zip(in.psrc.take(2)).foreach(elm => elm._1 := elm._2)
    enqEntry.srcType.take(2).zip(in.ctrl.srcType.take(2)).foreach(elm => elm._1 := elm._2)
    enqEntry.srcState.take(2).zip(in.srcState.take(2)).zip(in.ctrl.srcType.take(2)).foreach(elm =>
      elm._1._1 := Mux(SrcType.needWakeup(elm._2), elm._1._1, SrcState.rdy)
    )

    enqEntry.psrc(2) := in.old_pdest
    enqEntry.srcType(2) := in.ctrl.old_vdType
    enqEntry.srcState(2) := Mux(!(in.vCsrInfo.ta || in.vCsrInfo.ma), in.oldPdestState, SrcState.rdy)

    enqEntry.psrc(3) := in.vm
    enqEntry.srcType(3) := SrcType.vec
    enqEntry.srcState(3) := Mux(in.ctrl.vm, in.vmState, SrcState.rdy)

    enqEntry.fuType := in.ctrl.fuType
    enqEntry.robIdx := in.robIdx
    enqEntry.isOrdered := in.ctrl.isOrder
    enqEntry.uopIdx := in.uopIdx
    enqEntry
  }

  statusArray.io.redirect := io.redirect
  io.selectInfo := statusArray.io.selectInfo
  io.allocateInfo := statusArray.io.allocateInfo
  statusArray.io.enq.valid := io.enq.valid
  statusArray.io.enq.bits.addrOH := io.enq.bits.addrOH
  statusArray.io.enq.bits.data := EnqToEntry(io.enq.bits.data)
  statusArray.io.issue := io.issueAddr
  statusArray.io.wakeup := io.wakeup

  payloadArray.io.write.en := io.enq.valid
  payloadArray.io.write.addr := io.enq.bits.addrOH
  payloadArray.io.write.data := io.enq.bits.data
  payloadArray.io.read.zip(io.issueAddr).zip(io.issueUop).foreach({
    case((port, iAddr), iData) =>{
      port.addr := iAddr.bits
      iData.bits := port.data
      iData.valid := iAddr.valid
    }
  })
}
