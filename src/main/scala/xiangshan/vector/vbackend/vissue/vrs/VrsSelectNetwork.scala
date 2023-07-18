package xiangshan.vector.vbackend.vissue.vrs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.rob.RobPtr
import xiangshan.{ExuOutput, FuType, Redirect, XSBundle, XSModule}
import xs.utils.{LogicShiftRight, ParallelOperation}

class VrsSelectInfo(implicit p: Parameters) extends XSBundle{
  val fuType = FuType()
  val robPtr = new RobPtr
  val uopIdx: UInt = UInt(3.W)
  val isOrdered :Bool = Bool()
}

class VrsSelectResp(val bankIdxWidth:Int, entryIdxWidth:Int)(implicit p: Parameters) extends XSBundle {
  val info = new VrsSelectInfo
  val entryIdxOH = UInt(entryIdxWidth.W)
  val bankIdxOH = UInt(bankIdxWidth.W)
}

class VrsSelectMux(bankIdxWidth:Int, entryIdxWidth:Int, isOrdered:Boolean)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in0 = Input(Valid(new VrsSelectResp(bankIdxWidth, entryIdxWidth)))
    val in1 = Input(Valid(new VrsSelectResp(bankIdxWidth, entryIdxWidth)))
    val out = Output(Valid(new VrsSelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val valid0 = io.in0.valid
  private val valid1 = io.in1.valid
  private val ptr0 = io.in0.bits.info.robPtr
  private val ptr1 = io.in1.bits.info.robPtr
  private val idx0 = io.in0.bits.info.uopIdx
  private val idx1 = io.in1.bits.info.uopIdx
  private val validVec = Cat(valid1, valid0)
  private val sel = WireInit(true.B)
  if(isOrdered) {
    when(validVec === "b01".U) {
      sel := true.B
    }.elsewhen(validVec === "b10".U) {
      sel := false.B
    }.elsewhen(validVec === "b11".U) {
      when(ptr0 < ptr1) {
        sel := true.B
      }.elsewhen(ptr0 > ptr1) {
        sel := false.B
      }.otherwise {
        sel := Mux(idx0 < idx1, true.B, false.B)
      }
    }
  } else {
    when(validVec === "b01".U) {
      sel := true.B
    }.elsewhen(validVec === "b10".U) {
      sel := false.B
    }.elsewhen(validVec === "b11".U) {
      when(ptr0 <= ptr1){
        sel := true.B
      }.otherwise{
        sel := false.B
      }
    }
  }

  private val res = Mux(sel, io.in0, io.in1)
  io.out := res
}

object VrsSelectMux{
  def apply(in0: Valid[VrsSelectResp], in1: Valid[VrsSelectResp], bankIdxWidth:Int, entryIdxWidth:Int, isOrdered:Boolean, p: Parameters):Valid[VrsSelectResp] = {
    val smux = Module(new VrsSelectMux(bankIdxWidth, entryIdxWidth, isOrdered)(p))
    smux.io.in0 := in0
    smux.io.in1 := in1
    smux.io.out
  }
}

class VrsSelector(bankNum:Int, entryNum:Int, inputWidth:Int, isOrdered:Boolean)(implicit p: Parameters) extends Module{
  private val bankIdxWidth = bankNum
  private val entryIdxWidth = entryNum
  val io = IO(new Bundle{
    val in = Input(Vec(inputWidth, Valid(new VrsSelectResp(bankIdxWidth, entryIdxWidth))))
    val out = Output(Valid(new VrsSelectResp(bankIdxWidth, entryIdxWidth)))
  })
  private val operationFunction = VrsSelectMux(_, _, bankIdxWidth, entryIdxWidth, isOrdered, p)
  private val res  = ParallelOperation(io.in, operationFunction)
  io.out := res
}

class VrsSelectNetwork(bankNum:Int, entryNum:Int, issueNum:Int, isOrdered:Boolean, needToken:Boolean = false, tokenNum:Int = 0, val fuTypeList:Seq[UInt], name:Option[String] = None)(implicit p: Parameters) extends XSModule {
  require(issueNum <= bankNum && 0 < issueNum && bankNum % issueNum == 0, "Illegal number of issue ports are supported now!")
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val selectInfo = Input(Vec(bankNum,Vec(entryNum, Valid(new VrsSelectInfo))))
    val issueInfo = Vec(issueNum, Decoupled(new VrsSelectResp(bankNum, entryNum)))
    val tokenRelease = if(needToken) Some(Input(Vec(issueNum, Valid(new ExuOutput)))) else None
  })
  override val desiredName:String = name.getOrElse("VrsSelectNetwork")

  private val issueValidBitVecList = if(isOrdered){
    io.selectInfo.map(_.map(info => info.valid && info.bits.isOrdered && Cat(fuTypeList.map(_ === info.bits.fuType)).orR))
  } else {
    io.selectInfo.map(_.map(info => info.valid && !info.bits.isOrdered && Cat(fuTypeList.map(_ === info.bits.fuType)).orR))
  }
  private val issueDataVecList = io.selectInfo.map(_.map(_.bits))
  private val issueBankIdxVecList = io.selectInfo.indices.map(idx => Seq.fill(entryNum)((1<<idx).U(bankNum.W)))
  private val issueEntryIdxVecList = io.selectInfo.indices.map(_ => Seq.tabulate(entryNum)(idx0 => (1<<idx0).U(entryNum.W)))
  private val issueAllDataList = issueValidBitVecList.zip(issueDataVecList).zip(issueBankIdxVecList).zip(issueEntryIdxVecList).map({
    case(((v, d),bi),ei) => v.zip(d).zip(bi).zip(ei)
  })

  private val bankNumPerSelector = bankNum / issueNum
  private val selectorSeq = Seq.fill(issueNum)(Module(new VrsSelector(bankNum, entryNum, bankNumPerSelector * entryNum, isOrdered)))

  private val selectorInput = Seq.tabulate(issueNum)({idx =>
    issueAllDataList.slice(idx*bankNumPerSelector, idx*bankNumPerSelector + bankNumPerSelector).reduce(_++_)
  })

  for((s, si) <- selectorSeq zip selectorInput){
    s.io.in.zip(si).foreach({case(inPort, driver) =>
      inPort.valid := driver._1._1._1
      inPort.bits.info := driver._1._1._2
      inPort.bits.bankIdxOH := driver._1._2
      inPort.bits.entryIdxOH := driver._2
    })
  }

  for ((outPort, driver) <- io.issueInfo.zip(selectorSeq)) {
    val shouldBeFlushed = driver.io.out.bits.info.robPtr.needFlush(io.redirect)
    outPort.valid := driver.io.out.valid && !shouldBeFlushed
    outPort.bits.bankIdxOH := driver.io.out.bits.bankIdxOH
    outPort.bits.entryIdxOH := driver.io.out.bits.entryIdxOH
    outPort.bits.info := driver.io.out.bits.info
  }

  if (needToken) {
    val tokenAllocators = Seq.fill(issueNum)(Module(new VectorTokenAllocator(tokenNum)))
    for ((((outPort, driver), ta), tr) <- io.issueInfo.zip(selectorSeq).zip(tokenAllocators).zip(io.tokenRelease.get)) {
      ta.io.redirect := io.redirect
      ta.io.alloc.valid := outPort.fire
      ta.io.alloc.bits.uopIdx := driver.io.out.bits.info.uopIdx
      ta.io.alloc.bits.robPtr := driver.io.out.bits.info.robPtr
      ta.io.release := tr
      val shouldBeFlushed = driver.io.out.bits.info.robPtr.needFlush(io.redirect)
      outPort.valid := driver.io.out.valid && ta.io.allow && !shouldBeFlushed
      outPort.bits.bankIdxOH := driver.io.out.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.io.out.bits.entryIdxOH
      outPort.bits.info := driver.io.out.bits.info
    }
  } else {
    for ((outPort, driver) <- io.issueInfo.zip(selectorSeq)) {
      val shouldBeFlushed = driver.io.out.bits.info.robPtr.needFlush(io.redirect)
      outPort.valid := driver.io.out.valid && !shouldBeFlushed
      outPort.bits.bankIdxOH := driver.io.out.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.io.out.bits.entryIdxOH
      outPort.bits.info := driver.io.out.bits.info
    }
  }

}
