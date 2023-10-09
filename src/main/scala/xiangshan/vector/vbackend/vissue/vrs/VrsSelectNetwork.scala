package xiangshan.vector.vbackend.vissue.vrs

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.issue.SelectPolicy
import xiangshan.backend.rob.RobPtr
import xiangshan.{ExuOutput, FuType, Redirect, XSBundle, XSModule}

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

object VecSelectPolicy {
  def apply(in:Seq[Valid[VrsSelectResp]], bankNum:Int, entryNum:Int, p:Parameters) :Valid[VrsSelectResp] = {
    val selector = Module(new SelectPolicy(in.length, false, true)(p))
    selector.io.in.zip(in).foreach({case(a, b) =>
      a.valid := b.valid
      a.bits := b.bits.info.robPtr
    })
    val res = Wire(Valid(new VrsSelectResp(bankNum, entryNum)(p)))
    res.valid := selector.io.out.valid
    res.bits := Mux1H(selector.io.out.bits, in.map(_.bits))
    res
  }
}

class VrsSelectNetwork(bankNum:Int, entryNum:Int, issueNum:Int, isOrdered:Boolean, needToken:Boolean = false, tokenNum:Int = 0, val fuTypeList:Seq[UInt], name:Option[String] = None)(implicit p: Parameters) extends XSModule {
  require(issueNum <= bankNum && 0 < issueNum && bankNum % issueNum == 0, "Illegal number of issue ports are supported now!")
  val io = IO(new Bundle{
    val redirect = Input(Valid(new Redirect))
    val selectInfo = Input(Vec(bankNum,Vec(entryNum, Valid(new VrsSelectInfo))))
    val issueInfo = Vec(issueNum, Decoupled(new VrsSelectResp(bankNum, entryNum)))
    val tokenRelease = if(needToken) Some(Input(Vec(issueNum, Valid(new ExuOutput)))) else None
    val orderedCtrl = if(isOrdered) Some(Input(Valid(new OIQEntry))) else None
  })
  override val desiredName:String = name.getOrElse("VrsSelectNetwork")

  private val selectInputPerBank = io.selectInfo.zipWithIndex.map({ case (si, bidx) =>
    si.zipWithIndex.map({ case (in, eidx) =>
      val selInfo = Wire(Valid(new VrsSelectResp(bankNum, entryNum)))
      val orderCond = if(isOrdered) {
        in.bits.isOrdered && io.orderedCtrl.get.valid &&
          io.orderedCtrl.get.bits.uopIdx === in.bits.uopIdx &&
          io.orderedCtrl.get.bits.robIdx === in.bits.robPtr
      } else {
        !in.bits.isOrdered
      }
      val outPort = io.issueInfo(bidx * issueNum / bankNum)
      val addrHit = outPort.valid && outPort.bits.bankIdxOH(bidx) && outPort.bits.entryIdxOH(eidx)
      selInfo.valid := in.valid && fuTypeList.map(_ === in.bits.fuType).reduce(_ | _) && orderCond && !addrHit && !io.redirect.valid
      selInfo.bits.info := in.bits
      selInfo.bits.bankIdxOH := (1 << bidx).U(bankNum.W)
      selInfo.bits.entryIdxOH := (1 << eidx).U(entryNum.W)
      selInfo
    })
  })
  private val finalSelectResult = Wire(Vec(issueNum, Valid(new VrsSelectResp(bankNum, entryNum))))
  private val bankNumPerIss = bankNum / issueNum
  finalSelectResult.zipWithIndex.foreach({ case (res, i) =>
    val selBanks = selectInputPerBank.slice(i * bankNumPerIss, i * bankNumPerIss + bankNumPerIss).reduce(_ ++ _)
    val selRes = VecSelectPolicy(selBanks, bankNum, entryNum, p)
    val validReg = RegNext(selRes.valid)
    val bitsReg = RegEnable(selRes.bits, selRes.valid)
    val shouldBeFlushed = res.bits.info.robPtr.needFlush(io.redirect)
    res.valid := validReg && !shouldBeFlushed
    res.bits := bitsReg
  })

  if (needToken) {
    val tokenAllocators = Seq.fill(issueNum)(Module(new VectorTokenAllocator(tokenNum)))
    for ((((outPort, driver), ta), tr) <- io.issueInfo.zip(finalSelectResult).zip(tokenAllocators).zip(io.tokenRelease.get)) {
      ta.io.redirect := io.redirect
      ta.io.alloc.valid := outPort.fire
      ta.io.alloc.bits.uopIdx := driver.bits.info.uopIdx
      ta.io.alloc.bits.robPtr := driver.bits.info.robPtr
      ta.io.release := tr
      val shouldBeFlushed = driver.bits.info.robPtr.needFlush(io.redirect)
      outPort.valid := driver.valid && ta.io.allow && !shouldBeFlushed
      outPort.bits.bankIdxOH := driver.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.bits.entryIdxOH
      outPort.bits.info := driver.bits.info
    }
  } else {
    for ((outPort, driver) <- io.issueInfo.zip(finalSelectResult)) {
      val shouldBeFlushed = driver.bits.info.robPtr.needFlush(io.redirect)
      outPort.valid := driver.valid && !shouldBeFlushed
      outPort.bits.bankIdxOH := driver.bits.bankIdxOH
      outPort.bits.entryIdxOH := driver.bits.entryIdxOH
      outPort.bits.info := driver.bits.info
    }
  }
}
