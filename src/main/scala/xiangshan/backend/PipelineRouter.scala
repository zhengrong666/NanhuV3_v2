package xiangshan.backend
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.XSModule

class PipelineRouter[T <: Data](gen:T, vecLen:Int, outNum:Int) extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Vec(vecLen, Decoupled(gen)))
    val out = Vec(outNum, Vec(vecLen, Decoupled(gen)))
    val flush = Input(Bool())
  })

  private val validRegs = Seq.tabulate(outNum, vecLen)((_,_) => RegInit(false.B))
  private val bitsRegs = Seq.tabulate(outNum, vecLen)((_,_) => Reg(gen))
  private val allowOuts = Seq.tabulate(outNum, vecLen)((_,_) => Wire(Bool()))
  private val allowIns = Wire(Vec(vecLen,Bool()))
  private val auxValids = RegInit(VecInit(Seq.tabulate(vecLen)(_ => false.B)))

    for((sl, oidx) <- allowOuts.zipWithIndex) {
    val otherReadies = io.out.zipWithIndex.filterNot(_._2 == oidx).map(_._1).map(_.map(_.ready)).transpose
    for((s, rl) <- sl.zip(otherReadies)){
      s := rl.reduce(_&_)
    }
  }


  for((vs, bs) <- validRegs.zip(bitsRegs)){
    for((((i, v), b), en) <- io.in.zip(vs).zip(bs).zip(allowIns)){
      when(en){
        v := i.valid
      }
      when(i.fire){
        b := i.bits
      }
      i.ready := en
    }
  }

  for(((v, en), in) <- auxValids.zip(allowIns).zip(io.in)){
    when(en){
      v := in.valid
    }
  }

  for ((((ol, vl), bl), el) <- io.out.zip(validRegs).zip(bitsRegs).zip(allowOuts)) {
    for ((((o, v), b), e) <- ol.zip(vl).zip(bl).zip(el)) {
      o.valid := v && !io.flush && e
      o.bits := b
    }
  }

  for((r, iidx) <- allowIns.zipWithIndex){
    val fires = io.out.map(_(iidx)).map(_.fire)
    r := fires.reduce(_&_) || auxValids(iidx) === false.B
  }

  validRegs.foreach(vl => {
    vl.zip(auxValids).foreach({case(a, b) => assert(a === b)})
  })
}
