package xiangshan.vector.vbackend.vregfile
import chisel3._
import chisel3.util._
import xs.utils.SignExt
class MaskRead(size:Int, dataWidth:Int) extends Bundle {
  val addr = Input(UInt(log2Ceil(size).W))
  val data = Output(UInt(dataWidth.W))
}
class MaskWrite(size:Int, dataWidth:Int, maskWidth:Int) extends Bundle {
  val addr = Input(UInt(log2Ceil(size).W))
  val en = Input(Bool())
  val data = Input(UInt(dataWidth.W))
  val mask = Input(UInt(maskWidth.W))
}

class MaskRegfile(size:Int, dataWidth:Int, maskWidth:Int, readNum:Int, writeNum:Int) extends Module {
  val io = IO(new Bundle{
    val read = Vec(readNum, new MaskRead(size, dataWidth))
    val write = Vec(writeNum, new MaskWrite(size, dataWidth, maskWidth))
  })
  require(dataWidth % maskWidth == 0)
  class InternalWriteBundle extends Bundle {
    val addr = UInt(log2Ceil(size).W)
    val en = Bool()
    val data = UInt(dataWidth.W)
    val mask = UInt(dataWidth.W)
  }
  private val internalWrites = Wire(Vec(writeNum, new InternalWriteBundle))

  private val maskSegWidth = dataWidth / maskWidth
  for((iw, pw) <- internalWrites.zip(io.write)){
    iw.addr := pw.addr
    iw.en := pw.en
    iw.data := pw.data
    val resSeq = Seq.tabulate(maskWidth)(i => Fill(maskSegWidth, pw.mask(i)))
    iw.mask := Cat(resSeq.reverse)
  }

  private val array = Reg(Vec(size, UInt(dataWidth.W)))
  array.zipWithIndex.foreach({case(a, i) =>
    val wh = internalWrites.map(w => w.en && w.addr === i.U)
    val wd = internalWrites.zip(wh).map(w => Mux(w._2, w._1.data & w._1.mask, 0.U)).reduce(_ | _)
    val wm = internalWrites.zip(wh).map(w => Mux(w._2, w._1.mask, 0.U)).reduce(_ | _)
    val hit = Cat(internalWrites.map(w => w.en && w.addr === i.U)).orR
    when(hit){
      a := (a & (~wm).asUInt) | wd
    }
  })

  io.read.foreach(r =>
    r.data := array(r.addr)
  )

  private var readPortIdx = 0
  private var writePortIdx = 0

  def read(addr:UInt):UInt = {
    readPortIdx = readPortIdx + 1
    this.io.read(readPortIdx - 1).addr := addr
    this.io.read(readPortIdx - 1).data
  }

  def apply(addr:UInt):UInt = read(addr)

  def write(addr:UInt, data:UInt, mask:UInt, en:Bool):Unit = {
    this.io.write(writePortIdx).addr := addr
    this.io.write(writePortIdx).data := data
    this.io.write(writePortIdx).mask := mask
    this.io.write(writePortIdx).en := en
    writePortIdx = writePortIdx + 1
  }
}
