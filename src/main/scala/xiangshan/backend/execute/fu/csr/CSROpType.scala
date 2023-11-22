package xiangshan.backend.execute.fu.csr
import chisel3._
object CSROpType {
  def jmp = "b0000".U

  def wrt = "b0001".U

  def set = "b0010".U

  def clr = "b0011".U

  def wfi = "b0100".U

  def wrti = "b0101".U

  def seti = "b0110".U

  def clri = "b0111".U

  def vsetivli = "b1011".U

  def vsetvli = "b1001".U

  def vsetvl = "b1010".U

  def needAccess(op: UInt): Bool = op(1, 0) =/= 0.U
}
