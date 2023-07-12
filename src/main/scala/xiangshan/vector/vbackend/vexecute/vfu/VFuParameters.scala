package xiangshan.vector.vbackend.vexecute.vfu

import chisel3._
import chisel3.util._

object VFUParam {
  val XLEN = 64
  val VLEN = 128
  val VLENB = VLEN/8
  val bVL = log2Up(VLEN) + 1
  val bVSTART = bVL - 1
}