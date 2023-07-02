package xiangshan.vector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

import utils._
import xiangshan._

import xiangshan.vector.virename._

abstract class VectorBaseModule(implicit val p: Parameters) extends Module with HasVectorParameters
abstract class VectorBaseBundle(implicit val p: Parameters) extends Bundle with HasVectorParameters
