package xiangshan.vector

import chisel3.util._
import chipsalliance.rocketchip.config._

case class VectorParameters (
    vLen: Int               = 128, //maybe 64、256、512...
    vRenameWidth: Int       = 4,
    vCommitWidth: Int       = 4,
    vPhyRegsNum: Int        = 64
) {
    def vPhyRegIdxWidth: Int = log2Up(vPhyRegsNum + 1)
}

case object VectorParametersKey extends Field[VectorParameters](VectorParameters())

trait HasVectorParameters {
    implicit val p: Parameters
    
    val vector = p(VectorParametersKey)

    val VLEN = vector.vLen
    val VIRenameWidth = vector.vRenameWidth
    val VICommitWidth = vector.vCommitWidth
    val VIPhyRegsNum = vector.vPhyRegsNum
    val VIPhyRegIdxWidth = vector.vPhyRegIdxWidth

    //unit debug only
    val RobSize = 64
}

// case class VectorRenameParameters(
//     robSize: Int    = 256,
//     vRenameWidth: Int = 4

// ) {

// }
