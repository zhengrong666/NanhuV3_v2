package xiangshan.vector.vbackend.vexecute.vfu.fp

import chipsalliance.rocketchip.config._
// import darecreek.{DarecreekParameters, LaneWidth, xLen}
import xiangshan.vector.vbackend.vexecute.vfu._
import xiangshan.vector.vbackend.vexecute.vfu.DarecreekParamVFU._

case object NAME extends Field[String]
case object LANE_WIDTH extends Field[Int]
case object XLEN extends Field[Int]


class WithVFPUConfig extends Config((site, here, up) => {
  case NAME => "VFPU"
  case LANE_WIDTH => LaneWidth
  case XLEN => xLen
}) with DarecreekParametersVFU

trait HasVFPUParams {
  implicit val p: Parameters
//  val name = p(NAME)
  val laneWidth = p(LANE_WIDTH)
  val xLen = p(XLEN)
}

