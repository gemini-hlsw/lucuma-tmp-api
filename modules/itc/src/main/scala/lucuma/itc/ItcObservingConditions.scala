// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import lucuma.core.enum._
import io.circe.Encoder

final case class ItcObservingConditions(
  iq:      ImageQuality,
  cc:      CloudExtinction,
  wv:      WaterVapor,
  sb:      SkyBackground,
  airmass: Double
)

object ItcObservingConditions {

  implicit val encoder: Encoder[ItcObservingConditions] =
    Encoder.forProduct5("iq", "cc", "wv", "sb", "airmass") { a =>
      (a.iq.label, a.cc.label, a.wv.label, a.sb.label, a.airmass)
    }

}
