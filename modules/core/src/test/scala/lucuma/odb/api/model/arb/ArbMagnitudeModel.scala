// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.MagnitudeSystem
import lucuma.core.model.Magnitude
import lucuma.core.model.arb.ArbMagnitude
import lucuma.core.math.MagnitudeValue
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbMagnitudeModel {

  import ArbMagnitude._

  implicit val arbMagnitudeModelInput: Arbitrary[MagnitudeModel.Input] =
    Arbitrary {
      arbitrary[Magnitude].map { m =>
        MagnitudeModel.Input(m.value.toDoubleValue, m.band, Some(m.system), m.error.map(_.toDoubleValue))
      }
    }

  implicit val cogMagnitudeModelInput: Cogen[MagnitudeModel.Input] =
    Cogen[Magnitude].contramap { in =>
      Magnitude(
        MagnitudeValue.fromBigDecimal.unsafeGet(in.value),
        in.band,
        in.error.flatMap(MagnitudeValue.fromBigDecimal.getOption),
        in.system.getOrElse(MagnitudeSystem.Vega)
      )
    }

}

object ArbMagnitudeModel extends ArbMagnitudeModel
