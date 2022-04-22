// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.{GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu}
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Arbitrary.arbitrary


trait ArbScienceConfigurationModel {

  import ScienceConfigurationModel.Modes.GmosNorthLongSlit

  import ArbAngle._
  import ArbEnumerated._

  implicit val arbGmosNorthLongSlit: Arbitrary[GmosNorthLongSlit] =
    Arbitrary {
      for {
        f <- arbitrary[Option[GmosNorthFilter]]
        d <- arbitrary[GmosNorthDisperser]
        u <- arbitrary[GmosNorthFpu]
        a <- arbitrary[Angle]
      } yield GmosNorthLongSlit(f, d, u, a)
    }

  implicit val cogGmosNorthLongSlit: Cogen[GmosNorthLongSlit] =
    Cogen[(
      Option[GmosNorthFilter],
      GmosNorthDisperser,
      GmosNorthFpu,
      Angle
    )].contramap { in => (
      in.filter,
      in.grating,
      in.fpu,
      in.slitWidth
    )}

}

object ArbScienceConfigurationModel extends ArbScienceConfigurationModel
