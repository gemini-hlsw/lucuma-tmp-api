// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.{GmosNorthGrating, GmosNorthFilter, GmosNorthFpu}
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Arbitrary.arbitrary


trait ArbScienceConfigurationModel {

  import ScienceConfigurationModel.Modes.GmosNorthLongSlit

  import ArbEnumerated._

  implicit val arbGmosNorthLongSlit: Arbitrary[GmosNorthLongSlit] =
    Arbitrary {
      for {
        f <- arbitrary[Option[GmosNorthFilter]]
        d <- arbitrary[GmosNorthGrating]
        u <- arbitrary[GmosNorthFpu]
      } yield GmosNorthLongSlit(f, d, u)
    }

  implicit val cogGmosNorthLongSlit: Cogen[GmosNorthLongSlit] =
    Cogen[(
      Option[GmosNorthFilter],
      GmosNorthGrating,
      GmosNorthFpu
    )].contramap { in => (
      in.filter,
      in.grating,
      in.fpu
    )}

}

object ArbScienceConfigurationModel extends ArbScienceConfigurationModel
