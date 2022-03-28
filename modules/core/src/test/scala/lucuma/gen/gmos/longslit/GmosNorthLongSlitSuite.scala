// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eu.timepit.refined.auto._
import lucuma.core.`enum`.ImageQuality
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.ScienceConfigurationModel.Modes
import lucuma.odb.api.model.arb._
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import scala.concurrent.duration._


final class GmosNorthLongSlitSuite extends ScalaCheckSuite {

  import ArbEnumerated._
  import ArbScienceConfigurationModel._
  import ArbSourceProfile._

  property("all atoms and steps have unique ids") {
    forAll { (mode: Modes.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val λ        = Wavelength.unsafeFromInt(500000)
      val longSlit = GmosNorthLongSlit(
        mode,
        AcqExposureTime.unsafeFrom(10.seconds),
        SciExposureTime.unsafeFrom( 5.minutes),
        λ,
        sp,
        iq,
        2.5
      )
      val seq = longSlit.generate.science[IO](IO.pure(false))
      val ids = seq.take(100).compile.toList.unsafeRunSync().flatMap(a => a.id.toUuid :: a.steps.toList.map(_.id.toUuid))

      assertEquals(ids.size, ids.distinct.size)

    }

  }


}
