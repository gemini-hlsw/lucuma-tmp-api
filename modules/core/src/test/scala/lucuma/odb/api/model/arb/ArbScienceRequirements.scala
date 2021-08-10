// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.util.arb.ArbEnumerated

import eu.timepit.refined._
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosDouble
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Arbitrary.arbitrary
import lucuma.core.enum.FocalPlane
import lucuma.core.enum.ScienceMode
import lucuma.core.enum.SpectroscopyCapabilities
import lucuma.core.math.Wavelength
import lucuma.core.math.arb._
import lucuma.core.math.Angle

trait ArbScienceRequirements {

  import ArbEnumerated._
  import ArbAngle._
  import ArbWavelength._

  implicit val arbSpectroscopyScienceRequirements: Arbitrary[SpectroscopyScienceRequirements] =
    Arbitrary {
      for {
        w  <- arbitrary[Option[Wavelength]]
        r  <- arbitrary[Option[PosInt]]
        s  <- arbitrary[Option[PosDouble]].map(_.flatMap(r => refineV[Positive](BigDecimal(r.value)).toOption))
        a  <- arbitrary[Option[Wavelength]]
        wr <- arbitrary[Option[Wavelength]]
        f  <- arbitrary[Option[FocalPlane]]
        fa <- arbitrary[Option[Angle]]
        c  <- arbitrary[Option[SpectroscopyCapabilities]]
      } yield SpectroscopyScienceRequirements(w, r, s, a, wr, f, fa, c)
    }

  implicit val cogSpectroscopyScienceRequirements: Cogen[SpectroscopyScienceRequirements] =
    Cogen[(
        Option[Wavelength],
        Option[PosInt],
        Option[BigDecimal],
        Option[Wavelength],
        Option[Wavelength],
        Option[FocalPlane],
        Option[Angle],
        Option[SpectroscopyCapabilities]
    )].contramap { sc => (
      sc.wavelength,
      sc.resolution,
      sc.signalToNoise.map(_.value),
      sc.signalToNoiseAt,
      sc.wavelengthRange,
      sc.focalPlane,
      sc.focalPlaneAngle,
      sc.capabilities,
    )}

  implicit val arbScienceRequirements: Arbitrary[ScienceRequirements] =
    Arbitrary {
      for {
        p <- arbitrary[ScienceMode]
        o <- arbitrary[SpectroscopyScienceRequirements]
      } yield ScienceRequirements(p, o)
    }

  implicit val cogScienceRequirements: Cogen[ScienceRequirements] =
    Cogen[(
      ScienceMode,
      SpectroscopyScienceRequirements
    )].contramap { sc => (
      sc.mode,
      sc.spectroscopyRequirements
    )}
}

object ArbScienceRequirements extends ArbScienceRequirements
