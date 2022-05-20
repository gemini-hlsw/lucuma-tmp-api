// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.all.NonEmptyString
import lucuma.core.`enum`.{GmosAmpGain, GmosAmpReadMode, GmosNorthFilter, GmosNorthFpu, GmosNorthGrating, GmosRoi, GmosSouthFilter, GmosSouthFpu, GmosSouthGrating, GmosXBinning, GmosYBinning}
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import lucuma.odb.api.model.gmos.longslit.{AdvancedConfig, BasicConfig}
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Arbitrary.arbitrary


trait ArbScienceMode {

  import ScienceMode.{GmosNorthLongSlit, GmosSouthLongSlit}

  import ArbEnumerated._
  import ArbWavelength._

  implicit def arbBasicConfig[G: Arbitrary, F: Arbitrary, U: Arbitrary]: Arbitrary[BasicConfig[G, F, U]] =
    Arbitrary {
      for {
        g <- arbitrary[G]
        f <- arbitrary[Option[F]]
        u <- arbitrary[U]
      } yield BasicConfig(g, f, u)
    }

  implicit def cogBasicConfig[G: Cogen, F: Cogen, U: Cogen]: Cogen[BasicConfig[G, F, U]] =
    Cogen[(
      G,
      Option[F],
      U
    )].contramap { a => (
      a.grating,
      a.filter,
      a.fpu
    )}

  implicit def arbAdvancedConfig[G: Arbitrary, F: Arbitrary, U: Arbitrary]: Arbitrary[AdvancedConfig[G, F, U]] =
    Arbitrary {
      for {
        n  <- arbitrary[Option[NonEmptyString]]
        w  <- arbitrary[Option[Wavelength]]
        g  <- arbitrary[Option[G]]
        f  <- arbitrary[Option[Option[F]]]
        u  <- arbitrary[Option[U]]
        x  <- arbitrary[Option[GmosXBinning]]
        y  <- arbitrary[Option[GmosYBinning]]
        ar <- arbitrary[Option[GmosAmpReadMode]]
        ag <- arbitrary[Option[GmosAmpGain]]
        ro <- arbitrary[Option[GmosRoi]]
      } yield AdvancedConfig(n, w, g, f, u, x, y, ar, ag, ro)
    }

  implicit def cogAdvancedConfig[G: Cogen, F: Cogen, U: Cogen]: Cogen[AdvancedConfig[G, F, U]] =
    Cogen[(
      Option[String],
      Option[Wavelength],
      Option[G],
      Option[Option[F]],
      Option[U],
      Option[GmosXBinning],
      Option[GmosYBinning],
      Option[GmosAmpReadMode],
      Option[GmosAmpGain],
      Option[GmosRoi]
    )].contramap { a => (
      a.name.map(_.value),
      a.overrideWavelength,
      a.overrideGrating,
      a.overrideFilter,
      a.overrideFpu,
      a.explicitXBin,
      a.explicitYBin,
      a.explicitAmpReadMode,
      a.explicitAmpGain,
      a.explicitRoi
    )}

  implicit val arbGmosNorthLongSlit: Arbitrary[GmosNorthLongSlit] =
    Arbitrary {
      for {
        b <- arbitrary[BasicConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]]
        a <- arbitrary[Option[AdvancedConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]]]
      } yield GmosNorthLongSlit(b, a)
    }

  implicit val cogGmosNorthLongSlit: Cogen[GmosNorthLongSlit] =
    Cogen[(
      BasicConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu],
      Option[AdvancedConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]]
    )].contramap { a => (
      a.basic,
      a.advanced
    )}

  implicit val arbGmosSouthLongSlit: Arbitrary[GmosSouthLongSlit] =
    Arbitrary {
      for {
        b <- arbitrary[BasicConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]]
        a <- arbitrary[Option[AdvancedConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]]]
      } yield GmosSouthLongSlit(b, a)
    }

  implicit val cogGmosSouthLongSlit: Cogen[GmosSouthLongSlit] =
    Cogen[(
      BasicConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu],
      Option[AdvancedConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]]
    )].contramap { a => (
      a.basic,
      a.advanced
    )}
}

object ArbScienceMode extends ArbScienceMode
