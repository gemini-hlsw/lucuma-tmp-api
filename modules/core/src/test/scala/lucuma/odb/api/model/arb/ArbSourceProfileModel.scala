// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.`enum`.{CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.arb.ArbRefined
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.targetModel.SourceProfileModel._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbSourceProfileModel {

  import ArbAngleModel._
  import ArbEnumerated._
  import ArbRefined._
  import ArbWavelengthModel._

  implicit val arbFluxDensityInput: Arbitrary[FluxDensityInput] =
    Arbitrary {
      for {
        w <- arbitrary[WavelengthModel.Input]
        d <- arbitrary[PosBigDecimal]
      } yield FluxDensityInput(w, d)
    }

  implicit val cogFluxDensityInput: Cogen[FluxDensityInput] =
    Cogen[(
      WavelengthModel.Input,
      PosBigDecimal
    )].contramap { in => (
      in.wavelength,
      in.density
    )}


  implicit val arbUnnormalizedSedInput: Arbitrary[UnnormalizedSedInput] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[StellarLibrarySpectrum].map(UnnormalizedSedInput.stellarLibrary),
        arbitrary[CoolStarTemperature].map(UnnormalizedSedInput.coolStar),
        arbitrary[GalaxySpectrum].map(UnnormalizedSedInput.galaxy),
        arbitrary[PlanetSpectrum].map(UnnormalizedSedInput.planet),
        arbitrary[HIIRegionSpectrum].map(UnnormalizedSedInput.hiiRegionSpectrum),
        arbitrary[PlanetaryNebulaSpectrum].map(UnnormalizedSedInput.planetaryNebula),
        arbitrary[BigDecimal].map(UnnormalizedSedInput.powerLaw),
        arbitrary[PosBigDecimal].map(UnnormalizedSedInput.blackBody),
        arbitrary[List[FluxDensityInput]].map(UnnormalizedSedInput.userDefined)
      )
    }

  implicit val cogUnnormalizedSedInput: Cogen[UnnormalizedSedInput] =
    Cogen[(
      Option[StellarLibrarySpectrum],
      Option[CoolStarTemperature],
      Option[GalaxySpectrum],
      Option[PlanetSpectrum],
      Option[HIIRegionSpectrum],
      Option[PlanetaryNebulaSpectrum],
      Option[BigDecimal],
      Option[PosBigDecimal],
      Option[List[FluxDensityInput]]
    )].contramap { in => (
      in.stellarLibrary,
      in.coolStar,
      in.galaxy,
      in.planet,
      in.hiiRegion,
      in.planetaryNebula,
      in.powerLaw,
      in.blackBodyTempK,
      in.fluxDensities
    )}

  
}

object ArbSourceProfileModel extends ArbSourceProfileModel
