// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.`enum`.{Band, CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.BrightnessUnits.{Brightness, FluxDensityContinuum, Integrated, LineFlux, Surface}
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional.{Of, Units}
import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.targetModel.SourceProfileModel._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbSourceProfileModel {

  import ArbAngleModel._
  import ArbEnumerated._
  import ArbInput._
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

  implicit def arbCreateMeasureInput[V: Arbitrary, U](
    implicit ev: Enumerated[Units Of U]
  ): Arbitrary[CreateMeasureInput[V, U]] =
    Arbitrary {
      for {
        v <- arbitrary[V]
        u <- arbitrary[Units Of U]
      } yield CreateMeasureInput(v, u)
    }

  implicit def cogCreateMeasureInput[V: Cogen, U](
    implicit ev: Enumerated[Units Of U]
  ): Cogen[CreateMeasureInput[V, U]] =
    Cogen[(
      V,
      Units Of U
    )].contramap { in => (
      in.value,
      in.units
    )}

  implicit def arbCreateBandBrightnessInput[T](
    implicit ev: Enumerated[Units Of Brightness[T]]
  ): Arbitrary[CreateBandBrightnessInput[T]] =
    Arbitrary {
      for {
        br <- arbitrary[CreateMeasureInput[BigDecimal, Brightness[T]]]
        bd <- arbitrary[Band]
        e  <- arbitrary[Option[BigDecimal]]
      } yield CreateBandBrightnessInput(bd, br.value, br.units, e)
    }

  implicit def cogCreateBandBrightnessInput[T](
    implicit ev: Enumerated[Units Of Brightness[T]]
  ): Cogen[CreateBandBrightnessInput[T]] =
    Cogen[(
      Band,
      BigDecimal,
      Units Of Brightness[T],
      Option[BigDecimal]
    )].contramap { in => (
      in.band,
      in.value,
      in.units,
      in.error
    )}

  implicit def arbBandNormalizedInput[T](
    implicit ev: Enumerated[Units Of Brightness[T]]
  ): Arbitrary[BandNormalizedInput[T]] =
    Arbitrary {
      for {
        s <- arbitrary[UnnormalizedSedInput]
        b <- arbitrary[List[CreateBandBrightnessInput[T]]]
      } yield BandNormalizedInput(s, b)
    }

  implicit def cogBandNormalizedInput[T](
    implicit ev: Enumerated[Units Of Brightness[T]]
  ): Cogen[BandNormalizedInput[T]] =
    Cogen[(
      UnnormalizedSedInput,
      List[CreateBandBrightnessInput[T]]
    )].contramap { in => (
      in.sed,
      in.brightnesses
    )}

  implicit def arbCreateEmissionLineInput[T](
    implicit ev: Enumerated[Units Of LineFlux[T]]
  ): Arbitrary[CreateEmissionLineInput[T]] =
    Arbitrary {
      for {
        wl <- arbitrary[WavelengthModel.Input]
        lw <- arbitrary[PosBigDecimal]
        lf <- arbitrary[CreateMeasureInput[PosBigDecimal, LineFlux[T]]]
      } yield CreateEmissionLineInput(wl, lw, lf)
    }

  implicit def cogCreateEmissionLineInput[T](
    implicit ev: Enumerated[Units Of LineFlux[T]]
  ): Cogen[CreateEmissionLineInput[T]] =
    Cogen[(
      WavelengthModel.Input,
      PosBigDecimal,
      CreateMeasureInput[PosBigDecimal, LineFlux[T]]
    )].contramap { in => (
      in.wavelength,
      in.lineWidth,
      in.lineFlux
    )}

  implicit def arbEmissionLinesInput[T](
    implicit ev0: Enumerated[Units Of LineFlux[T]],
             ev1: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[EmissionLinesInput[T]] =
    Arbitrary {
      for {
        ls  <- arbitrary[List[CreateEmissionLineInput[T]]]
        fdc <- arbitrary[CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]]
      } yield EmissionLinesInput(ls, fdc)
    }

  implicit def cogCreateEmissionLinesInput[T](
    implicit ev0: Enumerated[Units Of LineFlux[T]],
             ev1: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Cogen[EmissionLinesInput[T]] =
    Cogen[(
      List[CreateEmissionLineInput[T]],
      CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]
    )].contramap { in => (
      in.lines,
      in.fluxDensityContinuum
    )}

  implicit def arbSpectralDefinition[T](
    implicit ev0: Enumerated[Units Of Brightness[T]],
             ev1: Enumerated[Units Of LineFlux[T]],
             ev2: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[SpectralDefinitionInput[T]] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[BandNormalizedInput[T]].map(SpectralDefinitionInput.bandNormalized),
        arbitrary[EmissionLinesInput[T]].map(SpectralDefinitionInput.emissionLines)
      )
    }

  implicit def cogSpectralDefinition[T](
    implicit ev0: Enumerated[Units Of Brightness[T]],
             ev1: Enumerated[Units Of LineFlux[T]],
             ev2: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Cogen[SpectralDefinitionInput[T]] =
    Cogen[(
      Input[BandNormalizedInput[T]],
      Input[EmissionLinesInput[T]]
    )].contramap { in => (
      in.bandNormalized,
      in.emissionLines
    )}

  implicit val arbGaussianInput: Arbitrary[GaussianInput] =
    Arbitrary {
      for {
        f <- arbitrary[Input[AngleModel.AngleInput]]
        s <- arbitrary[Input[SpectralDefinitionInput[Integrated]]]
      } yield GaussianInput(f, s)
    }

  implicit val cogGaussianInput: Cogen[GaussianInput] =
    Cogen[(
      Input[AngleModel.AngleInput],
      Input[SpectralDefinitionInput[Integrated]]
    )].contramap { in => (
      in.fwhm,
      in.spectralDefinition
    )}

  implicit val arbSourceProfileInput: Arbitrary[SourceProfileInput] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[SpectralDefinitionInput[Integrated]].map(SourceProfileInput.point),
        arbitrary[SpectralDefinitionInput[Surface]].map(SourceProfileInput.uniform),
        arbitrary[GaussianInput].map(SourceProfileInput.gaussian)
      )
    }

  implicit val cogSourceProfileInput: Cogen[SourceProfileInput] =
    Cogen[(
      Input[SpectralDefinitionInput[Integrated]],
      Input[SpectralDefinitionInput[Surface]],
      Input[GaussianInput]
    )].contramap { in => (
      in.point,
      in.uniform,
      in.gaussian
    )}
}

object ArbSourceProfileModel extends ArbSourceProfileModel
