// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

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

  implicit def arbCreateBandNormalizedInput[T](
    implicit ev: Enumerated[Units Of Brightness[T]]
  ): Arbitrary[CreateBandNormalizedInput[T]] =
    Arbitrary {
      for {
        s <- arbitrary[UnnormalizedSedInput]
        b <- arbitrary[List[CreateBandBrightnessInput[T]]]
      } yield CreateBandNormalizedInput(s, b)
    }

  implicit def cogCreateBandNormalizedInput[T](
    implicit ev: Enumerated[Units Of Brightness[T]]
  ): Cogen[CreateBandNormalizedInput[T]] =
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

  implicit def arbCreateEmissionLinesInput[T](
    implicit ev0: Enumerated[Units Of LineFlux[T]],
             ev1: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[CreateEmissionLinesInput[T]] =
    Arbitrary {
      for {
        ls  <- arbitrary[List[CreateEmissionLineInput[T]]]
        fdc <- arbitrary[CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]]
      } yield CreateEmissionLinesInput(ls, fdc)
    }

  implicit def cogCreateCreateEmissionLinesInput[T](
    implicit ev0: Enumerated[Units Of LineFlux[T]],
             ev1: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Cogen[CreateEmissionLinesInput[T]] =
    Cogen[(
      List[CreateEmissionLineInput[T]],
      CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]
    )].contramap { in => (
      in.lines,
      in.fluxDensityContinuum
    )}

  implicit def arbCreateSpectralDefinition[T](
    implicit ev0: Enumerated[Units Of Brightness[T]],
             ev1: Enumerated[Units Of LineFlux[T]],
             ev2: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[CreateSpectralDefinitionInput[T]] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[CreateBandNormalizedInput[T]].map(CreateSpectralDefinitionInput.bandNormalized),
        arbitrary[CreateEmissionLinesInput[T]].map(CreateSpectralDefinitionInput.emissionLines)
      )
    }

  implicit def cogCreateSpectralDefinition[T](
    implicit ev0: Enumerated[Units Of Brightness[T]],
             ev1: Enumerated[Units Of LineFlux[T]],
             ev2: Enumerated[Units Of FluxDensityContinuum[T]]
  ): Cogen[CreateSpectralDefinitionInput[T]] =
    Cogen[(
      Option[CreateBandNormalizedInput[T]],
      Option[CreateEmissionLinesInput[T]]
    )].contramap { in => (
      in.bandNormalized,
      in.emissionLines
    )}

  implicit val arbCreateGaussianInput: Arbitrary[CreateGaussianInput] =
    Arbitrary {
      for {
        f <- arbitrary[AngleModel.AngleInput]
        s <- arbitrary[CreateSpectralDefinitionInput[Integrated]]
      } yield CreateGaussianInput(f, s)
    }

  implicit val cogCreateGaussianInput: Cogen[CreateGaussianInput] =
    Cogen[(
      AngleModel.AngleInput,
      CreateSpectralDefinitionInput[Integrated]
    )].contramap { in => (
      in.fwhm,
      in.spectralDefinition
    )}

  implicit val arbCreateSourceProfileInput: Arbitrary[CreateSourceProfileInput] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[CreateSpectralDefinitionInput[Integrated]].map(CreateSourceProfileInput.point),
        arbitrary[CreateSpectralDefinitionInput[Surface]].map(CreateSourceProfileInput.uniform),
        arbitrary[CreateGaussianInput].map(CreateSourceProfileInput.gaussian)
      )
    }

  implicit val cogCreateSourceProfileInput: Cogen[CreateSourceProfileInput] =
    Cogen[(
      Option[CreateSpectralDefinitionInput[Integrated]],
      Option[CreateSpectralDefinitionInput[Surface]],
      Option[CreateGaussianInput]
    )].contramap { in => (
      in.point,
      in.uniform,
      in.gaussian
    )}
}

object ArbSourceProfileModel extends ArbSourceProfileModel
