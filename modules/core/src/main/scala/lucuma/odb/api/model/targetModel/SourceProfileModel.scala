// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.{Nested, NonEmptyList, NonEmptyMap}
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import coulomb.Quantity
import coulomb.si.Kelvin
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosBigDecimal
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import lucuma.core.`enum`.{Band, CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.{Measure, Of, Units}
import lucuma.core.math.units.KilometersPerSecond
import lucuma.core.math.{BrightnessValue, Wavelength}
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.{BandBrightness, EmissionLine, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.odb.api.model.{AngleModel, InputError, ValidatedInput, WavelengthModel}

import scala.collection.immutable.SortedMap

/**
 * SourceProfile GraphQL schema support model.
 */
object SourceProfileModel {

  final case class FluxDensityEntry(
    wavelength: Wavelength,
    density:    PosBigDecimal
  )

  object FluxDensityEntry {

    def EqFluxDensityEntry: Eq[FluxDensityEntry] =
      Eq.by { a => (
        a.wavelength,
        a.density
      )}

  }

  final case class FluxDensityInput(
    wavelength: WavelengthModel.Input,
    density:    PosBigDecimal
  ) {

    val toFluxDensityEntry: ValidatedInput[FluxDensityEntry] =
      wavelength.toWavelength("wavelength").map { w =>
        FluxDensityEntry(w, density)
      }

  }

  object FluxDensityInput {

    implicit val DecoderFluxDensityInput: Decoder[FluxDensityInput] =
      deriveDecoder[FluxDensityInput]

    implicit val EqFluxDensityInput: Eq[FluxDensityInput] =
      Eq.by { a => (
        a.wavelength,
        a.density
      )}

  }

  final case class UnnormalizedSedInput(
    stellarLibrary:  Option[StellarLibrarySpectrum],
    coolStar:        Option[CoolStarTemperature],
    galaxy:          Option[GalaxySpectrum],
    planet:          Option[PlanetSpectrum],
    quasar:          Option[QuasarSpectrum],
    hiiRegion:       Option[HIIRegionSpectrum],
    planetaryNebula: Option[PlanetaryNebulaSpectrum],
    powerLaw:        Option[BigDecimal],
    blackBodyTempK:  Option[PosBigDecimal],
    fluxDensities:   Option[List[FluxDensityInput]]
  ) {

    val toUserDefined: Option[ValidatedInput[UnnormalizedSED.UserDefined]] =
      fluxDensities.map { fluxDensityInputs =>
        NonEmptyList
          .fromList(fluxDensityInputs)
          .toValidNec(InputError.fromMessage("One or more flux densities must be provided for a user defined SED"))
          .andThen(_.traverse(_.toFluxDensityEntry))
          .map { nel =>
            UnnormalizedSED.UserDefined(
              NonEmptyMap(
                nel.head.wavelength -> nel.head.density,
                SortedMap.from {
                  nel.tail.map { entry => entry.wavelength -> entry.density }
                }
              )
            )
          }
      }


    val toUnnormalizedSed: ValidatedInput[UnnormalizedSED] =
      ValidatedInput.requireOne(
        "sed",
        Nested(List[Option[UnnormalizedSED]](
          stellarLibrary .map(UnnormalizedSED.StellarLibrary(_)),
          coolStar       .map(UnnormalizedSED.CoolStarModel(_)),
          galaxy         .map(UnnormalizedSED.Galaxy(_)),
          planet         .map(UnnormalizedSED.Planet(_)),
          quasar         .map(UnnormalizedSED.Quasar(_)),
          hiiRegion      .map(UnnormalizedSED.HIIRegion(_)),
          planetaryNebula.map(UnnormalizedSED.PlanetaryNebula(_)),
          powerLaw       .map(UnnormalizedSED.PowerLaw(_)),
          blackBodyTempK .map(k => UnnormalizedSED.BlackBody(Quantity[PosBigDecimal, Kelvin](k))),
        )).map(_.validNec[InputError]).value :+ toUserDefined
      )
  }

  object UnnormalizedSedInput {

    implicit val DecoderUnnormalizedSedInput: Decoder[UnnormalizedSedInput] =
      deriveDecoder[UnnormalizedSedInput]

    implicit val EqUnnormalizedSedInput: Eq[UnnormalizedSedInput] =
      Eq.by { a => (
        a.stellarLibrary,
        a.coolStar,
        a.galaxy,
        a.planet,
        a.quasar,
        a.hiiRegion,
        a.planetaryNebula,
        a.powerLaw,
        a.blackBodyTempK,
        a.fluxDensities
      )}

    val Empty: UnnormalizedSedInput =
      UnnormalizedSedInput(None, None, None, None, None, None, None, None, None, None)

    def stellarLibrary(e: StellarLibrarySpectrum): UnnormalizedSedInput =
      Empty.copy(stellarLibrary = e.some)

    def coolStar(e: CoolStarTemperature): UnnormalizedSedInput =
      Empty.copy(coolStar = e.some)

    def galaxy(e: GalaxySpectrum): UnnormalizedSedInput =
      Empty.copy(galaxy = e.some)

    def planet(e: PlanetSpectrum): UnnormalizedSedInput =
      Empty.copy(planet = e.some)

    def hiiRegionSpectrum(e: HIIRegionSpectrum): UnnormalizedSedInput =
      Empty.copy(hiiRegion = e.some)

    def planetaryNebula(e: PlanetaryNebulaSpectrum): UnnormalizedSedInput =
      Empty.copy(planetaryNebula = e.some)

    def powerLaw(d: BigDecimal): UnnormalizedSedInput =
      Empty.copy(powerLaw = d.some)

    def blackBody(t: PosBigDecimal): UnnormalizedSedInput =
      Empty.copy(blackBodyTempK = t.some)

    def userDefined(lst: List[FluxDensityInput]): UnnormalizedSedInput =
      Empty.copy(fluxDensities = lst.some)

  }

  final case class CreateMeasureInput[V, U](
    value: V,
    units: Units Of U
  ) {

    def toMeasure: Measure[V] Of U =
      toMappedMeasure[V](identity)

    def toMappedMeasure[N](f: V => N): Measure[N] Of U =
      units.withValueTagged(f(value))

  }

  object CreateMeasureInput {

    implicit def DecoderCreateMeasureInput[V: Decoder, U](
      implicit ev: Decoder[Units Of U]
    ): Decoder[CreateMeasureInput[V, U]] =
      deriveDecoder[CreateMeasureInput[V, U]]

    implicit def EqMeasureInput[V: Eq, U](
      implicit ev: Eq[Units Of U]
    ): Eq[CreateMeasureInput[V, U]] =
      Eq.by { a => (
        a.value,
        a.units
      )}

  }

  final case class CreateBandBrightnessInput[T](
    brightness: CreateMeasureInput[BigDecimal, Brightness[T]],
    band:       Band,
    error:      Option[BigDecimal]
  ) {

    val toBandBrightness: BandBrightness[T] =
      BandBrightness(
        brightness.toMappedMeasure(BrightnessValue.fromBigDecimal.get),
        band,
        error.map(e => BrightnessValue.fromBigDecimal.get(e))
      )

  }

  object CreateBandBrightnessInput {

    implicit def DecoderCreateBandBrightnessInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[CreateBandBrightnessInput[T]] =
      deriveDecoder[CreateBandBrightnessInput[T]]

    implicit def EqCreateBandBrightnessInput[T]: Eq[CreateBandBrightnessInput[T]] =
      Eq.by { a => (a.brightness, a.band, a.error) }

  }

  final case class CreateBandNormalizedInput[T](
    sed:          UnnormalizedSedInput,
    brightnesses: List[CreateBandBrightnessInput[T]]
  ) {

    def toBandNormalized: ValidatedInput[BandNormalized[T]] =
      sed.toUnnormalizedSed.map { sed =>
        BandNormalized(
          sed,
          SortedMap.from(brightnesses.map(_.toBandBrightness).fproductLeft(_.band))
        )
      }

  }

  object CreateBandNormalizedInput {

    implicit def DecoderCreateBandNormalizedInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[CreateBandNormalizedInput[T]] =
      deriveDecoder[CreateBandNormalizedInput[T]]

    implicit def EqCreateBandNormalizedInput[T]: Eq[CreateBandNormalizedInput[T]] =
      Eq.by { a => (a.sed, a.brightnesses) }

  }

  // TODO: should lineWidth offer other units?  m/s etc?
  final case class CreateEmissionLineInput[T](
    wavelength: WavelengthModel.Input,
    lineWidth:  PosBigDecimal,
    lineFlux:   CreateMeasureInput[PosBigDecimal, LineFlux[T]]
  ) {

    def toEmissionLine: ValidatedInput[EmissionLine[T]] =
      wavelength.toWavelength("wavelength").map { w =>
        EmissionLine(
          w,
          Quantity[PosBigDecimal, KilometersPerSecond](lineWidth),
          lineFlux.toMeasure
        )
      }

  }

  object CreateEmissionLineInput {

    implicit def DecoderCreateEmissionLine[T](
      implicit ev: Decoder[Units Of LineFlux[T]]
    ): Decoder[CreateEmissionLineInput[T]] =
      deriveDecoder[CreateEmissionLineInput[T]]

    implicit def EqCreateEmissionLine[T]: Eq[CreateEmissionLineInput[T]] =
      Eq.by { a => (
        a.wavelength,
        a.lineWidth,
        a.lineFlux
      )}

  }

  final case class CreateEmissionLinesInput[T](
    lines:                List[CreateEmissionLineInput[T]],
    fluxDensityContinuum: CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]
  ) {

    def toEmissionLines: ValidatedInput[EmissionLines[T]] =
      lines
        .traverse(_.toEmissionLine)
        .map { lst =>
          EmissionLines(
            SortedMap.from(lst.fproductLeft(_.wavelength)),
            fluxDensityContinuum.toMeasure
          )
        }

  }

  object CreateEmissionLinesInput {

    implicit def DecoderCreateEmissionsLineInput[T](
      implicit ev0: Decoder[Units Of LineFlux[T]], ev1: Decoder[Units Of FluxDensityContinuum[T]]
    ): Decoder[CreateEmissionLinesInput[T]] =
      deriveDecoder[CreateEmissionLinesInput[T]]

    implicit def EqCreateEmissionLinesInput[T]: Eq[CreateEmissionLinesInput[T]] =
      Eq.by { a => (
        a.lines,
        a.fluxDensityContinuum
      )}
  }

  final case class CreateSpectralDefinitionInput[T](
    bandNormalized: Option[CreateBandNormalizedInput[T]],
    emissionLines:  Option[CreateEmissionLinesInput[T]]
  ) {

    def toSpectralDefinition: ValidatedInput[SpectralDefinition[T]] =
      ValidatedInput.requireOne("spectralDefinition",
        bandNormalized.map(_.toBandNormalized),
        emissionLines.map(_.toEmissionLines)
      )

  }

  object CreateSpectralDefinitionInput {

    implicit def DecoderCreateSpectralDefinitionInput[T](
      implicit ev0: Decoder[Units Of Brightness[T]],
               ev1: Decoder[Units Of LineFlux[T]],
               ev2: Decoder[Units Of FluxDensityContinuum[T]]
    ): Decoder[CreateSpectralDefinitionInput[T]] =
      deriveDecoder[CreateSpectralDefinitionInput[T]]

    implicit def EqCreateSpectralDefinitionInput[T]: Eq[CreateSpectralDefinitionInput[T]] =
      Eq.by { a => (
        a.bandNormalized,
        a.emissionLines
      )}

    def Empty[T]: CreateSpectralDefinitionInput[T] =
      CreateSpectralDefinitionInput[T](None, None)

    def bandNormalized[T](bn: CreateBandNormalizedInput[T]): CreateSpectralDefinitionInput[T] =
      Empty[T].copy(bandNormalized = bn.some)

    def emissionLines[T](el: CreateEmissionLinesInput[T]): CreateSpectralDefinitionInput[T] =
      Empty[T].copy(emissionLines = el.some)
  }

  final case class CreateGaussianInput(
    fwhm:               AngleModel.AngleInput,
    spectralDefinition: CreateSpectralDefinitionInput[Integrated]
  ) {

    def toGaussian: ValidatedInput[SourceProfile.Gaussian] =
      (fwhm.toAngle, spectralDefinition.toSpectralDefinition).mapN { (a, s) =>
        SourceProfile.Gaussian(a, s)
      }

  }

  object CreateGaussianInput {

    implicit val DecoderCreateGaussianInput: Decoder[CreateGaussianInput] =
      deriveDecoder[CreateGaussianInput]

    implicit val EqCreateGaussianInput: Eq[CreateGaussianInput] =
      Eq.by { a => (
        a.fwhm,
        a.spectralDefinition
      )}

  }

  final case class CreateSourceProfileInput(
    point:    Option[CreateSpectralDefinitionInput[Integrated]],
    uniform:  Option[CreateSpectralDefinitionInput[Surface]],
    gaussian: Option[CreateGaussianInput]
  ) {

    def toSourceProfile: ValidatedInput[SourceProfile] =
      ValidatedInput.requireOne("sourceProfile",
        point.map(_.toSpectralDefinition.map(SourceProfile.Point(_))),
        uniform.map(_.toSpectralDefinition.map(SourceProfile.Uniform(_))),
        gaussian.map(_.toGaussian)
      )

  }

  object CreateSourceProfileInput {

    implicit val DecoderCreateSourceProfileInput: Decoder[CreateSourceProfileInput] =
      deriveDecoder[CreateSourceProfileInput]

    implicit val EqCreateSourceProfileInput: Eq[CreateSourceProfileInput] =
      Eq.by { a => (
        a.point,
        a.uniform,
        a.gaussian
      )}

    val Empty: CreateSourceProfileInput =
      CreateSourceProfileInput(None, None, None)

    def point(sd: CreateSpectralDefinitionInput[Integrated]): CreateSourceProfileInput =
      Empty.copy(point = sd.some)

    def uniform(sd: CreateSpectralDefinitionInput[Surface]): CreateSourceProfileInput =
      Empty.copy(uniform = sd.some)

    def gaussian(g: CreateGaussianInput): CreateSourceProfileInput =
      Empty.copy(gaussian = g.some)

  }

}
