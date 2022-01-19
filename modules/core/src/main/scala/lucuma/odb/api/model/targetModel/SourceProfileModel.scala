// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.{Nested, NonEmptyList, NonEmptyMap, StateT}
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import clue.data.{Assign, Ignore, Input, Unassign}
import clue.data.syntax._
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
import lucuma.core.model.{EmissionLine, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.odb.api.model.{AngleModel, EditorInput, EitherInput, InputError, ValidatedInput, WavelengthModel}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.prism._
import lucuma.odb.api.model.syntax.lens._

import scala.collection.immutable.SortedMap

/**
 * SourceProfile GraphQL schema support model.
 */
object SourceProfileModel {

  // TODO: a more appropriate place for this stuff

  def error[S](m: String): StateT[EitherInput, S, Unit] =
    StateT.setF[EitherInput, S](InputError.fromMessage(m).leftNec)

  def empty[S]: StateT[EitherInput, S, Unit] =
    StateT.empty


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
      units.withValueTagged(value)

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

  final case class BandBrightnessPair[T](
    band:    Band,
    measure: Measure[BrightnessValue] Of Brightness[T]
  ) {

    def toTuple: (Band, Measure[BrightnessValue] Of Brightness[T]) =
      (band, measure)

  }

  object BandBrightnessPair {

    implicit def EqBandBrightness[T]: Eq[BandBrightnessPair[T]] =
      Eq.by { a => (
        a.band,
        a.measure
      )}

  }

  final case class CreateBandBrightnessInput[T](
    band:      Band,
    value:     BigDecimal,
    units:     Units Of Brightness[T],
    error:     Option[BigDecimal]
  ) {

    val toBandBrightnessPair: BandBrightnessPair[T] = {
      val m = units.withValueTagged(BrightnessValue.fromBigDecimal.get(value))
      BandBrightnessPair[T](
        band,
        error.fold(m) { e => m.withError(BrightnessValue.fromBigDecimal.get(e)) }
      )
    }

  }

  object CreateBandBrightnessInput {

    implicit def DecoderCreateBandBrightnessInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[CreateBandBrightnessInput[T]] =
      deriveDecoder[CreateBandBrightnessInput[T]]

    implicit def EqCreateBandBrightnessInput[T]: Eq[CreateBandBrightnessInput[T]] =
      Eq.by { a => (a.band, a.value, a.units, a.error) }

  }

  final case class CreateBandNormalizedInput[T](
    sed:          UnnormalizedSedInput,
    brightnesses: List[CreateBandBrightnessInput[T]]
  ) {

    def toBandNormalized: ValidatedInput[BandNormalized[T]] =
      sed.toUnnormalizedSed.map { sed =>
        BandNormalized(
          sed,
          SortedMap.from(brightnesses.map(_.toBandBrightnessPair.toTuple))
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

  final case class WavelengthEmissionLinePair[T](
    wavelength: Wavelength,
    line:       EmissionLine[T]
  ) {

    def toTuple: (Wavelength, EmissionLine[T]) =
      (wavelength, line)
  }

  object WavelengthEmissionLinePair {

    implicit def EqWavelengthEmissionLinePair[T]: Eq[WavelengthEmissionLinePair[T]] =
      Eq.by { a => (
        a.wavelength,
        a.line
      )}

  }


  // TODO: should lineWidth offer other units?  m/s etc?
  final case class CreateEmissionLineInput[T](
    wavelength: WavelengthModel.Input,
    lineWidth:  PosBigDecimal,
    lineFlux:   CreateMeasureInput[PosBigDecimal, LineFlux[T]]
  ) {

    def toWavelengthEmissionLinePair: ValidatedInput[WavelengthEmissionLinePair[T]] =
      wavelength.toWavelength("wavelength").map { w =>
        WavelengthEmissionLinePair(
          w,
          EmissionLine(
            Quantity[PosBigDecimal, KilometersPerSecond](lineWidth),
            lineFlux.toMeasure
          )
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
        .traverse(_.toWavelengthEmissionLinePair)
        .map { lst =>
          EmissionLines(SortedMap.from(lst.map(_.toTuple)), fluxDensityContinuum.toMeasure)
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

  final case class SpectralDefinitionInput[T](
    bandNormalized: Input[CreateBandNormalizedInput[T]] = Input.ignore,
    emissionLines:  Input[CreateEmissionLinesInput[T]]  = Input.ignore
  ) extends EditorInput[SpectralDefinition[T]] {

    override val create: ValidatedInput[SpectralDefinition[T]] =
      ValidatedInput.requireOne("spectralDefinition",
        bandNormalized.toOption.map(_.toBandNormalized),
        emissionLines.toOption.map(_.toEmissionLines)
      )

    override val edit: StateT[EitherInput, SpectralDefinition[T], Unit] = {

      def createBandNormalized(b: CreateBandNormalizedInput[T]): StateT[EitherInput, SpectralDefinition[T], Unit] =
        SpectralDefinition.bandNormalized[T].transformOrIgnore(
          StateT.setF[EitherInput, BandNormalized[T]](b.toBandNormalized.toEither)
        )

      def editBandNormalized(b: CreateBandNormalizedInput[T]): StateT[EitherInput, SpectralDefinition[T], Unit] =
        SpectralDefinition.bandNormalized[T].transformOrIgnore(
          StateT.setF[EitherInput, BandNormalized[T]](b.toBandNormalized.toEither)
        )

      def createEmissionLines(e: CreateEmissionLinesInput[T]): StateT[EitherInput, SpectralDefinition[T], Unit] =
        SpectralDefinition.emissionLines[T].transformOrIgnore(
          StateT.setF[EitherInput, EmissionLines[T]](e.toEmissionLines.toEither)
        )

      def editEmissionLines(e: CreateEmissionLinesInput[T]): StateT[EitherInput, SpectralDefinition[T], Unit] =
        SpectralDefinition.emissionLines[T].transformOrIgnore(
          StateT.setF[EitherInput, EmissionLines[T]](e.toEmissionLines.toEither)
        )

      val oneIsRequired: StateT[EitherInput, SpectralDefinition[T], Unit] =
        error("One of `bandNormalized` or `emissionLines` must be set")

      def fold(
        ifBandNormalized: StateT[EitherInput, SpectralDefinition[T], Unit],
        ifEmissionLines:  StateT[EitherInput, SpectralDefinition[T], Unit]
      ): StateT[EitherInput, SpectralDefinition[T], Unit] =
        StateT.get[EitherInput, SpectralDefinition[T]].flatMap {
          case _: BandNormalized[T] => ifBandNormalized
          case _: EmissionLines[T]  => ifEmissionLines
        }

      (bandNormalized, emissionLines) match {
        case (Assign(_), Assign(_)) => error("Assign one of `bandNormalized` or `emissionLines`, but not both")
        case (Unassign,  Unassign ) => oneIsRequired
        case (Ignore,    Ignore   ) => empty
        case (Assign(b), _        ) => fold(editBandNormalized(b), createBandNormalized(b))
        case (_        , Assign(e)) => fold(createEmissionLines(e), editEmissionLines(e))
        case (Unassign,  _        ) => fold(oneIsRequired, empty)
        case (_,         Unassign ) => fold(empty, oneIsRequired)
      }

    }

  }

  object SpectralDefinitionInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit def DecoderSpectralDefinitionInput[T](
      implicit ev0: Decoder[Units Of Brightness[T]],
               ev1: Decoder[Units Of LineFlux[T]],
               ev2: Decoder[Units Of FluxDensityContinuum[T]]
    ): Decoder[SpectralDefinitionInput[T]] =
      deriveConfiguredDecoder[SpectralDefinitionInput[T]]

    implicit def EqSpectralDefinitionInput[T]: Eq[SpectralDefinitionInput[T]] =
      Eq.by { a => (
        a.bandNormalized,
        a.emissionLines
      )}

    def Empty[T]: SpectralDefinitionInput[T] =
      SpectralDefinitionInput[T](Input.ignore, Input.ignore)

    def bandNormalized[T](bn: CreateBandNormalizedInput[T]): SpectralDefinitionInput[T] =
      Empty[T].copy(bandNormalized = bn.assign)

    def emissionLines[T](el: CreateEmissionLinesInput[T]): SpectralDefinitionInput[T] =
      Empty[T].copy(emissionLines = el.assign)
  }

  final case class GaussianInput(
    fwhm:               Input[AngleModel.AngleInput]               = Input.ignore,
    spectralDefinition: Input[SpectralDefinitionInput[Integrated]] = Input.ignore
  ) extends EditorInput[SourceProfile.Gaussian] {

    override val create: ValidatedInput[SourceProfile.Gaussian] =
      (fwhm.notMissingAndThen("fwhm")(_.toAngle),
       spectralDefinition.notMissingAndThen("spectralDefinition")(_.create)
      ).mapN { (a, s) => SourceProfile.Gaussian(a, s) }

    override val edit: StateT[EitherInput, SourceProfile.Gaussian, Unit] = {
      for {
        a <- StateT.liftF(fwhm.validateNotNullable("fwhm")(_.toAngle).toEither)
        _ <- SourceProfile.Gaussian.fwhm               := a
        _ <- SourceProfile.Gaussian.spectralDefinition :! spectralDefinition
      } yield ()
    }

  }

  object GaussianInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderCreateGaussianInput: Decoder[GaussianInput] =
      deriveConfiguredDecoder[GaussianInput]

    implicit val EqCreateGaussianInput: Eq[GaussianInput] =
      Eq.by { a => (
        a.fwhm,
        a.spectralDefinition
      )}

  }

  final case class SourceProfileInput(
    point:    Input[SpectralDefinitionInput[Integrated]] = Input.ignore,
    uniform:  Input[SpectralDefinitionInput[Surface]]    = Input.ignore,
    gaussian: Input[GaussianInput]                       = Input.ignore
  ) extends EditorInput[SourceProfile] {

    override val create: ValidatedInput[SourceProfile] =
      ValidatedInput.requireOne("sourceProfile",
        point.map(_.create.map(SourceProfile.Point(_))).toOption,
        uniform.map(_.create.map(SourceProfile.Uniform(_))).toOption,
        gaussian.map(_.create).toOption
      )

    override val edit: StateT[EitherInput, SourceProfile, Unit] =
      (point.toOption, uniform.toOption, gaussian.toOption) match {
        case (Some(p), None,    None   ) => StateT.setF(p.create.toEither.map(SourceProfile.Point(_)))
        case (None,    Some(u), None   ) => StateT.setF(u.create.toEither.map(SourceProfile.Uniform(_)))
        case (None,    None,    Some(g)) => StateT.setF(g.create.toEither)
        case _                           => StateT.setF(InputError.fromMessage("""exactly one of "point", "uniform", or "gaussian" must be set""").leftNec[SourceProfile])
      }

  }

  object SourceProfileInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderSourceProfileInput: Decoder[SourceProfileInput] =
      deriveConfiguredDecoder[SourceProfileInput]

    implicit val EqSourceProfileInput: Eq[SourceProfileInput] =
      Eq.by { a => (
        a.point,
        a.uniform,
        a.gaussian
      )}

    val Empty: SourceProfileInput =
      SourceProfileInput(Input.ignore, Input.ignore, Input.ignore)

    def point(sd: SpectralDefinitionInput[Integrated]): SourceProfileInput =
      Empty.copy(point = sd.assign)

    def uniform(sd: SpectralDefinitionInput[Surface]): SourceProfileInput =
      Empty.copy(uniform = sd.assign)

    def gaussian(g: GaussianInput): SourceProfileInput =
      Empty.copy(gaussian = g.assign)

  }

}
