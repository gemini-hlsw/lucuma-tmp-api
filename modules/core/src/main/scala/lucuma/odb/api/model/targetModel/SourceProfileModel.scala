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
import clue.data.Input
import clue.data.syntax._
import coulomb.Quantity
import coulomb.si.Kelvin
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.{PosBigDecimal, PosInt}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.refined._
import lucuma.core.enums.{Band, CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.{Measure, Of, Units}
import lucuma.core.math.units.KilometersPerSecond
import lucuma.core.math.Wavelength
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.{EmissionLine, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.odb.api.model.{AngleModel, EditorInput, EitherInput, InputError, ValidatedInput, WavelengthModel}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.{Focus, Lens}

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
    wavelength: WavelengthModel.WavelengthInput,
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
    blackBodyTempK:  Option[PosInt],
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
          blackBodyTempK .map(k => UnnormalizedSED.BlackBody(Quantity[PosInt, Kelvin](k))),
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

    def blackBody(t: PosInt): UnnormalizedSedInput =
      Empty.copy(blackBodyTempK = t.some)

    def userDefined(lst: List[FluxDensityInput]): UnnormalizedSedInput =
      Empty.copy(fluxDensities = lst.some)

  }

  final case class MeasureInput[V, U](
    value: V,
    units: Units Of U
  ) {

    def toMeasure: Measure[V] Of U =
      units.withValueTagged(value)

  }

  object MeasureInput {

    implicit def DecoderMeasureInput[V: Decoder, U](
      implicit ev: Decoder[Units Of U]
    ): Decoder[MeasureInput[V, U]] =
      deriveDecoder[MeasureInput[V, U]]

    implicit def EqMeasureInput[V: Eq, U](
      implicit ev: Eq[Units Of U]
    ): Eq[MeasureInput[V, U]] =
      Eq.by { a => (
        a.value,
        a.units
      )}

  }

  final case class BandBrightnessPair[T](
    band:    Band,
    measure: Measure[BigDecimal] Of Brightness[T]
  ) {

    def toTuple: (Band, Measure[BigDecimal] Of Brightness[T]) =
      (band, measure)

  }

  object BandBrightnessPair {

    implicit def EqBandBrightness[T]: Eq[BandBrightnessPair[T]] =
      Eq.by { a => (
        a.band,
        a.measure
      )}

    def measure[T]: Lens[BandBrightnessPair[T], Measure[BigDecimal] Of Brightness[T]] =
      Focus[BandBrightnessPair[T]](_.measure)

  }

  final case class BandBrightnessInput[T](
    band:  Band,
    value: Input[BigDecimal]             = Input.ignore,
    units: Input[Units Of Brightness[T]] = Input.ignore,
    error: Input[BigDecimal]             = Input.ignore
  ) extends EditorInput[BandBrightnessPair[T]] {

    override val create: ValidatedInput[BandBrightnessPair[T]] =
      (value.notMissing("value"),
       units.notMissing("units")
      ).mapN { (v, u) =>
        val m = u.withValueTagged(v)
        BandBrightnessPair[T](
          band,
          error.toOption.fold(m)(m.withError)
        )
      }

    override val edit: StateT[EitherInput, BandBrightnessPair[T], Unit] = {
      val validArgs = (
        value.validateIsNotNull("value"),
        units.validateIsNotNull("units")
      ).tupled

      for {
        args <- validArgs.liftState
        (v, u)  = args
        measure = StateT.modify[EitherInput, Measure[BigDecimal] Of Brightness[T]] { m =>
          val newValue = v.getOrElse(m.value)
          val newUnits = u.getOrElse(Measure.unitsTagged.get(m))
          val newError = error.fold(m.error, Option.empty[BigDecimal], _.some)

          val newMeasure = newUnits.withValueTagged(newValue)
          newError.fold(newMeasure)(newMeasure.withError)
        }
        _  <- BandBrightnessPair.measure[T] :< measure.some
      } yield ()
    }
  }

  object BandBrightnessInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit def DecoderBandBrightnessInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[BandBrightnessInput[T]] =
      deriveConfiguredDecoder[BandBrightnessInput[T]]

    implicit def EqBandBrightnessInput[T]: Eq[BandBrightnessInput[T]] =
      Eq.by { a => (a.band, a.value, a.units, a.error) }

  }

  final case class BandNormalizedInput[T](
    sed:          Input[UnnormalizedSedInput]         = Input.ignore,
    brightnesses: Input[List[BandBrightnessInput[T]]] = Input.ignore
  ) extends EditorInput[BandNormalized[T]] {

    import lucuma.odb.api.model.targetModel.SourceProfileModel.BandNormalizedInput._

    override val create: ValidatedInput[BandNormalized[T]] = {
      // we can't update lucuma-core thus we need to default to somthing
      val modelSed = sed.toOption.map(_.toUnnormalizedSed)
        .getOrElse(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.O5V).validNec)
      (modelSed, brightnesses.notMissingAndThen("brightnesses")(brightnessCreator[T])).mapN { (sed, bright) =>
        BandNormalized(sed, bright)
      }
    }

    override val edit: StateT[EitherInput, BandNormalized[T], Unit] =
      for {
        s <- sed.validateNotNullable("sed")(_.toUnnormalizedSed).liftState
        _ <- BandNormalized.sed[T]          := s
        _ <- BandNormalized.brightnesses[T] :<
               brightnesses.fold(
                 brightnessNoop[T],
                 StateT.setF(emptyBrightnessMap[T].rightNec[InputError]),
                 in => StateT.setF(brightnessCreator[T](in).toEither)
               ).some
      } yield ()
  }

  object BandNormalizedInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit def DecoderBandNormalizedInput[T](
      implicit ev: Decoder[Units Of Brightness[T]]
    ): Decoder[BandNormalizedInput[T]] =
      deriveConfiguredDecoder[BandNormalizedInput[T]]

    implicit def EqBandNormalizedInput[T]: Eq[BandNormalizedInput[T]] =
      Eq.by { a => (a.sed, a.brightnesses) }

    private def emptyBrightnessMap[T]: SortedMap[Band, BrightnessMeasure[T]] =
      SortedMap.empty

    private def brightnessNoop[T]: StateT[EitherInput, SortedMap[Band, BrightnessMeasure[T]], Unit] =
      StateT.empty

    private def brightnessCreator[T](
      inputs: List[BandBrightnessInput[T]]
    ): ValidatedInput[SortedMap[Band, BrightnessMeasure[T]]] =
        inputs
          .traverse(_.create)
          .map(lst => SortedMap.from(lst.map(_.toTuple)))

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

    def line[T]: Lens[WavelengthEmissionLinePair[T], EmissionLine[T]] =
      Focus[WavelengthEmissionLinePair[T]](_.line)

  }


  // TODO: should lineWidth offer other units?  m/s etc?
  final case class EmissionLineInput[T](
    wavelength: WavelengthModel.WavelengthInput,
    lineWidth:  Input[PosBigDecimal]                                  = Input.ignore,
    lineFlux:   Input[MeasureInput[PosBigDecimal, LineFlux[T]]] = Input.ignore
  ) extends EditorInput[WavelengthEmissionLinePair[T]] {

    override val create: ValidatedInput[WavelengthEmissionLinePair[T]] =
      (wavelength.toWavelength("wavelength"),
       lineWidth.notMissing("lineWidth"),
       lineFlux.notMissing("lineFlux")
      ).mapN { (w, lw, lf) =>
        WavelengthEmissionLinePair(
          w,
          EmissionLine(
            Quantity[PosBigDecimal, KilometersPerSecond](lw),
            lf.toMeasure
          )
        )
      }

    override val edit: StateT[EitherInput, WavelengthEmissionLinePair[T], Unit] = {
      val validArgs = (
        wavelength.toWavelength("wavelength"),
        lineWidth.validateIsNotNull("lineWidth"),
        lineFlux.validateIsNotNull("lineFlux")
      ).tupled

      for {
        args <- validArgs.liftState
        (_, lw, lf) = args

        line = lw.orElse(lf).as(
          StateT.modify[EitherInput, EmissionLine[T]] { l =>
            val newWidth = lw.map(d => Quantity[PosBigDecimal, KilometersPerSecond](d)).getOrElse(l.lineWidth)
            val newFlux  = lf.map(_.toMeasure).getOrElse(l.lineFlux)
            EmissionLine(newWidth, newFlux)
          }
        )

        _ <- WavelengthEmissionLinePair.line[T] :< line
      } yield ()
    }


  }

  object EmissionLineInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit def DecoderEmissionLineInput[T](
      implicit ev: Decoder[Units Of LineFlux[T]]
    ): Decoder[EmissionLineInput[T]] =
      deriveConfiguredDecoder[EmissionLineInput[T]]

    implicit def EqEmissionLineInput[T]: Eq[EmissionLineInput[T]] =
      Eq.by { a => (
        a.wavelength,
        a.lineWidth,
        a.lineFlux
      )}

  }

  final case class EmissionLinesInput[T](
    lines:                Input[List[EmissionLineInput[T]]]                           = Input.ignore,
    fluxDensityContinuum: Input[MeasureInput[PosBigDecimal, FluxDensityContinuum[T]]] = Input.ignore
  ) extends EditorInput[EmissionLines[T]] {

    import lucuma.odb.api.model.targetModel.SourceProfileModel.EmissionLinesInput._

    override val create: ValidatedInput[EmissionLines[T]] =
      (lines.notMissingAndThen("lines")(lineCreator[T]),
       fluxDensityContinuum.notMissing("fluxDensityContinuum")
      ).mapN { (lines, fdc) => EmissionLines(lines, fdc.toMeasure) }

    override val edit: StateT[EitherInput, EmissionLines[T], Unit] =
      for {
        f <- fluxDensityContinuum.validateIsNotNull("fluxDensityContinuum").liftState
        _ <- EmissionLines.lines[T]                :<
             lines.fold(
               lineNoop[T],
               StateT.setF(emptyLineMap[T].rightNec[InputError]),
               in => StateT.setF(lineCreator[T](in).toEither)
             ).some

        _ <- EmissionLines.fluxDensityContinuum[T] := f.map(_.toMeasure)
      } yield ()


  }

  object EmissionLinesInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit def DecoderEmissionsLineInput[T](
      implicit ev0: Decoder[Units Of LineFlux[T]], ev1: Decoder[Units Of FluxDensityContinuum[T]]
    ): Decoder[EmissionLinesInput[T]] =
      deriveConfiguredDecoder[EmissionLinesInput[T]]

    implicit def EqEmissionLinesInput[T]: Eq[EmissionLinesInput[T]] =
      Eq.by { a => (
        a.lines,
        a.fluxDensityContinuum
      )}

    private def emptyLineMap[T]: SortedMap[Wavelength, EmissionLine[T]] =
      SortedMap.empty

    private def lineNoop[T]: StateT[EitherInput, SortedMap[Wavelength, EmissionLine[T]], Unit] =
      StateT.empty

    def lineCreator[T](
      inputs: List[EmissionLineInput[T]]
    ): ValidatedInput[SortedMap[Wavelength, EmissionLine[T]]] =
      inputs
        .traverse(_.create)
        .map(lst => SortedMap.from(lst.map(_.toTuple)))

  }

  final case class SpectralDefinitionInput[T](
    bandNormalized: Input[BandNormalizedInput[T]] = Input.ignore,
    emissionLines:  Input[EmissionLinesInput[T]]  = Input.ignore
  ) extends EditorInput[SpectralDefinition[T]] {

    override val create: ValidatedInput[SpectralDefinition[T]] =
      ValidatedInput.requireOne("spectralDefinition",
        bandNormalized.toOption.map(_.create),
        emissionLines.toOption.map(_.create)
      )

    override val edit: StateT[EitherInput, SpectralDefinition[T], Unit] =
      EditorInput.editOneOf[SpectralDefinition[T], BandNormalized[T], EmissionLines[T]](
        ("bandNormalized", bandNormalized, SpectralDefinition.bandNormalized[T]),
        ("emissionLines",  emissionLines,  SpectralDefinition.emissionLines[T] )
      )
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

    def bandNormalized[T](bn: BandNormalizedInput[T]): SpectralDefinitionInput[T] =
      Empty[T].copy(bandNormalized = bn.assign)

    def emissionLines[T](el: EmissionLinesInput[T]): SpectralDefinitionInput[T] =
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

    implicit val DecoderGaussianInput: Decoder[GaussianInput] =
      deriveConfiguredDecoder[GaussianInput]

    implicit val EqGaussianInput: Eq[GaussianInput] =
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

    override val edit: StateT[EitherInput, SourceProfile, Unit] = {
      val pointInput: Input[EditorInput[SourceProfile.Point]] =
        point.map(_.imap[SourceProfile.Point](SourceProfile.Point(_), _.spectralDefinition))

      val uniformInput: Input[EditorInput[SourceProfile.Uniform]] =
        uniform.map(_.imap[SourceProfile.Uniform](SourceProfile.Uniform(_), _.spectralDefinition))

      EditorInput.editOneOf[SourceProfile, SourceProfile.Point, SourceProfile.Uniform, SourceProfile.Gaussian](
        ("point",    pointInput,   SourceProfile.point   ),
        ("uniform",  uniformInput, SourceProfile.uniform ),
        ("gaussian", gaussian,     SourceProfile.gaussian)
      )
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
