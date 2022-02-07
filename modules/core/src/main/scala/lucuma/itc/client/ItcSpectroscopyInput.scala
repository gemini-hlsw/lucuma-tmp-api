// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.all._
import eu.timepit.refined.types.all.PosBigDecimal
import io.circe._
import io.circe.syntax._
import lucuma.core.`enum`.{Band, CloudExtinction, GmosNorthFpu, GmosSouthFpu, ImageQuality, SkyBackground, WaterVapor}
import lucuma.core.math.BrightnessUnits.{ABMagnitudeIsIntegratedBrightnessUnit, ABMagnitudePerArcsec2IsSurfaceBrightnessUnit, BrightnessMeasure, ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceBrightnessUnit, ErgsPerSecondCentimeter2AngstromIsIntegratedBrightnessUnit, ErgsPerSecondCentimeter2HertzArcsec2IsSurfaceBrightnessUnit, ErgsPerSecondCentimeter2HertzIsIntegratedBrightnessUnit, Integrated, JanskyIsIntegratedBrightnessUnit, JanskyPerArcsec2IsSurfaceBrightnessUnit, Surface, VegaMagnitudeIsIntegratedBrightnessUnit, VegaMagnitudePerArcsec2IsSurfaceBrightnessUnit, WattsPerMeter2MicrometerArcsec2IsSurfaceBrightnessUnit, WattsPerMeter2MicrometerIsIntegratedBrightnessUnit}
import lucuma.core.math.{Angle, RadialVelocity, Wavelength}
import lucuma.core.model.{SourceProfile, Target, UnnormalizedSED}
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.{AirmassRange, ConstraintSetModel, HourAngleRange, ObservationModel, ScienceConfigurationModel}

import scala.collection.immutable.SortedMap

final case class ItcSpectroscopyInput(
  wavelength:           Wavelength,
  signalToNoise:        PosBigDecimal,
  sourceProfile:        SourceProfile,
  spectralDistribution: UnnormalizedSED,
  magnitude:            (Band, Either[BrightnessMeasure[Integrated], BrightnessMeasure[Surface]]),
  radialVelocity:       RadialVelocity,
  constraints:          ConstraintSetModel,
  modes:                List[ScienceConfigurationModel]
)

object ItcSpectroscopyInput {

  def fromObservation(o: ObservationModel, t: Target): Option[ItcSpectroscopyInput] = {

    val sed: Option[UnnormalizedSED] =
      SourceProfile.unnormalizedSED.getOption(t.sourceProfile).filter {
        case UnnormalizedSED.CoolStarModel(_)   => false
        case UnnormalizedSED.PlanetaryNebula(_) => false
        case UnnormalizedSED.UserDefined(_)     => false
        case _                                  => true
      }

    def extractMag[T](w: Wavelength, m: SortedMap[Band, BrightnessMeasure[T]]): Option[(Band, BrightnessMeasure[T])] =
      m.minByOption { case (b, _) =>
        (w.toPicometers.value.value - b.center.toPicometers.value.value).abs
      }

    def magnitude: Option[(Band, Either[BrightnessMeasure[Integrated], BrightnessMeasure[Surface]])] =
      o.scienceRequirements.spectroscopy.wavelength.flatMap { w =>
        SourceProfile
          .integratedBrightnesses
          .getOption(t.sourceProfile)
          .flatMap(m => extractMag[Integrated](w, m).map(_.map(_.asLeft[BrightnessMeasure[Surface]])))
          .orElse(
            SourceProfile
              .surfaceBrightnesses
              .getOption(t.sourceProfile)
              .flatMap(m => extractMag[Surface](w, m).map(_.map(_.asRight[BrightnessMeasure[Integrated]])))
          )
      }

    val radialVelocity: Option[RadialVelocity] =
      t match {
        case Target.Sidereal(_, track, _, _) => track.radialVelocity
        case Target.Nonsidereal(_, _, _)     => Option.empty
      }

    for {
      w   <- o.scienceRequirements.spectroscopy.wavelength
      s2n <- o.scienceRequirements.spectroscopy.signalToNoise
      s   <- sed
      m   <- magnitude
      r   <- radialVelocity
      c   <- o.scienceConfiguration
    } yield
      ItcSpectroscopyInput(
        w,
        s2n,
        t.sourceProfile,
        s,
        m,
        r,
        o.constraintSet,
        List(c)
      )
  }

  private def sourceType(a: SourceProfile): Json =
    a match {
      case SourceProfile.Point(_)       => "POINT_SOURCE".asJson
      case SourceProfile.Uniform(_)     => "UNIFORM_SOURCE".asJson
      case SourceProfile.Gaussian(_, _) => "GAUSSIAN_SOURCE".asJson
    }

  private def fwhm(a: SourceProfile): Option[Json] =
    a match {
      case SourceProfile.Gaussian(fwhm, _) =>
        Json.obj("microarcseconds" -> fwhm.toMicroarcseconds.asJson).some
      case _                               =>
        Option.empty
    }

  implicit val EncoderUnnormalizedSED: Encoder[UnnormalizedSED] = {
      case UnnormalizedSED.StellarLibrary(librarySpectrum) =>
        Json.obj("stellar" -> librarySpectrum.tag.toUpperCase.asJson)

      case UnnormalizedSED.CoolStarModel(_)                =>
        Json.obj()

      case UnnormalizedSED.Galaxy(galaxySpectrum)          =>
        Json.obj("nonStellar" -> s"${galaxySpectrum.tag.toUpperCase}_GALAXY".asJson)

      case UnnormalizedSED.Planet(planetSpectrum)          =>
        Json.obj("nonStellar" -> planetSpectrum.tag.toUpperCase.asJson)

      case UnnormalizedSED.Quasar(quasarSpectrum)          =>
        Json.obj("nonStellar" -> quasarSpectrum.tag.toUpperCase.asJson)

      case UnnormalizedSED.HIIRegion(_)                    =>
        Json.obj("nonStellar" -> "ORION_NEBULA".asJson)

      case UnnormalizedSED.PlanetaryNebula(_)              =>
        Json.obj()

      case UnnormalizedSED.PowerLaw(index)                 =>
        Json.obj("powerLaw" -> Json.obj("index" -> index.asJson))

      case UnnormalizedSED.BlackBody(temperature)          =>
        Json.obj("blackBody" -> Json.obj("temperature" -> temperature.value.value.asJson))

      case UnnormalizedSED.UserDefined(_)                  =>
        Json.obj()

    }

  val BrightnessIntegratedToItc: Map[String, String] =
    Map(
      VegaMagnitudeIsIntegratedBrightnessUnit.unit.serialized                    -> "VEGA",
      ABMagnitudeIsIntegratedBrightnessUnit.unit.serialized                      -> "AB",
      JanskyIsIntegratedBrightnessUnit.unit.serialized                           -> "JY",
      WattsPerMeter2MicrometerIsIntegratedBrightnessUnit.unit.serialized         -> "WATTS",
      ErgsPerSecondCentimeter2AngstromIsIntegratedBrightnessUnit.unit.serialized -> "ERGS_WAVELENGTH",
      ErgsPerSecondCentimeter2HertzIsIntegratedBrightnessUnit.unit.serialized    -> "ERGS_FREQUENCY"
    )

  val BrightnessSurfaceToItc: Map[String, String] =
    Map(
      VegaMagnitudePerArcsec2IsSurfaceBrightnessUnit.unit.serialized                 -> "VEGA",
      ABMagnitudePerArcsec2IsSurfaceBrightnessUnit.unit.serialized                   -> "AB",
      JanskyPerArcsec2IsSurfaceBrightnessUnit.unit.serialized                        -> "JY",
      WattsPerMeter2MicrometerArcsec2IsSurfaceBrightnessUnit.unit.serialized         -> "WATTS",
      ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceBrightnessUnit.unit.serialized -> "ERGS_WAVELENGTH",
      ErgsPerSecondCentimeter2HertzArcsec2IsSurfaceBrightnessUnit.unit.serialized    -> "ERGS_FREQUENCY"
    )

  def encodeBrightness(b: Band, e: Either[BrightnessMeasure[Integrated], BrightnessMeasure[Surface]]): Json = {
    val error: Option[BigDecimal] =
      e.map(_.error.map(_.toBigDecimal)).swap.map(_.error.map(_.toBigDecimal)).merge

    // Couldn't figure out the right way to do this
    val system: String =
      e.map(m => BrightnessSurfaceToItc(m.units.serialized))
       .swap
       .map(m => BrightnessIntegratedToItc(m.units.serialized))
       .merge

    Json.fromFields(
      List(
        "band"   -> b.tag.toScreamingSnakeCase.asJson,
        "value"  -> e.map(_.value.toBigDecimal).swap.map(_.value.toBigDecimal).merge.asJson,
        "system" -> system.asJson
      ) ++ error.map(_.asJson).tupleLeft("error").toList
    )
  }

  implicit val EncoderSourceProfile: Encoder[SourceProfile] =
    (a: SourceProfile) =>
      Json.fromFields(
        "sourceType" -> sourceType(a) :: fwhm(a).tupleLeft("fwhm").toList
      )

  implicit val EncoderConstraintSetModel: Encoder[ConstraintSetModel] =
    (a: ConstraintSetModel) =>
      Json.obj(
        "imageQuality"    -> Enumerated[ImageQuality].tag(a.imageQuality).toScreamingSnakeCase.asJson,
        "cloudExtinction" -> Enumerated[CloudExtinction].tag(a.cloudExtinction).toScreamingSnakeCase.asJson,
        "skyBackground"   -> Enumerated[SkyBackground].tag(a.skyBackground).toScreamingSnakeCase.asJson,
        "waterVapor"      -> Enumerated[WaterVapor].tag(a.waterVapor).toScreamingSnakeCase.asJson,
        "elevationRange"  -> (a.elevationRange match {
          case AirmassRange(min, max)             =>
            Json.obj(
              "airmassRange"->
                Json.obj(
                  "min" -> min.value.asJson,
                  "max" -> max.value.asJson
                )
            )
          case HourAngleRange(minHours, maxHours) =>
            Json.obj(
              "hourAngleRange" ->
                Json.obj(
                  "minHours" -> minHours.value.asJson,
                  "maxHours" -> maxHours.value.asJson
                )
            )
        })
      )

  private def gnSlitWidthToJson(slitWidth: Angle): Json =
    GmosNorthFpu
      .all
      .filter(_.tag.startsWith("LongSlit"))
      .minBy(fpu => (fpu.slitWidth.get.toMicroarcseconds - slitWidth.toMicroarcseconds).abs)
      .tag
      .toScreamingSnakeCase
      .asJson

  private def gsSlitWidthToJson(slitWidth: Angle): Json =
    GmosSouthFpu
      .all
      .filter(_.tag.startsWith("LongSlit"))
      .minBy(fpu => (fpu.slitWidth.get.toMicroarcseconds - slitWidth.toMicroarcseconds).abs)
      .tag
      .toScreamingSnakeCase
      .asJson

  implicit val EncoderScienceConfigurationModel: Encoder[ScienceConfigurationModel] = {
    case ScienceConfigurationModel.Modes.GmosNorthLongSlit(filter, disperser, slitWidth) =>
      Json.obj(
        "gmosN" ->
          Json.fromFields(
            List(
              "disperser"  -> disperser.tag.toScreamingSnakeCase.asJson,
              "fpu"        -> gnSlitWidthToJson(slitWidth)
            ) ++ filter.map(_.tag.toScreamingSnakeCase.asJson).tupleLeft("filter").toList
          )
      )
    case ScienceConfigurationModel.Modes.GmosSouthLongSlit(filter, disperser, slitWidth) =>
      Json.obj(
        "gmosS" ->
          Json.fromFields(
            List(
              "disperser"  -> disperser.tag.toScreamingSnakeCase.asJson,
              "fpu"        -> gsSlitWidthToJson(slitWidth)
            ) ++ filter.map(_.tag.toScreamingSnakeCase.asJson).tupleLeft("filter").toList
          )
      )
  }

  implicit val EncoderItcSpectroscopyInput: Encoder[ItcSpectroscopyInput] =
    (a: ItcSpectroscopyInput) =>
      Json.obj(
        "wavelength"           -> Json.obj(
          "picometers" -> Wavelength.picometers.reverseGet(a.wavelength).value.asJson
        ),
        "signalToNoise"        -> Encoder[BigDecimal].apply(a.signalToNoise.value),
        "spatialProfile"       -> Encoder[SourceProfile].apply(a.sourceProfile),
        "spectralDistribution" -> Encoder[UnnormalizedSED].apply(a.spectralDistribution),
        "magnitude"            -> (encodeBrightness _).tupled(a.magnitude),
        "radialVelocity"       -> Json.obj(
          "metersPerSecond" -> RadialVelocity.fromMetersPerSecond.reverseGet(a.radialVelocity).asJson
        ),
        "constraints"          -> Encoder[ConstraintSetModel].apply(a.constraints),
        "modes"                -> a.modes.map(Encoder[ScienceConfigurationModel].apply).asJson
      )

}