// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.all._
import eu.timepit.refined.types.all.PosBigDecimal
import io.circe._
import io.circe.syntax._
import lucuma.core.`enum`.Band
import lucuma.core.math.BrightnessUnits.{BrightnessMeasure, Integrated, Surface}
import lucuma.core.math.{RadialVelocity, Wavelength}
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.{ConstraintSet, ElevationRange, SourceProfile, SpectralDefinition, Target, UnnormalizedSED}
import lucuma.core.syntax.enumerated._
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.{ObservationModel, ScienceConfigurationModel}

import scala.collection.immutable.SortedMap

final case class ItcSpectroscopyInput(
  wavelength:      Wavelength,
  signalToNoise:   PosBigDecimal,
  sourceProfile:   SourceProfile,
  band:            Band,
  radialVelocity:  RadialVelocity,
  constraints:     ConstraintSet,
  modes:           List[ScienceConfigurationModel]
)

object ItcSpectroscopyInput {

  def fromObservation(o: ObservationModel, t: Target): Option[ItcSpectroscopyInput] = {

    def extractBand[T](w: Wavelength, m: SortedMap[Band, BrightnessMeasure[T]]): Option[Band] =
      m.minByOption { case (b, _) =>
        (w.toPicometers.value.value - b.center.toPicometers.value.value).abs
      }.map(_._1)

    def band: Option[Band] =
      o.scienceRequirements.spectroscopy.wavelength.flatMap { w =>
        SourceProfile
          .integratedBrightnesses
          .getOption(t.sourceProfile)
          .flatMap(m => extractBand[Integrated](w, m))
          .orElse(
            SourceProfile
              .surfaceBrightnesses
              .getOption(t.sourceProfile)
              .flatMap(m => extractBand[Surface](w, m))
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
      b   <- band
      r   <- radialVelocity
      c   <- o.scienceConfiguration
    } yield
      ItcSpectroscopyInput(
        w,
        s2n,
        t.sourceProfile,
        b,
        r,
        o.constraintSet,
        List(c)
      )
  }

  implicit val EncoderPosBigDecimal: Encoder[PosBigDecimal] =
    (pbd: PosBigDecimal) => pbd.value.asJson

  implicit val EncoderWavelength: Encoder[Wavelength] =
    (a: Wavelength) =>
      Json.obj(
        "picometers" -> Wavelength.picometers.reverseGet(a).value.asJson
      )

  def screaming[A: Enumerated](a: A): Json =
    a.tag.toScreamingSnakeCase.asJson

  implicit val EncoderUnnormalizedSED: Encoder[UnnormalizedSED] = {
      case UnnormalizedSED.StellarLibrary(s)      =>
        Json.obj("stellarLibrary" -> screaming(s))

      case UnnormalizedSED.CoolStarModel(s)       =>
        Json.obj("coolStar" -> screaming(s))

      case UnnormalizedSED.Galaxy(s)              =>
        Json.obj("galaxy" -> screaming(s))

      case UnnormalizedSED.Planet(s)              =>
        Json.obj("planet" -> screaming(s))

      case UnnormalizedSED.Quasar(s)              =>
        Json.obj("quasar" -> screaming(s))

      case UnnormalizedSED.HIIRegion(s)           =>
        Json.obj("hiiRegion" -> screaming(s))

      case UnnormalizedSED.PlanetaryNebula(s)     =>
        Json.obj("planetaryNebula" -> screaming(s))

      case UnnormalizedSED.PowerLaw(index)        =>
        Json.obj("powerLaw" -> index.asJson)

      case UnnormalizedSED.BlackBody(temperature) =>
        Json.obj("blackBodyTempK" -> temperature.value.value.asJson)

      case UnnormalizedSED.UserDefined(fs)        =>
        Json.arr(fs.toNel.toList.map { case (w, d) =>
          Json.obj(
            "wavelength" -> w.asJson,
            "density"    -> d.asJson
          )
        }: _*)
    }

  implicit def EncoderBandNormalized[T]: Encoder[BandNormalized[T]] =
    (bn: BandNormalized[T]) =>
      Json.obj(
        "sed"          -> bn.sed.asJson,
        "brightnesses" -> Json.arr(bn.brightnesses.toList.map { case (b, m) =>
          Json.fromFields(
            List(
              "band"  -> screaming(b),
              "value" -> m.value.toBigDecimal.asJson,
              "units" -> m.units.serialized.asJson
            ) ++ m.error.toList.map(v => "error" -> v.toBigDecimal.asJson)
          )
        }: _*)
      )

  implicit def EncoderEmissionLines[T]: Encoder[EmissionLines[T]] =
    (el: EmissionLines[T]) =>
      Json.obj(
        "lines" -> Json.arr(el.lines.toList.map { case (w, l) =>
          Json.obj(
            "wavelength" -> w.asJson,
            "lineWidth"  -> l.lineWidth.value.asJson,
            "lineFlux"   ->
              Json.obj(
                "value" -> l.lineFlux.value.value.asJson,
                "units" -> l.lineFlux.units.serialized.asJson
              )
          )
        }: _*),
        "fluxDensityContinuum" ->
          Json.obj(
            "value" -> el.fluxDensityContinuum.value.value.asJson,
            "units" -> el.fluxDensityContinuum.units.serialized.asJson
          )
      )

  implicit def EncoderSpectralDefinition[T]: Encoder[SpectralDefinition[T]] = {
    case bn@SpectralDefinition.BandNormalized(_, _) =>
      Json.obj("bandNormalized" -> bn.asJson)

    case el@SpectralDefinition.EmissionLines(_, _)  =>
      Json.obj("emissionLines" -> el.asJson)
  }

  implicit val EncoderSourceProfile: Encoder[SourceProfile] = {
      case SourceProfile.Point(s)       =>
        Json.obj("point"    -> s.asJson)

      case SourceProfile.Uniform(s)     =>
        Json.obj("uniform"  -> s.asJson)

      case SourceProfile.Gaussian(f, s) =>
        Json.obj(
          "gaussian" -> Json.obj(
            "fwhm"               -> Json.obj("microarcseconds" -> f.toMicroarcseconds.asJson),
            "spectralDefinition" -> s.asJson
          )
        )
    }

  implicit val EncoderConstraintSetModel: Encoder[ConstraintSet] =
    (a: ConstraintSet) =>
      Json.obj(
        "imageQuality"    -> screaming(a.imageQuality),
        "cloudExtinction" -> screaming(a.cloudExtinction),
        "skyBackground"   -> screaming(a.skyBackground),
        "waterVapor"      -> screaming(a.waterVapor),
        "elevationRange"  -> (a.elevationRange match {
          case ElevationRange.AirMass(min, max)             =>
            Json.obj(
              // TODO: we should switch this to airMass to be consistent
              "airmassRange"->
                Json.obj(
                  "min" -> min.value.asJson,
                  "max" -> max.value.asJson
                )
            )
          case ElevationRange.HourAngle(minHours, maxHours) =>
            Json.obj(
              // TODO: we should switch this to hourAngle to be consistent
              "hourAngleRange" ->
                Json.obj(
                  "minHours" -> minHours.value.asJson,
                  "maxHours" -> maxHours.value.asJson
                )
            )
        })
      )

  implicit val EncoderScienceConfigurationModel: Encoder[ScienceConfigurationModel] = {
    case ScienceConfigurationModel.Modes.GmosNorthLongSlit(filter, disperser, fpu, _) =>
      Json.obj(
        "gmosN" ->
          Json.fromFields(
            List(
              "disperser"  -> screaming(disperser),
              "fpu"        -> screaming(fpu)
            ) ++ filter.map(screaming(_)).tupleLeft("filter").toList
          )
      )
    case ScienceConfigurationModel.Modes.GmosSouthLongSlit(filter, disperser, fpu, _) =>
      Json.obj(
        "gmosS" ->
          Json.fromFields(
            List(
              "disperser"  -> screaming(disperser),
              "fpu"        -> screaming(fpu)
            ) ++ filter.map(screaming(_)).tupleLeft("filter").toList
          )
      )
  }

  implicit val EncoderItcSpectroscopyInput: Encoder[ItcSpectroscopyInput] =
    (a: ItcSpectroscopyInput) =>
      Json.obj(
        "wavelength"           -> a.wavelength.asJson,
        "signalToNoise"        -> a.signalToNoise.asJson,
        "sourceProfile"        -> a.sourceProfile.asJson,
        "band"                 -> screaming(a.band),
        "radialVelocity"       -> Json.obj(
          "metersPerSecond" -> RadialVelocity.fromMetersPerSecond.reverseGet(a.radialVelocity).asJson
        ),
        "constraints"          -> a.constraints.asJson,
        "modes"                -> a.modes.map(_.asJson).asJson
      )

}