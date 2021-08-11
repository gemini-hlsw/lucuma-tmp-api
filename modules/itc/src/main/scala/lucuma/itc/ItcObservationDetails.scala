// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import lucuma.core.math.Angle
import lucuma.odb.api.model.syntax.finiteduration._
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import scala.concurrent.duration.FiniteDuration

final case class ItcObservationDetails(
  calculationMethod: ItcObservationDetails.CalculationMethod,
  analysisMethod:    ItcObservationDetails.AnalysisMethod
)

object ItcObservationDetails {

  sealed trait CalculationMethod
    extends Product with Serializable

  object CalculationMethod {

    sealed trait SignalToNoise extends CalculationMethod
    object SignalToNoise {

      final case class Imaging(
        exposures:        Int,
        coadds:           Option[Int],
        exposureDuration: FiniteDuration,
        sourceFraction:   Double,
        ditherOffset:     Angle
      ) extends SignalToNoise

      object Imaging {
        val encoder: Encoder[Imaging] =
          Encoder.instance { a =>
            Json.obj(
              "exposures"      -> a.exposures.asJson,
              "coadds"         -> a.coadds.asJson,
              "exposureTime"   -> a.exposureDuration.toDoubleSeconds.asJson,
              "sourceFraction" -> a.sourceFraction.asJson,
              "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson,
            )
          }
      }

      final case class Spectroscopy(
        exposures:        Int,
        coadds:           Option[Int],
        exposureDuration: FiniteDuration,
        sourceFraction:   Double,
        ditherOffset:     Angle
      ) extends SignalToNoise

      object Spectroscopy {
        val encoder: Encoder[Spectroscopy] =
          Encoder.instance { a =>
            Json.obj(
              "exposures"      -> a.exposures.asJson,
              "coadds"         -> a.coadds.asJson,
              "exposureTime"   -> a.exposureDuration.toDoubleSeconds.asJson,
              "sourceFraction" -> a.sourceFraction.asJson,
              "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson,
            )
          }
      }

      implicit val encoder: Encoder[SignalToNoise] =
        new Encoder[SignalToNoise] {
          def apply(a: SignalToNoise): Json =
            a match {
              case a: Spectroscopy => Json.obj("SpectroscopyS2N" -> Spectroscopy.encoder(a))
              case a: Imaging      => Json.obj("ImagingS2N" -> Imaging.encoder(a))
            }
        }

    }

    sealed trait IntegrationTime extends CalculationMethod
    object IntegrationTime {

      final case class Imaging(
        sigma:            Double,
        exposureDuration: FiniteDuration,
        coadds:           Option[Int],
        sourceFraction:   Double,
        ditherOffset:     Angle
      ) extends IntegrationTime

      object Imaging {
        val encoder: Encoder[Imaging] =
          Encoder.instance { a =>
            Json.obj(
              "sigma"          -> a.sigma.asJson,
              "exposureTime"   -> a.exposureDuration.toDoubleSeconds.asJson,
              "coadds"         -> a.coadds.asJson,
              "sourceFraction" -> a.sourceFraction.asJson,
              "offset"         -> Angle.signedDecimalArcseconds.get(a.ditherOffset).asJson,
            )
          }
      }

      // We expect a spectroscopy option at some point
      val encoder: Encoder[IntegrationTime] =
        new Encoder[IntegrationTime] {
          def apply(a: IntegrationTime): Json =
            a match {
              case a: Imaging => Json.obj("ImagingInt" -> Imaging.encoder(a))
              // case a: Spectroscpy => Json.Null
            }
        }

    }

    implicit val encoder: Encoder[CalculationMethod] =
      new Encoder[CalculationMethod] {
        def apply(a: CalculationMethod): Json =
          a match {
            case a: SignalToNoise   => Json.obj("S2NMethod" -> SignalToNoise.encoder(a))
            case a: IntegrationTime => Json.obj("IntMethod" -> IntegrationTime.encoder(a))
          }
      }

  }

  sealed trait AnalysisMethod
    extends Product with Serializable

  object AnalysisMethod {

    sealed trait Aperture extends AnalysisMethod
    object Aperture {

      final case class Auto(
        skyAperture: Double
      ) extends Aperture

      object Auto {
        val encoder: Encoder[Auto] = deriveEncoder
      }

      final case class User(
        diameter: Double,
        skyAperture: Double
      ) extends Aperture

      object User {
        val encoder: Encoder[User] = deriveEncoder
      }

      val encoder: Encoder[Aperture] =
        new Encoder[Aperture] {
          def apply(a: Aperture): Json =
            a match {
              case a: Auto => Json.obj("AutoAperture" -> Auto.encoder(a))
              case a: User => Json.obj("UserAperture" -> User.encoder(a))
            }
        }

    }

    sealed trait Ifu extends AnalysisMethod
    object Ifu {

      final case class Single(
        skyFibres: Int,
        offset:    Double
      ) extends Ifu

      object Single {
        val encoder: Encoder[Single] = deriveEncoder
      }

      final case class Radial(
        skyFibres: Int,
        minOffset: Double,
        maxOffset: Double
      ) extends Ifu

      object Radial {
        val encoder: Encoder[Radial] = deriveEncoder
      }

      final case class Summed(
        skyFibres: Int,
        numX:      Int,
        numY:      Int,
        centerX:   Double,
        centerY:   Double
      ) extends Ifu

      object Summed {
        val encoder: Encoder[Summed] = deriveEncoder
      }

      final case class Sum(
        skyFibres: Int,
        num:       Double,
        isIfu2:    Boolean
      ) extends Ifu

      object Sum {
        val encoder: Encoder[Sum] = deriveEncoder
      }

      val encoder: Encoder[Ifu] =
        new Encoder[Ifu] {
          def apply(a: Ifu): Json =
            a match {
              case a: Single => Json.obj("IfuSingle" -> Single.encoder(a))
              case a: Radial => Json.obj("IfuRadial" -> Radial.encoder(a))
              case a: Summed => Json.obj("IfuSummed" -> Summed.encoder(a))
              case a: Sum    => Json.obj("IfuSum"    -> Sum.encoder(a))
            }
        }

    }

    implicit val encoder: Encoder[AnalysisMethod] =
      new Encoder[AnalysisMethod] {
        def apply(a: AnalysisMethod): Json =
          a match {
            case a: Aperture => Json.obj("ApertureMethod" -> Aperture.encoder(a))
            case a: Ifu      => Json.obj("IfuMethod"      -> Ifu.encoder(a))
          }
      }

  }

  implicit val encoder: Encoder[ItcObservationDetails] =
    deriveEncoder

}
