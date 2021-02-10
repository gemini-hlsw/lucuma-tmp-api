// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.math._
import monocle.{ Getter, Prism }
import monocle.macros.GenPrism

/**
 * Elevation range constraint. Can either be an AirmassRange or HourAngleRange.
 */
sealed trait ElevationRangeModel extends Product with Serializable

object ElevationRangeModel {
  val airmassRange: Prism[ElevationRangeModel, AirmassRange] =
    GenPrism[ElevationRangeModel, AirmassRange]

  val hourAngleRange: Prism[ElevationRangeModel, HourAngleRange] =
    GenPrism[ElevationRangeModel, HourAngleRange]

  implicit val EqElevationRange: Eq[ElevationRangeModel] = Eq.fromUniversalEquals

  final case class Create(
    airmassRange:   Option[AirmassRange.Create],
    hourAngleRange: Option[HourAngleRange.Create]
  ) {
    def create: ValidatedInput[ElevationRangeModel] =
      ValidatedInput.requireOne("elevationRange",
                                airmassRange.map(_.create),
                                hourAngleRange.map(_.create)
      )
  }

  /**
   * Input parameter used to create an elevation range. Both fields
   * are optional, but validation will check that exactly one is defined.
   */
  object Create {
    val Empty: Create = Create(airmassRange = None, hourAngleRange = None)

    def airmassRange(amr: AirmassRange.Create): Create =
      Empty.copy(airmassRange = amr.some)

    def hourAngleRange(har: HourAngleRange.Create): Create =
      Empty.copy(hourAngleRange = har.some)

    implicit val DecoderCreate: Decoder[Create] = deriveDecoder

    implicit val EqCreate: Eq[Create] =
      Eq.by(c => (c.airmassRange, c.hourAngleRange))

    implicit val InputValidatorCreate: InputValidator[Create, ElevationRangeModel] =
      InputValidator.by(_.create)
  }
}

/**
 * Elevation range defined as an airmass range.
 * deciMin must be <= deciMax
 */
sealed abstract case class AirmassRange protected (
  deciMin: AirmassRange.IntDeciValue,
  deciMax: AirmassRange.IntDeciValue
) extends ElevationRangeModel {
  def min: Double = deciMin.value / 10.0
  def max: Double = deciMax.value / 10.0
}

object AirmassRange extends AirmassRangeOptics {
  type DeciValue    = Interval.Closed[10, 30]
  type IntDeciValue = Int Refined DeciValue

  /**
   * Construct a new AirmassRange.
   * deciMin must be <= deciMax.
   * @group Optics
   */
  def apply(deciMin: IntDeciValue, deciMax: IntDeciValue): Option[AirmassRange] =
    fromOrderedDeciVals.getOption((deciMin, deciMax))

  implicit val EqAirmassRange: Eq[AirmassRange] = Eq.fromUniversalEquals

  /**
   * Input parameter used to create an airmass range. Parameter ranges
   * are validated and deciMin must be <= deciMax
   */
  final case class Create(deciMin: Int, deciMax: Int) {
    def create: ValidatedInput[AirmassRange] =
      (ValidatedInput.closedInterval("deciMin", deciMin, 10, 30),
       ValidatedInput.closedInterval("deciMax", deciMax, 10, 30)
      ).tupled.andThen { case (min, max) =>
        apply(min, max).toValidNec(
          InputError.fromMessage(s"Invalid AirmassRange: 'deciMin' must be <= 'deciMax'")
        )
      }
  }

  object Create {
    implicit val DecoderCreateAirmassRange: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreateAirmassRange: Eq[Create] =
      Eq.fromUniversalEquals

    implicit val ValidatorCreateAirmassRange: InputValidator[Create, AirmassRange] =
      InputValidator.by(_.create)
  }
}

trait AirmassRangeOptics {
  import AirmassRange.IntDeciValue

  /** @group Optics */
  lazy val fromOrderedDeciVals: Prism[(IntDeciValue, IntDeciValue), AirmassRange] =
    Prism[(IntDeciValue, IntDeciValue), AirmassRange] { case (min, max) =>
      if (min.value <= max.value) (new AirmassRange(min, max) {}).some
      else none
    } { a =>
      (a.deciMin, a.deciMax)
    }

  /** @group Optics */
  lazy val deciMin: Getter[AirmassRange, IntDeciValue] =
    Getter(_.deciMin)

  /** @group Optics */
  lazy val deciMax: Getter[AirmassRange, IntDeciValue] =
    Getter(_.deciMax)
}

/**
 * Elevation range defined as an hour angle range.
 * deciMin must be <= deciMax
 */
sealed abstract case class HourAngleRange protected (
  deciMin: HourAngleRange.IntDeciHour,
  deciMax: HourAngleRange.IntDeciHour
) extends ElevationRangeModel {
  def minDoubleHours: Double = deciMin.value / 10.0
  def maxDoubleHours: Double = deciMax.value / 10.0

  def minHourAngle: HourAngle = HourAngle.fromDoubleHours(minDoubleHours)
  def maxHourAngle: HourAngle = HourAngle.fromDoubleDegrees(maxDoubleHours)

  def minAngle: Angle = HourAngle.angle.get(minHourAngle)
  def maxAngle: Angle = HourAngle.angle.get(maxHourAngle)

  // Declination is used by Contraint.ElevationConstraint
  def minDeclination: Declination = Declination.fromAngleWithCarry(minAngle)._1
  def maxDeclination: Declination = Declination.fromAngleWithCarry(maxAngle)._1
}

object HourAngleRange extends HourAngleRangeOptics {
  type DeciHour    = Interval.Closed[-50, 50]
  type IntDeciHour = Int Refined DeciHour

  /**
   * Construct a new HourAngleRange.
   * deciMin must be <= deciMax.
   * @group Optics
   */
  def apply(deciMin: IntDeciHour, deciMax: IntDeciHour): Option[HourAngleRange] =
    fromOrderedDeciHours.getOption((deciMin, deciMax))

  implicit val EqHourAngleRange: Eq[HourAngleRange] = Eq.fromUniversalEquals

  /**
   * Input parameter used to create an hour angle range. Parameter ranges
   * are validated and deciMin must be <= deciMax
   */
  final case class Create(deciMin: Int, deciMax: Int) {
    def create: ValidatedInput[HourAngleRange] =
      (ValidatedInput.closedInterval("deciMin", deciMin, -50, 50),
       ValidatedInput.closedInterval("deciMax", deciMax, -50, 50)
      ).tupled.andThen { case (min, max) =>
        apply(min, max).toValidNec(
          InputError.fromMessage(s"Invalid HourAngleRange: 'deciMin' must be <= 'deciMax'")
        )
      }
  }

  object Create {
    implicit val DecoderCreateHourAngleRange: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreateHourAngleRange: Eq[Create] =
      Eq.fromUniversalEquals

    implicit val ValidatorCreateHourAngleRange: InputValidator[Create, HourAngleRange] =
      InputValidator.by(_.create)
  }
}

trait HourAngleRangeOptics {
  import HourAngleRange.IntDeciHour

  lazy val fromOrderedDeciHours: Prism[(IntDeciHour, IntDeciHour), HourAngleRange] =
    Prism[(IntDeciHour, IntDeciHour), HourAngleRange] { case (min, max) =>
      if (min.value <= max.value) (new HourAngleRange(min, max) {}).some
      else none
    } { a =>
      (a.deciMin, a.deciMax)
    }

  /** @group Optics */
  lazy val deciMin: Getter[HourAngleRange, IntDeciHour] =
    Getter(_.deciMin)

  /** @group Optics */
  lazy val deciMax: Getter[HourAngleRange, IntDeciHour] =
    Getter(_.deciMax)
}
