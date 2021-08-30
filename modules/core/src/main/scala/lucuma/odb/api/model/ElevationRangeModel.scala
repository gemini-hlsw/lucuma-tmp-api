// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.math._
import monocle.{Getter, Prism}
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
 * min must be <= max
 */
sealed abstract case class AirmassRange protected (
  min: AirmassRange.DecimalValue,
  max: AirmassRange.DecimalValue
) extends ElevationRangeModel

object AirmassRange extends AirmassRangeOptics {
  val MinValue = BigDecimal(1.0)
  val MaxValue = BigDecimal(3.0)
  type Value        = Interval.Closed[MinValue.type, MaxValue.type]
  type DecimalValue = BigDecimal Refined Value

  val Any: AirmassRange =
    unsafeFromBigDecimal(MinValue, MaxValue)

  val Default: AirmassRange =
    unsafeFromBigDecimal(MinValue, BigDecimal(2.0))

  /**
   * Construct a new AirmassRange.
   * min must be <= max.
   * @group Optics
   */
  def apply(min: DecimalValue, max: DecimalValue): Option[AirmassRange] =
    fromOrderedDecimalValues.getOption((min, max))

  def unsafeFromBigDecimal(min: BigDecimal, max: BigDecimal): AirmassRange =
    if ((min < MinValue) || (max > MaxValue) || (min > max))
      throw new IllegalArgumentException(s"invalid airmass range ($min, $max)")
    else
      new AirmassRange(
        Refined.unsafeApply[BigDecimal, Value](MinValue),
        Refined.unsafeApply[BigDecimal, Value](MaxValue)
      ) {}

  implicit val EqAirmassRange: Eq[AirmassRange] = Eq.fromUniversalEquals

  /**
   * Input parameter used to create an airmass range. Parameter ranges
   * are validated and min must be <= max
   */
  final case class Create(min: BigDecimal, max: BigDecimal) {
    def create: ValidatedInput[AirmassRange] =
      (ValidatedInput.closedInterval("min", min, MinValue, MaxValue),
       ValidatedInput.closedInterval("max", max, MinValue, MaxValue)
      ).tupled.andThen { case (min, max) =>
        apply(min, max).toValidNec(
          InputError.fromMessage(s"Invalid AirmassRange: 'min' must be <= 'max'")
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
  import AirmassRange.DecimalValue

  /** @group Optics */
  lazy val fromOrderedDecimalValues: Prism[(DecimalValue, DecimalValue), AirmassRange] =
    Prism[(DecimalValue, DecimalValue), AirmassRange] { case (min, max) =>
      if (min.value <= max.value) (new AirmassRange(min, max) {}).some
      else none
    } { a =>
      (a.min, a.max)
    }

  /** @group Optics */
  lazy val min: Getter[AirmassRange, DecimalValue] =
    Getter(_.min)

  /** @group Optics */
  lazy val max: Getter[AirmassRange, DecimalValue] =
    Getter(_.max)
}

/**
 * Elevation range defined as an hour angle range.
 * minHours must be <= maxHours
 */
sealed abstract case class HourAngleRange protected (
  minHours: HourAngleRange.DecimalHour,
  maxHours: HourAngleRange.DecimalHour
) extends ElevationRangeModel {
  def minHourAngle: HourAngle = HourAngle.fromDoubleHours(minHours.value.toDouble)
  def maxHourAngle: HourAngle = HourAngle.fromDoubleDegrees(maxHours.value.toDouble)

  def minAngle: Angle = HourAngle.angle.get(minHourAngle)
  def maxAngle: Angle = HourAngle.angle.get(maxHourAngle)

  // Declination is used by Contraint.ElevationConstraint
  def minDeclination: Declination = Declination.fromAngleWithCarry(minAngle)._1
  def maxDeclination: Declination = Declination.fromAngleWithCarry(maxAngle)._1
}

object HourAngleRange extends HourAngleRangeOptics {
  val MinHour = BigDecimal(-5.0)
  val MaxHour = BigDecimal(5.0)
  type DeciHour    = Interval.Closed[MinHour.type, MaxHour.type]
  type DecimalHour = BigDecimal Refined DeciHour

  /**
   * Construct a new HourAngleRange.
   * minHours must be <= maxHours.
   * @group Optics
   */
  def apply(minHours: DecimalHour, maxHours: DecimalHour): Option[HourAngleRange] =
    fromOrderedDecimalHours.getOption((minHours, maxHours))

  implicit val EqHourAngleRange: Eq[HourAngleRange] = Eq.fromUniversalEquals

  /**
   * Input parameter used to create an hour angle range. Parameter ranges
   * are validated and minHours must be <= maxHours
   */
  final case class Create(minHours: BigDecimal, maxHours: BigDecimal) {
    def create: ValidatedInput[HourAngleRange] =
      (ValidatedInput.closedInterval("minHours", minHours, MinHour, MaxHour),
       ValidatedInput.closedInterval("maxHours", maxHours, MinHour, MaxHour)
      ).tupled.andThen { case (min, max) =>
        apply(min, max).toValidNec(
          InputError.fromMessage(s"Invalid HourAngleRange: 'minHours' must be <= 'maxHours'")
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
  import HourAngleRange.DecimalHour

  lazy val fromOrderedDecimalHours: Prism[(DecimalHour, DecimalHour), HourAngleRange] =
    Prism[(DecimalHour, DecimalHour), HourAngleRange] { case (minHours, maxHours) =>
      if (minHours.value <= maxHours.value) (new HourAngleRange(minHours, maxHours) {}).some
      else none
    } { a =>
      (a.minHours, a.maxHours)
    }

  /** @group Optics */
  lazy val minHours: Getter[HourAngleRange, DecimalHour] =
    Getter(_.minHours)

  /** @group Optics */
  lazy val maxHours: Getter[HourAngleRange, DecimalHour] =
    Getter(_.maxHours)
}
