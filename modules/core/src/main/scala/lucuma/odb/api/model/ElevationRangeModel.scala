// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.numeric.Interval.Closed
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.math._
import lucuma.odb.api.model.syntax.validatedinput._
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

}

final case class ElevationRangeInput(
  airmassRange:   Input[AirmassRangeInput]   = Input.ignore,
  hourAngleRange: Input[HourAngleRangeInput] = Input.ignore
) extends EditorInput[ElevationRangeModel] {

  override val create: ValidatedInput[ElevationRangeModel] =
    ValidatedInput.requireOne(
      "elevationRange",
      airmassRange.map(_.create).toOption,
      hourAngleRange.map(_.create).toOption
    )

  override val edit: StateT[EitherInput, ElevationRangeModel, Unit] =
    EditorInput.editOneOf(
      ("airmassRange",   airmassRange,   ElevationRangeModel.airmassRange),
      ("hourAngleRange", hourAngleRange, ElevationRangeModel.hourAngleRange)
    )

}

/**
 * Input parameter used to create an elevation range. Both fields
 * are optional, but validation will check that exactly one is defined.
 */
object ElevationRangeInput {
  val Empty: ElevationRangeInput =
    ElevationRangeInput()

  def airmassRange(amr: AirmassRangeInput): ElevationRangeInput =
    Empty.copy(airmassRange = amr.assign)

  def hourAngleRange(har: HourAngleRangeInput): ElevationRangeInput =
    Empty.copy(hourAngleRange = har.assign)


  implicit val DecoderElevationRangeInput: Decoder[ElevationRangeInput] = {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    deriveConfiguredDecoder[ElevationRangeInput]
  }

  implicit val EqElevationRangeInput: Eq[ElevationRangeInput] =
    Eq.by(c => (c.airmassRange, c.hourAngleRange))

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
   *
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
}

trait AirmassRangeOptics {
  import AirmassRange.DecimalValue

  /** @group Optics */
  lazy val fromOrderedDecimalValues: Prism[(DecimalValue, DecimalValue), AirmassRange] =
    Prism[(DecimalValue, DecimalValue), AirmassRange] { case (min, max) =>
      Option.when(min.value <= max.value)(new AirmassRange(min, max) {})
    } { a => (a.min, a.max) }

  /** @group Optics */
  lazy val min: Getter[AirmassRange, DecimalValue] =
    Getter(_.min)

  /** @group Optics */
  lazy val max: Getter[AirmassRange, DecimalValue] =
    Getter(_.max)
}

final case class AirmassRangeInput(
  min: Option[BigDecimal],
  max: Option[BigDecimal]
) extends EditorInput[AirmassRange] {

  import AirmassRange._

  private def invalidRange(min: DecimalValue, max: DecimalValue): InputError =
    InputError.fromMessage(
      s"""Invalid AirmassRange: "min" value ${min.value} must be <= "max" value ${max.value}."""
    )

  override val create: ValidatedInput[AirmassRange] = {

    def checkRange(name: String, value: Option[BigDecimal]): ValidatedInput[Refined[BigDecimal, Closed[MinValue.type, MaxValue.type]]] =
      value
        .toValidNec(InputError.fromMessage(s""""$name" parameter of AirmassRange must be defined on creation"""))
        .andThen(v => ValidatedInput.closedInterval(name, v, MinValue, MaxValue))

    (checkRange("min", min),
     checkRange("max", max)
    ).tupled.andThen { case (min, max) =>
      AirmassRange(min, max).toValidNec(invalidRange(min, max))
    }
  }

  override val edit: StateT[EitherInput, AirmassRange, Unit] = {
    val validArgs = (
      min.traverse(n => ValidatedInput.closedInterval("min", n, MinValue, MaxValue)),
      max.traverse(x => ValidatedInput.closedInterval("max", x, MinValue, MaxValue))
    ).tupled

    for {
      args <- validArgs.liftState[AirmassRange]
      (n0, x0) = args
      n1 <- StateT.inspect[EitherInput, AirmassRange, DecimalValue](r => n0.getOrElse(r.min))
      x1 <- StateT.inspect[EitherInput, AirmassRange, DecimalValue](r => x0.getOrElse(r.max))
      _  <- StateT.setF(AirmassRange(n1, x1).toRightNec(invalidRange(n1, x1)))
    } yield ()
  }

}

object AirmassRangeInput {

  implicit val DecoderAirmassRangeInput: Decoder[AirmassRangeInput] =
    deriveDecoder[AirmassRangeInput]

  implicit val EqAirmassRangeInput: Eq[AirmassRangeInput] =
    Eq.by { a => (
      a.min,
      a.max
    )}

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
}

trait HourAngleRangeOptics {
  import HourAngleRange.DecimalHour

  lazy val fromOrderedDecimalHours: Prism[(DecimalHour, DecimalHour), HourAngleRange] =
    Prism[(DecimalHour, DecimalHour), HourAngleRange] { case (minHours, maxHours) =>
      Option.when(minHours.value <= maxHours.value)(new HourAngleRange(minHours, maxHours) {})
    } { a => (a.minHours, a.maxHours) }

  /** @group Optics */
  lazy val minHours: Getter[HourAngleRange, DecimalHour] =
    Getter(_.minHours)

  /** @group Optics */
  lazy val maxHours: Getter[HourAngleRange, DecimalHour] =
    Getter(_.maxHours)
}

final case class HourAngleRangeInput(
  minHours: Option[BigDecimal],
  maxHours: Option[BigDecimal]
) extends EditorInput[HourAngleRange] {

  import HourAngleRange._

  private def invalidRange(min: DecimalHour, max: DecimalHour): InputError =
    InputError.fromMessage(
      s"""Invalid HourAngleRange: "minHours" value ${min.value} must be <= "maxHours" value ${max.value}."""
    )

  override val create: ValidatedInput[HourAngleRange] = {
    def checkRange(name: String, value: Option[BigDecimal]): ValidatedInput[Refined[BigDecimal, Closed[MinHour.type, MaxHour.type]]] =
      value
        .toValidNec(InputError.fromMessage(s""""$name" parameter of HourAngleRange must be defined on creation"""))
        .andThen(v => ValidatedInput.closedInterval(name, v, MinHour, MaxHour))

    (checkRange("min", minHours),
     checkRange("max", maxHours)
    ).tupled.andThen { case (min, max) =>
      HourAngleRange(min, max).toValidNec(invalidRange(min,  max))
    }

  }

  override val edit: StateT[EitherInput, HourAngleRange, Unit] = {
    val validArgs = (
      minHours.traverse(n => ValidatedInput.closedInterval("minHours", n, MinHour, MaxHour)),
      maxHours.traverse(x => ValidatedInput.closedInterval("maxHours", x, MinHour, MaxHour))
    ).tupled

    for {
      args <- validArgs.liftState[HourAngleRange]
      (n0, x0) = args
      n1 <- StateT.inspect[EitherInput, HourAngleRange, DecimalHour](r => n0.getOrElse(r.minHours))
      x1 <- StateT.inspect[EitherInput, HourAngleRange, DecimalHour](r => x0.getOrElse(r.maxHours))
      _  <- StateT.setF(HourAngleRange(n1, x1).toRightNec(invalidRange(n1, x1)))
    } yield ()
  }

}

object HourAngleRangeInput {

  implicit val DecoderHourAngleRangeInput: Decoder[HourAngleRangeInput] =
    deriveDecoder[HourAngleRangeInput]

  implicit val EqHourAngleRangeInput: Eq[HourAngleRangeInput] =
    Eq.by { a => (
      a.minHours,
      a.maxHours
    )}

}
