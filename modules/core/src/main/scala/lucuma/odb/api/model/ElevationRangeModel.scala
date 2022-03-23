// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval.Closed
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.ElevationRange
import lucuma.core.model.ElevationRange.HourAngle.DecimalHour
import lucuma.odb.api.model.syntax.validatedinput._

final case class ElevationRangeInput(
  airMass:   Input[AirMassRangeInput]   = Input.ignore,
  hourAngle: Input[HourAngleRangeInput] = Input.ignore
) extends EditorInput[ElevationRange] {

  override val create: ValidatedInput[ElevationRange] =
    ValidatedInput.requireOne(
      "elevationRange",
      airMass.map(_.create).toOption,
      hourAngle.map(_.create).toOption
    )

  override val edit: StateT[EitherInput, ElevationRange, Unit] =
    EditorInput.editOneOf(
      ("airMass",   airMass,   ElevationRange.airMass),
      ("hourAngle", hourAngle, ElevationRange.hourAngle)
    )

}

/**
 * Input parameter used to create an elevation range. Both fields
 * are optional, but validation will check that exactly one is defined.
 */
object ElevationRangeInput {
  val Empty: ElevationRangeInput =
    ElevationRangeInput()

  def airMass(amr: AirMassRangeInput): ElevationRangeInput =
    Empty.copy(airMass = amr.assign)

  def hourAngle(har: HourAngleRangeInput): ElevationRangeInput =
    Empty.copy(hourAngle = har.assign)


  implicit val DecoderElevationRangeInput: Decoder[ElevationRangeInput] = {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    deriveConfiguredDecoder[ElevationRangeInput]
  }

  implicit val EqElevationRangeInput: Eq[ElevationRangeInput] =
    Eq.by(c => (c.airMass, c.hourAngle))

}

final case class AirMassRangeInput(
  min: Option[BigDecimal],
  max: Option[BigDecimal]
) extends EditorInput[ElevationRange.AirMass] {

  import ElevationRange.AirMass
  import AirMass.{DecimalValue, MaxValue, MinValue}

  private def invalidRange(min: DecimalValue, max: DecimalValue): InputError =
    InputError.fromMessage(
      s"""Invalid AirMass: "min" value ${min.value} must be <= "max" value ${max.value}."""
    )

  override val create: ValidatedInput[AirMass] = {

    def checkRange(name: String, value: Option[BigDecimal]): ValidatedInput[Refined[BigDecimal, Closed[MinValue.type, MaxValue.type]]] =
      value
        .toValidNec(InputError.fromMessage(s""""$name" parameter of AirMass must be defined on creation"""))
        .andThen(v => ValidatedInput.closedInterval(name, v, MinValue, MaxValue))

    (checkRange("min", min),
     checkRange("max", max)
    ).tupled.andThen { case (min, max) =>
      AirMass.fromOrderedDecimalValues.getOption(min, max).toValidNec(invalidRange(min, max))
    }
  }

  override val edit: StateT[EitherInput, AirMass, Unit] = {
    val validArgs = (
      min.traverse(n => ValidatedInput.closedInterval("min", n, MinValue, MaxValue)),
      max.traverse(x => ValidatedInput.closedInterval("max", x, MinValue, MaxValue))
    ).tupled

    for {
      args <- validArgs.liftState[AirMass]
      (n0, x0) = args
      n1 <- StateT.inspect[EitherInput, AirMass, DecimalValue](r => n0.getOrElse(r.min))
      x1 <- StateT.inspect[EitherInput, AirMass, DecimalValue](r => x0.getOrElse(r.max))
      _  <- StateT.setF(AirMass.fromOrderedDecimalValues.getOption(n1, x1).toRightNec(invalidRange(n1, x1)))
    } yield ()
  }

}

object AirMassRangeInput {

  implicit val DecoderAirMassRangeInput: Decoder[AirMassRangeInput] =
    deriveDecoder[AirMassRangeInput]

  implicit val EqAirMassRangeInput: Eq[AirMassRangeInput] =
    Eq.by { a => (
      a.min,
      a.max
    )}

}

final case class HourAngleRangeInput(
  minHours: Option[BigDecimal],
  maxHours: Option[BigDecimal]
) extends EditorInput[ElevationRange.HourAngle] {

  import ElevationRange.HourAngle
  import HourAngle.{MaxHour, MinHour}

  private def invalidRange(min: DecimalHour, max: DecimalHour): InputError =
    InputError.fromMessage(
      s"""Invalid HourAngleRange: "minHours" value ${min.value} must be <= "maxHours" value ${max.value}."""
    )

  override val create: ValidatedInput[HourAngle] = {
    def checkRange(name: String, value: Option[BigDecimal]): ValidatedInput[Refined[BigDecimal, Closed[MinHour.type, MaxHour.type]]] =
      value
        .toValidNec(InputError.fromMessage(s""""$name" parameter of HourAngleRange must be defined on creation"""))
        .andThen(v => ValidatedInput.closedInterval(name, v, MinHour, MaxHour))

    (checkRange("min", minHours),
     checkRange("max", maxHours)
    ).tupled.andThen { case (min, max) =>
      HourAngle.fromOrderedDecimalHours.getOption(min, max).toValidNec(invalidRange(min,  max))
    }

  }

  override val edit: StateT[EitherInput, HourAngle, Unit] = {
    val validArgs = (
      minHours.traverse(n => ValidatedInput.closedInterval("minHours", n, MinHour, MaxHour)),
      maxHours.traverse(x => ValidatedInput.closedInterval("maxHours", x, MinHour, MaxHour))
    ).tupled

    for {
      args <- validArgs.liftState[HourAngle]
      (n0, x0) = args
      n1 <- StateT.inspect[EitherInput, HourAngle, DecimalHour](r => n0.getOrElse(r.minHours))
      x1 <- StateT.inspect[EitherInput, HourAngle, DecimalHour](r => x0.getOrElse(r.maxHours))
      _  <- StateT.setF(HourAngle.fromOrderedDecimalHours.getOption(n1, x1).toRightNec(invalidRange(n1, x1)))
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
