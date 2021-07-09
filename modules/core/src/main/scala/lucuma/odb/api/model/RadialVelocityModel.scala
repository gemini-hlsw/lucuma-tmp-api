// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.core.math.RadialVelocity
import lucuma.core.util.{Display, Enumerated}
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.{Iso, Prism}

object RadialVelocityModel {

  private def scalingBy(powerOf10: Int): Iso[BigDecimal, BigDecimal] =
    Iso[BigDecimal, BigDecimal](
      // from units to m/s
      units           => BigDecimal(units.underlying.scaleByPowerOfTen(-powerOf10))
    )(
      // from m/s to units
      metersPerSecond => BigDecimal(metersPerSecond.underlying.scaleByPowerOfTen(powerOf10))
    )

  private def rounding[A](p: Prism[BigDecimal, A]): Prism[Long, A] =
    Prism[Long, A](
      value => p.getOption(BigDecimal(value))
    )(
      a     => p.reverseGet(a).setScale(0, BigDecimal.RoundingMode.HALF_UP).longValue
    )

  sealed abstract class Units(
    val abbreviation: String,
    powerOf10:        Int     // relative to m/s: 2 for cm, -3 for km
  ) extends Product with Serializable {

    val decimal: Prism[BigDecimal, RadialVelocity] =
      scalingBy(powerOf10).andThen(RadialVelocity.fromMetersPerSecond)

    val long: Prism[Long, RadialVelocity] =
      rounding(decimal)

    def readLong(value: Long): ValidatedInput[RadialVelocity] =
      long.getOption(value).toValidNec(
        InputError.fromMessage(s"Invalid radial velocity $value $abbreviation")
      )

    def readDecimal(value: BigDecimal): ValidatedInput[RadialVelocity] =
      decimal.getOption(value).toValidNec(
        InputError.fromMessage(s"Invalid radial velocity $value $abbreviation")
      )
  }

  object Units {

    case object CentimetersPerSecond extends Units("cm/s",  2)
    case object MetersPerSecond      extends Units("m/s",   0)
    case object KilometersPerSecond  extends Units("km/s", -3)

    val centimetersPerSecond: Units = CentimetersPerSecond
    val metersPerSecond: Units      = MetersPerSecond
    val kilometersPerSecond: Units  = KilometersPerSecond

    implicit val EnumeratedRadialVelocityUnits: Enumerated[Units] =
      Enumerated.of(CentimetersPerSecond, MetersPerSecond, KilometersPerSecond)

    implicit val DisplayRadialVelocityUnits: Display[Units] =
      Display.by(_.abbreviation, _.abbreviation)

  }

  implicit val NumericUnitsRadialVelocity: NumericUnits[RadialVelocity, Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  final case class Input(
    centimetersPerSecond: Option[Long],
    metersPerSecond:      Option[BigDecimal],
    kilometersPerSecond:  Option[BigDecimal],
    fromLong:             Option[NumericUnits.LongInput[Units]],
    fromDecimal:          Option[NumericUnits.DecimalInput[Units]]
  ) {

    import Units._

    val toRadialVelocity: ValidatedInput[RadialVelocity] =
      ValidatedInput.requireOne("radial velocity",
        centimetersPerSecond.map(CentimetersPerSecond.readLong),
        metersPerSecond     .map(MetersPerSecond.readDecimal),
        kilometersPerSecond .map(KilometersPerSecond.readDecimal),
        fromLong            .map(_.read),
        fromDecimal         .map(_.read)
      )
  }

  object Input {

    val Empty: Input =
      Input(None, None, None, None, None)

    def fromCentimetersPerSecond(value: Long): Input =
      Empty.copy(centimetersPerSecond = Some(value))

    def fromMetersPerSecond(value: BigDecimal): Input =
      Empty.copy(metersPerSecond = Some(value))

    def fromKilometersPerSecond(value: BigDecimal): Input =
      Empty.copy(kilometersPerSecond = Some(value))

    def fromLong(value: NumericUnits.LongInput[Units]): Input =
      Empty.copy(fromLong = Some(value))

    def fromDecimal(value: NumericUnits.DecimalInput[Units]): Input =
      Empty.copy(fromDecimal = Some(value))

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.centimetersPerSecond,
        in.metersPerSecond,
        in.kilometersPerSecond,
        in.fromLong,
        in.fromDecimal
      ))

  }

}
