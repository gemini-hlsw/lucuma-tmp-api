// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.core.math.{Angle, Parallax}
import lucuma.core.util.{Display, Enumerated}
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.optics.SplitMono


object ParallaxModel {

  sealed abstract class Units(
    val angleUnit:    AngleModel.Units
  ) extends Product with Serializable {

    private def angleToParallax[A](m: SplitMono[Angle, A]): SplitMono[Parallax, A] =
      m.imapA(
        a => Parallax.fromMicroarcseconds(a.toMicroarcseconds),
        p => Angle.fromMicroarcseconds(p.μas.value.value)
      )

    val long: SplitMono[Parallax, Long] =
      angleToParallax(angleUnit.signedLong)

    def readLong(value: Long): ValidatedInput[Parallax] =
      long.reverseGet(value).validNec[InputError]

    val decimal: SplitMono[Parallax, BigDecimal] =
      angleToParallax(angleUnit.signedDecimal)

    def readDecimal(value: BigDecimal): ValidatedInput[Parallax] =
      decimal.reverseGet(value).validNec[InputError]

  }

  object Units {

    case object Microarcseconds extends Units(AngleModel.Units.Microarcseconds)
    case object Milliarcseconds extends Units(AngleModel.Units.Milliarcseconds)

    val microarcseconds: Units = Microarcseconds
    val milliarcseconds: Units = Milliarcseconds

    implicit val EnumeratedParallaxUnits: Enumerated[Units] =
      Enumerated.of(Microarcseconds, Milliarcseconds)

    implicit val DisplayParallaxUnits: Display[Units] =
      Display.by(_.angleUnit.abbreviation, _.angleUnit.name)

  }

  implicit val NumericUnitsParallax: NumericUnits[Parallax, Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  final case class Input(
    microarcseconds: Option[Long],
    milliarcseconds: Option[BigDecimal],
    fromLong:        Option[NumericUnits.LongInput[Units]],
    fromDecimal:     Option[NumericUnits.DecimalInput[Units]]
  ) {

    import Units._

    val toParallax: ValidatedInput[Parallax] =
      ValidatedInput.requireOne("parallax",
        microarcseconds.map(Microarcseconds.readLong),
        milliarcseconds.map(Milliarcseconds.readDecimal),
        fromLong       .map(_.read),
        fromDecimal    .map(_.read)
      )

  }

  object Input {

    val Empty: Input =
      Input(None, None, None, None)

    def fromMicroarcseconds(value: Long): Input =
      Empty.copy(microarcseconds = Some(value))

    def fromMilliarcseconds(value: BigDecimal): Input =
      Empty.copy(milliarcseconds = Some(value))

    def fromLong(value: NumericUnits.LongInput[Units]): Input =
      Empty.copy(fromLong = Some(value))

    def fromDecimal(value: NumericUnits.DecimalInput[Units]): Input =
      Empty.copy(fromDecimal = Some(value))

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.microarcseconds,
        in.milliarcseconds,
        in.fromLong,
        in.fromDecimal
      ))

  }
}
