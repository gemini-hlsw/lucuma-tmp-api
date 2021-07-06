// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.core.math.ProperMotion.AngularVelocityComponent
import lucuma.core.math.{Angle, ProperMotion}
import lucuma.core.math.VelocityAxis.{Dec, RA}
import lucuma.core.optics.SplitMono
import lucuma.core.util.{Display, Enumerated}
import cats.syntax.apply._
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto._


object ProperMotionModel {

  sealed abstract class Units(
    val angleUnit: AngleModel.Units
  ) extends Product with Serializable {

    private def angleToComponent[A, B](m: SplitMono[Angle, B]): SplitMono[AngularVelocityComponent[A], B] =
      m.imapA(
        a => AngularVelocityComponent.microarcsecondsPerYear.get(a.toMicroarcseconds),
        c => Angle.fromMicroarcseconds(c.μasy.value)
      )

    def long[A]: SplitMono[AngularVelocityComponent[A], Long] =
      angleToComponent[A, Long](angleUnit.signedLong)

    def readLong[A](l: Long): ValidatedInput[AngularVelocityComponent[A]] =
      long[A].reverseGet(l).validNec[InputError]

    def decimal[A]: SplitMono[AngularVelocityComponent[A], BigDecimal] =
      angleToComponent[A, BigDecimal](angleUnit.signedDecimal)

    def readDecimal[A](b: BigDecimal): ValidatedInput[AngularVelocityComponent[A]] =
      decimal[A].reverseGet(b).validNec[InputError]

  }

  object Units {

    case object MicroarcsecondsPerYear extends Units(AngleModel.Units.Microarcseconds)
    case object MilliarcsecondsPerYear extends Units(AngleModel.Units.Milliarcseconds)

    val microarcsecondsPerYear: Units = MicroarcsecondsPerYear
    val milliarcsecondsPerYear: Units = MilliarcsecondsPerYear

    implicit def EnumeratedProperMotionUnits: Enumerated[Units] =
      Enumerated.of(MicroarcsecondsPerYear, MilliarcsecondsPerYear)

    implicit def DisplayProperMotionUnits: Display[Units] =
      Display.by(u => s"${u.angleUnit.abbreviation}/yr", u => s"${u.angleUnit.name}/year")

  }

  implicit def NumericUnitsProperMotionComponent[A]: NumericUnits[AngularVelocityComponent[A], Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  final case class ComponentInput(
    microarcsecondsPerYear: Option[Long],
    milliarcsecondsPerYear: Option[BigDecimal],
    fromLong:               Option[NumericUnits.LongInput[Units]],
    fromDecimal:            Option[NumericUnits.DecimalInput[Units]]
  ) {

    def toComponent[A]: ValidatedInput[AngularVelocityComponent[A]] =
      ValidatedInput.requireOne("proper velocity component",
        microarcsecondsPerYear.map(Units.MicroarcsecondsPerYear.readLong[A]),
        milliarcsecondsPerYear.map(Units.MilliarcsecondsPerYear.readDecimal[A]),
        fromLong       .map(_.read),
        fromDecimal    .map(_.read)
      )

  }

  object ComponentInput {

    def Empty: ComponentInput =
      ComponentInput(None, None, None, None)

    def fromMicroarcsecondsPerYear[A](value: Long): ComponentInput =
      Empty.copy(microarcsecondsPerYear = Some(value))

    def fromMilliarcsecondsPerYear(value: BigDecimal): ComponentInput =
      Empty.copy(milliarcsecondsPerYear = Some(value))

    def fromLong(value: NumericUnits.LongInput[Units]): ComponentInput =
      Empty.copy(fromLong = Some(value))

    def fromDecimal(value: NumericUnits.DecimalInput[Units]): ComponentInput =
      Empty.copy(fromDecimal = Some(value))

    implicit def DecoderComponentInput: Decoder[ComponentInput] =
      deriveDecoder[ComponentInput]

    implicit def EqComponentInput: Eq[ComponentInput] =
      Eq.by(in => (
        in.microarcsecondsPerYear,
        in.milliarcsecondsPerYear,
        in.fromLong,
        in.fromDecimal
      ))

  }

  final case class Input(
    ra: ComponentInput,
    dec: ComponentInput
  ) {

    val toProperMotion: ValidatedInput[ProperMotion] =
      (ra.toComponent[RA], dec.toComponent[Dec]).mapN { case (ra, dec) =>
        ProperMotion(ra, dec)
      }

  }

  object Input {

    def fromMicroarcsecondsPerYear(ra: Long, dec: Long): Input =
      Input(
        ComponentInput.fromMicroarcsecondsPerYear(ra),
        ComponentInput.fromMicroarcsecondsPerYear(dec)
      )

    def fromMilliarcsecondsPerYear(ra: BigDecimal, dec: BigDecimal): Input =
      Input(
        ComponentInput.fromMilliarcsecondsPerYear(ra),
        ComponentInput.fromMilliarcsecondsPerYear(dec)
      )

    implicit val DecoderProperMotionInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.ra,
        in.dec
      ))

  }

}
