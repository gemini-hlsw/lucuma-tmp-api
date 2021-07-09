// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.core.math.{Angle, Axis, Offset}
import lucuma.core.optics.SplitMono
import cats.syntax.apply._
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.util.{Display, Enumerated}
import monocle.macros.GenLens
import monocle.Lens

object OffsetModel {

  sealed abstract class Units(
    val angleUnit: AngleModel.Units
  ) extends Product with Serializable {

    private def angleToComponent[A, B](m: SplitMono[Angle, B]): SplitMono[Offset.Component[A], B] =
      m.imapA(Offset.Component.apply[A], _.toAngle)

    def long[A]: SplitMono[Offset.Component[A], Long] =
      angleToComponent[A, Long](angleUnit.signedLong)

    def readLong[A](l: Long): ValidatedInput[Offset.Component[A]] =
      long[A].reverseGet(l).validNec[InputError]

    def decimal[A]: SplitMono[Offset.Component[A], BigDecimal] =
      angleToComponent[A, BigDecimal](angleUnit.signedDecimal)

    def readDecimal[A](b: BigDecimal): ValidatedInput[Offset.Component[A]] =
      decimal[A].reverseGet(b).validNec[InputError]
  }

  object Units {

    case object Microarcseconds extends Units(AngleModel.Units.Microarcseconds)
    case object Milliarcseconds extends Units(AngleModel.Units.Milliarcseconds)
    case object Arcseconds      extends Units(AngleModel.Units.Arcseconds)

    val microarcseconds: Units = Microarcseconds
    val milliarcseconds: Units = Milliarcseconds
    val arcseconds: Units      = Arcseconds

    implicit def EnumeratedUnits: Enumerated[Units] =
      Enumerated.of(
        Microarcseconds,
        Milliarcseconds,
        Arcseconds
      )

    implicit def DisplayUnits: Display[Units] =
      Display.byShortName(_.angleUnit.abbreviation)

  }

  implicit def NumericUnitsOffsetComponent[A]: NumericUnits[Offset.Component[A], Units] =
    NumericUnits.fromRead(_.readLong(_), _.readDecimal(_))

  final case class ComponentInput(
    microarcseconds: Option[Long],
    milliarcseconds: Option[BigDecimal],
    arcseconds:      Option[BigDecimal],
    fromLong:        Option[NumericUnits.LongInput[Units]],
    fromDecimal:     Option[NumericUnits.DecimalInput[Units]]
  ) {

    def toComponent[A]: ValidatedInput[Offset.Component[A]] =
      ValidatedInput.requireOne("offset component",
        microarcseconds.map(Units.Microarcseconds.readLong[A]),
        milliarcseconds.map(Units.Milliarcseconds.readDecimal[A]),
        arcseconds     .map(Units.Arcseconds.readDecimal[A]),
        fromLong       .map(_.read),
        fromDecimal    .map(_.read)
      )

  }

  object ComponentInput {

    def Empty: ComponentInput =
      ComponentInput(None, None, None, None, None)

    def Zero: ComponentInput =
      fromMicroarcseconds(0L)

    def apply[A](value: Angle): ComponentInput =
      fromMicroarcseconds(value.toMicroarcseconds)

    def fromMicroarcseconds[A](value: Long): ComponentInput =
      Empty.copy(microarcseconds = Some(value))

    def fromMilliarcseconds[A](value: BigDecimal): ComponentInput =
      Empty.copy(milliarcseconds = Some(value))

    def fromArcseconds[A](value: BigDecimal): ComponentInput =
      Empty.copy(arcseconds = Some(value))

    def fromLong(value: NumericUnits.LongInput[Units]): ComponentInput =
      Empty.copy(fromLong = Some(value))

    def fromDecimal(value: NumericUnits.DecimalInput[Units]): ComponentInput =
      Empty.copy(fromDecimal = Some(value))

    implicit def DecoderComponentInput: Decoder[ComponentInput] =
      deriveDecoder[ComponentInput]

    implicit def EqComponentInput: Eq[ComponentInput] =
      Eq.by(in => (
        in.microarcseconds,
        in.milliarcseconds,
        in.arcseconds,
        in.fromLong,
        in.fromDecimal
      ))
  }

  final case class Input(
    p: ComponentInput,
    q: ComponentInput
  ) {

    val create: ValidatedInput[Offset] =
      (p.toComponent[Axis.P], q.toComponent[Axis.Q]).mapN { case (p, q) =>
        Offset(p, q)
      }

  }

  object Input {
    val p: Lens[Input, ComponentInput] = GenLens[Input](_.p)
    val q: Lens[Input, ComponentInput] = GenLens[Input](_.q)

    val Zero: Input =
      Input(
        ComponentInput.Zero,
        ComponentInput.Zero
      )

    def fromMicroarcseconds(p: Long, q: Long): Input =
      Input(
        ComponentInput.fromMicroarcseconds(p),
        ComponentInput.fromMicroarcseconds(q)
      )

    def fromMilliarcseconds(p: BigDecimal, q: BigDecimal): Input =
      Input(
        ComponentInput.fromMilliarcseconds(p),
        ComponentInput.fromMilliarcseconds(q)
      )

    def fromArcseconds(p: BigDecimal, q: BigDecimal): Input =
      Input(
        ComponentInput.fromArcseconds(p),
        ComponentInput.fromArcseconds(q)
      )

    implicit val DecoderInput: Decoder[Input]=
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(in => (
        in.p,
        in.q
      ))

    implicit val ValidatorInput: InputValidator[Input, Offset] = {
      (in: Input) => in.create
    }
  }

}
