// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.functor._
import lucuma.odb.api.model.AngleModel.AngleInput
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import io.circe.Decoder

import cats.syntax.traverse._
import clue.data.Input

import PosAngleConstraintInput.{FixedInput, AverageParallacticInput}

final case class PosAngleConstraintInput(
  fixed:              Input[FixedInput]              = Input.ignore,
  averageParallactic: Input[AverageParallacticInput] = Input.ignore
) extends EditorInput[PosAngleConstraint] {

  override val create: ValidatedInput[PosAngleConstraint] =
    ValidatedInput.requireOne("posAngleConstraint",
      fixed.map(_.create.widen[PosAngleConstraint]).toOption,
      averageParallactic.map(_.create.widen[PosAngleConstraint]).toOption
   )

  override val edit: StateT[EitherInput, PosAngleConstraint, Unit] =
    EditorInput.editOneOf[PosAngleConstraint, PosAngleConstraint.Fixed, PosAngleConstraint.AverageParallactic](
      ("fixed",              fixed,              PosAngleConstraint.fixed             ),
      ("averageParallactic", averageParallactic, PosAngleConstraint.averageParallactic)
    )

}

object PosAngleConstraintInput {

  import PosAngleConstraint.{Fixed, AverageParallactic}

  final case class FixedInput(
    angle:     Input[AngleInput] = Input.ignore,
    allowFlip: Input[Boolean]    = Input.ignore
  ) extends EditorInput[Fixed] {

    override val create: ValidatedInput[Fixed] =
      angle.notMissing("angle").andThen { a =>
        a.toAngle.map(Fixed(_, allowFlip.toOption.getOrElse(false)))
      }

    override val edit: StateT[EitherInput, Fixed, Unit] =
      for {
        a <- angle.validateNotNullable("angle")(_.toAngle).liftState[Fixed]
        _ <- Fixed.angle     := a
        _ <- Fixed.allowFlip := allowFlip.toOption
      } yield ()

  }

  object FixedInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderFixedInput: Decoder[FixedInput] =
      deriveConfiguredDecoder[FixedInput]

    implicit val EqFixedInput: Eq[FixedInput] =
      Eq.by { a => (
        a.angle,
        a.allowFlip
      )}

  }

  final case class AverageParallacticInput(
    overrideAngle: Input[AngleInput] = Input.ignore
  ) extends EditorInput[AverageParallactic] {

    override val create: ValidatedInput[AverageParallactic] =
      overrideAngle.toOption.traverse(_.toAngle).map(AverageParallactic(_))

    override val edit: StateT[EitherInput, AverageParallactic, Unit] =
      for {
        a <- overrideAngle.validateNullable(_.toAngle).liftState[AverageParallactic]
        _ <- AverageParallactic.overrideAngle := a
      } yield ()

  }

  object AverageParallacticInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderAverageParallacticInput: Decoder[AverageParallacticInput] =
      deriveConfiguredDecoder[AverageParallacticInput]

    implicit val EqAverageParallacticInput: Eq[AverageParallacticInput] =
      Eq.by(_.overrideAngle)

  }

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderPosAngleConstraintInput: Decoder[PosAngleConstraintInput] =
    deriveConfiguredDecoder[PosAngleConstraintInput]

  implicit val EqPosAngleConstraintInput: Eq[PosAngleConstraintInput] =
    Eq.by { a => (
      a.fixed,
      a.averageParallactic
    )}

}
