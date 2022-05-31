// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import lucuma.core.syntax.string._
import lucuma.odb.api.model.AngleModel.AngleInput
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import io.circe.Decoder
import cats.syntax.traverse._
import clue.data.Input
import lucuma.odb.api.model.PosAngleConstraint.Type

final case class PosAngleConstraintInput(
  constraint: Input[PosAngleConstraint.Type] = Input.ignore,
  angle:      Input[AngleInput]              = Input.ignore,
) extends EditorInput[PosAngleConstraint] {

  override val create: ValidatedInput[PosAngleConstraint] =
    constraint.notMissing("constraint").andThen {
      case t@(Type.Fixed | Type.AllowFlip) =>
        angle.notMissingAndThen("angle")(_.toAngle).map { a =>
          PosAngleConstraint(t, a.some)
        }
      case t@Type.AverageParallactic       =>
        angle.toOption.traverse(_.toAngle).map { o =>
          PosAngleConstraint(t, o)
        }
    }

  private val validate: StateT[EitherInput, PosAngleConstraint, Unit] =
    StateT.inspectF { pac =>
      pac.constraint match {
        case t@(Type.Fixed | Type.AllowFlip) =>
          pac.angle.toRightNec(
            InputError.fromMessage(s"${t.tag.value.toScreamingSnakeCase} constraints require an associated `angle` value")
          ).void
        case Type.AverageParallactic     =>
          ().rightNec[InputError]
      }
    }

  override val edit: StateT[EitherInput, PosAngleConstraint, Unit] = {
    val validArgs = (
      constraint.validateIsNotNull("constraint"),
      angle.validateNullable(_.toAngle)
    ).tupled

    for {
      args  <- validArgs.liftState
      (c, a) = args
      _     <- PosAngleConstraint.constraint := c
      _     <- PosAngleConstraint.angle      := a
      _     <- validate
    } yield ()
  }

}

object PosAngleConstraintInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderPosAngleConstraintInput: Decoder[PosAngleConstraintInput] =
    deriveConfiguredDecoder[PosAngleConstraintInput]

  implicit val EqPosAngleConstraintInput: Eq[PosAngleConstraintInput] =
    Eq.by { a => (
      a.constraint,
      a.angle
    )}

}
