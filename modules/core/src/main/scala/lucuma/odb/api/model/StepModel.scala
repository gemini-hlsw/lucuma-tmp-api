// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.Step
import lucuma.odb.api.model.StepConfig.CreateStepConfig

import cats.Eq
import cats.data.State
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.macros.Lenses


@Lenses final case class StepModel[A](
  id:         Step.Id,
  breakpoint: Breakpoint,
  config:     StepConfig[A]
)

object StepModel {
  implicit def EqStepModel[A: Eq]: Eq[StepModel[A]] =
    Eq.by { a => (
      a.id,
      a.breakpoint,
      a.config
    )}

  @Lenses final case class Create[A](
    id:         Option[Step.Id],
    breakpoint: Breakpoint,
    config:     CreateStepConfig[A]
  ) {

    def create[T, B](db: Database[T])(implicit V: InputValidator[A, B]): State[T, ValidatedInput[StepModel[B]]] =
      for {
        i <- db.step.getUnusedId(id)
        o  = (i, config.create[B]).mapN { (i, c) => StepModel(i, breakpoint, c) }
        _ <- db.step.saveValid(o)(_.id)
      } yield o

  }

  object Create {

    def stopBefore[A](s: CreateStepConfig[A]): Create[A] =
      Create(None, Breakpoint.enabled, s)

    def continueTo[A](s: CreateStepConfig[A]): Create[A] =
      Create(None, Breakpoint.disabled, s)

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by { a => (
        a.id,
        a.breakpoint,
        a.config
      )}

    implicit def DecoderCreate[A: Decoder]: Decoder[Create[A]] =
      deriveDecoder[Create[A]]

  }

}