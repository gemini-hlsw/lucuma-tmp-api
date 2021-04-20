// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.StepConfig.CreateStepConfig

import cats.Eq
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.macros.Lenses


@Lenses final case class StepModel[A](
  breakpoint: Breakpoint,
  config:     StepConfig[A]
)

object StepModel {
  implicit def EqStepModel[A: Eq]: Eq[StepModel[A]] =
    Eq.by { a => (
      a.breakpoint,
      a.config
    )}

  @Lenses final case class Create[A](
    breakpoint: Breakpoint,
    step:       CreateStepConfig[A]
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[StepModel[B]] =
      step.create[B].map(s => StepModel(breakpoint, s))

  }

  object Create {

    def stopBefore[A](s: CreateStepConfig[A]): Create[A] =
      Create(Breakpoint.enabled, s)

    def continueTo[A](s: CreateStepConfig[A]): Create[A] =
      Create(Breakpoint.disabled, s)

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by { a => (
        a.breakpoint,
        a.step
      )}

    implicit def DecoderCreate[A: Decoder]: Decoder[Create[A]] =
      deriveDecoder[Create[A]]

    implicit def ValidatorCreate[A, B](implicit V: InputValidator[A, B]): InputValidator[Create[A], StepModel[B]] =
      (cbs: Create[A]) => cbs.create[B]
  }

}