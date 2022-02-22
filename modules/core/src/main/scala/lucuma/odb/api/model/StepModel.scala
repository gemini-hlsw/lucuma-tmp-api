// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.{Applicative, Eq, Eval, Traverse}
import cats.effect.Sync
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.odb.api.model.StepConfig.CreateStepConfig


final case class StepModel[A](
  id:         Step.Id,
  breakpoint: Breakpoint,
  config:     StepConfig[A]
) {

  def to[D](f: StepConfig[_] => Option[D]): Option[StepModel[D]] =
    f(config).map(this.as)

  def gmosNorth: Option[StepModel[GmosModel.NorthDynamic]] =
    to(_.gmosNorth)

  def gmosSouth: Option[StepModel[GmosModel.SouthDynamic]] =
    to(_.gmosSouth)

}

object StepModel {

  implicit val TraverseStepModel: Traverse[StepModel] =
    new Traverse[StepModel] {
      override def traverse[G[_], A, B](fa: StepModel[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[StepModel[B]] =
        fa.config.traverse(f).map(sc => StepModel(fa.id, fa.breakpoint, sc))

      override def foldLeft[A, B](fa: StepModel[A], b: B)(f: (B, A) => B): B =
        fa.config.foldLeft(b)(f)

      override def foldRight[A, B](fa: StepModel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.config.foldRight(lb)(f)
    }

  implicit def EqStepModel[A: Eq]: Eq[StepModel[A]] =
    Eq.by { a => (
      a.id,
      a.breakpoint,
      a.config
    )}

  final case class Create[A](
    breakpoint: Breakpoint,
    config:     CreateStepConfig[A]
  ) {

    def create[F[_]: Sync, B](implicit V: InputValidator[A, B]): F[ValidatedInput[StepModel[B]]] =
      Step.Id.random[F].map { sid =>
        config.create[B].map { c =>
          StepModel(sid, breakpoint, c)
        }
      }
  }

  object Create {

    def stopBefore[A](s: CreateStepConfig[A]): Create[A] =
      Create(Breakpoint.enabled, s)

    def continueTo[A](s: CreateStepConfig[A]): Create[A] =
      Create(Breakpoint.disabled, s)

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by { a => (
        a.breakpoint,
        a.config
      )}

    implicit def DecoderCreate[A: Decoder]: Decoder[Create[A]] =
      deriveDecoder[Create[A]]

  }

}
