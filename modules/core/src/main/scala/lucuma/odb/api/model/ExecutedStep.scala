// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Atom, Step}

import cats.{Eq, Functor}
import cats.mtl.Stateful

final case class ExecutedStep(
  stepId:       Step.Id,
  atomId:       Atom.Id,
  sequenceType: SequenceModel.SequenceType
) {

  def dereference[F[_]: Functor, T, D](
    db: DatabaseReader[T]
  )(
    f: StepConfig[_] => Option[D]
  )(
    implicit S: Stateful[F, T]
  ): F[Option[StepModel[D]]] =

    StepModel.dereference[F, T, D](db, stepId)(f)

}

object ExecutedStep {

  implicit val EqExecutedStep: Eq[ExecutedStep] =
    Eq.by { a => (
      a.stepId,
      a.atomId,
      a.sequenceType
    )}

}
