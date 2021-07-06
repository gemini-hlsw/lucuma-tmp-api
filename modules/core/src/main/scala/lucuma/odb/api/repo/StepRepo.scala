// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.Functor
import lucuma.core.model.Step
import lucuma.odb.api.model.StepModel
import cats.effect.Ref
import cats.syntax.all._

sealed trait StepRepo[F[_]] {

  def selectStep(
    sid: Step.Id
  ): F[Option[StepModel[_]]]

  def unsafeSelectStep(
    sid: Step.Id
  ): F[StepModel[_]]

}

object StepRepo {

  def create[F[_]: Functor](
    tablesRef: Ref[F, Tables]
  ): StepRepo[F] =

    new StepRepo[F] {
      override def selectStep(
        sid: Step.Id
      ): F[Option[StepModel[_]]] =
        tablesRef.get.map(Tables.step(sid).get)

      override def unsafeSelectStep(
        sid: Step.Id
      ): F[StepModel[_]] =
        selectStep(sid).map(_.getOrElse(sys.error(s"Step id '$sid' missing'")))

    }

}
