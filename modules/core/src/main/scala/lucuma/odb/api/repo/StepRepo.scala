// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.Functor
import lucuma.core.model.Step
import lucuma.odb.api.model.{Database, StepModel}
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
    databaseRef: Ref[F, Database]
  ): StepRepo[F] =

    new StepRepo[F] {

      override def selectStep(
        sid: Step.Id
      ): F[Option[StepModel[_]]] =
        databaseRef.get.map(_.steps.rows.get(sid))

      override def unsafeSelectStep(
        sid: Step.Id
      ): F[StepModel[_]] =
        selectStep(sid).map(_.getOrElse(sys.error(s"Step id '$sid' missing'")))

    }

}
