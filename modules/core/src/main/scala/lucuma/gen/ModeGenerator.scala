// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.effect.Sync
import fs2.Stream
import lucuma.odb.api.model.{AtomModel, StepModel}


trait ModeGenerator[S, D] {

  def static: S

  def acquisition[F[_]: Sync](
    acquired: F[Boolean]
  ): Stream[F, AtomModel[StepModel[D]]]

  def reacquisition[F[_]: Sync](
    acquired: F[Boolean]
  ): Stream[F, AtomModel[StepModel[D]]]

  def science[F[_]: Sync](
    observed: F[Boolean]
  ): Stream[F, AtomModel[StepModel[D]]]

}
