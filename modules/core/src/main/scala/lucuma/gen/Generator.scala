// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.effect.Sync
import fs2.Stream
import lucuma.odb.api.model.{AtomModel, StepModel}

/**
 * A sequence generator for the given static `S` and dynamic `D` types for a
 * particular observing mode.
 *
 * @tparam S type of static (unchanging) configuration data
 * @tparam D type of dynamic (potentially changing from step to step) configuration
 */
trait Generator[S, D] {

  def static: S

  /**
   * Generates a full acquisition sequence, stopping when the given `acquired`
   * computation produces a `true` value.
   *
   * @param acquired a computation that determines when the target is acquired
   */
  def acquisition[F[_]: Sync](
    acquired: F[Boolean]
  ): Stream[F, AtomModel[StepModel[D]]]

  /**
   * Generates a re-acquisition sequence, stopping only when the given
   * `acquired` computation produces a `true` value.
   *
   * @param acquired a computation that determines when the target is reacquired
   */
  def reacquisition[F[_]: Sync](
    acquired: F[Boolean]
  ): Stream[F, AtomModel[StepModel[D]]]

  /**
   * Generates the full science sequence, stopping when the given `observed`
   * computation produces a `true` value (e.g., when the required S/N has been
   * reached).
   *
   * @param observed a computation that decides when the observation is done
   */
  def science[F[_]: Sync](
    observed: F[Boolean]
  ): Stream[F, AtomModel[StepModel[D]]]

}
