// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.Monad
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.{ExecutionModel, Sequence, SequenceModel}

/**
 * A sequence generator for the given static `S` and dynamic `D` types for a
 * particular observing mode.
 *
 * @tparam F effect type
 * @tparam S type of static (unchanging) configuration data
 * @tparam D type of dynamic (potentially changing from step to step) configuration
 */
trait Generator[F[_], S, D] { self =>

  def instrument: Instrument

  /**
   * Static, unchanging, configuration for this observing mode.
   */
  def static: S

  /**
   * Generates the acquisition sequence expected for the remainder of the
   * acquisition.
   */
  def acquisition(
    recordedSteps: List[RecordedStep[D]]
  ): F[Sequence[D]]

  /**
   * Generates the science sequence expected for the remainder of the
   * observation.
   */
  def science(
    recordedSteps: List[RecordedStep[D]]
  ): F[Sequence[D]]

  /**
   * Runs the Generator to make the execution config with its static config,
   * acquisition sequence, and science sequence.
   */
  def run(
    recordedSteps: List[RecordedStep[D]]
  )(implicit ev: Monad[F]): F[ExecutionModel.Config[S, D]] =

    for {
      a <- acquisition(recordedSteps)
      s <- science(recordedSteps)
    } yield ExecutionModel.Config(static, a, s)

}

object Generator {

  /**
   * Runs the Generator to make the execution config with its static config,
   * acquisition sequence, and science sequence.
   */
  def run[F[_]: Monad, S, D](
    generator: Generator[F, S, D]
  )(
    recordedSteps: List[RecordedStep[D]]
  ): F[ExecutionModel.Config[S, D]] =

    for {
      a <- generator.acquisition(recordedSteps)
      s <- generator.science(recordedSteps)
    } yield ExecutionModel.Config(generator.static, a, s)

  /**
   * Creates a "generator" that faithfully generates the manually defined
   * sequences.
   */
  def manual[F[_]: Sync, S, D](
    inst:   Instrument,
    config: ExecutionModel.Config[S, D]
  ): Generator[F, S, D] =

    new Generator[F, S, D] with GeneratorHelper[D] {
      override def instrument: Instrument =
        inst

      override def static: S =
        config.static

      override def acquisition(recordedSteps: List[RecordedStep[D]]): F[Sequence[D]] =
        Sync[F].pure(remainingManualSteps(config.acquisition, recordedSteps))

      override def science(recordedSteps: List[RecordedStep[D]]): F[Sequence[D]] =
        Sync[F].pure(remainingManualSteps(config.science, recordedSteps))

      private def remainingManualSteps(
        wholeSequence: Sequence[D],
        recordedSteps: List[RecordedStep[D]]
      ): Sequence[D] =
        SequenceModel(
          remainingSteps(wholeSequence.atoms, recordedSteps)(_.steps.map(_.config))
        )

    }

}
