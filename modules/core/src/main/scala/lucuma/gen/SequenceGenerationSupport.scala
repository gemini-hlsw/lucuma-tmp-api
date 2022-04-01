// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.data.{NonEmptyList, State}
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import lucuma.core.`enum`.{GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import lucuma.core.math.{Angle, Offset}
import lucuma.odb.api.model.{Atom, AtomModel, Breakpoint, GcalModel, Step, StepConfig, StepModel}

/**
 * Sequence generation helper trait.
 */
private[gen] trait SequenceGenerationSupport[D] {

  /**
   * Sequence generation involves defining edits to the dynamic instrument
   * configuration.  The starting point instrument configuration, or
   * initialConfig, defines the initial state and subsequent steps edit it
   * in a `State` "program".
   */
  def initialConfig: D

  /**
   * Evaluates the stateful computation to produce a value.
   *
   * @param prog definition of the state computation
   * @tparam A type of value ultimately produced
   *
   * @return result of the state computation
   */
  def eval[A](prog: State[D, A]): A =
    prog.runA(initialConfig).value

  /**
   * Creates an atom from the given steps.
   */
  def atom[F[_]: Sync](
    s0: F[StepModel[D]],
    ss: F[StepModel[D]]*
  ): F[AtomModel[StepModel[D]]] =
    for {
      aid   <- Atom.Id.random[F]
      steps <- NonEmptyList(s0, ss.toList).sequence
    } yield AtomModel(aid, steps)

  def step[F[_]: Sync](
    toStepConfig: D => StepConfig[D],
    breakpoint:   Breakpoint = Breakpoint.disabled
  ): State[D, F[StepModel[D]]] =
    State.inspect[D, F[StepModel[D]]] { d =>
      Step.Id.random[F].map { sid => StepModel(sid, breakpoint, toStepConfig(d)) }
    }

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given telescope configuration.
   */
  def scienceStep[F[_]: Sync](o: Offset): State[D, F[StepModel[D]]] =
    step[F](StepConfig.Science(_, o))

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given offset.
   *
   * @param p offset in p
   * @param q offset in q
   */
  def scienceStep[F[_]: Sync](p: Angle, q: Angle): State[D, F[StepModel[D]]] =
    scienceStep[F](Offset(Offset.P(p), Offset.Q(q)))

  /**
   * Generates a GCAL flat based on the current instrument configuration.
   */
  def flatStep[F[_]: Sync]: State[D, F[StepModel[D]]] =
    step[F] { d =>
      StepConfig.Gcal(
        d,

        // TODO: SmartGcal.  This is a placeholder
        GcalModel(
          GcalModel.Lamp.fromContinuum(GcalContinuum.QuartzHalogen),
          GcalFilter.Nd10,
          GcalDiffuser.Ir,
          GcalShutter.Open
        )
      )
    }

}
