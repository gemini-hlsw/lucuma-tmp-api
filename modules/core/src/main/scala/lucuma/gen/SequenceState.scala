// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.data.State
import lucuma.core.`enum`.{GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import lucuma.core.math.{Angle, Offset}
import lucuma.odb.api.model.{GcalModel, StepConfig}

/**
 * Sequence generation helper trait.
 */
private[gen] trait SequenceState[D] {

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

  def step(f: D => StepConfig[D]): State[D, StepConfig[D]] =
    State.inspect[D, StepConfig[D]](f)

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given telescope configuration.
   */
  def scienceStep(o: Offset): State[D, StepConfig[D]] =
    step(StepConfig.Science(_, o))

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given offset.
   *
   * @param p offset in p
   * @param q offset in q
   */
  def scienceStep(p: Angle, q: Angle): State[D, StepConfig[D]] =
    scienceStep(Offset(Offset.P(p), Offset.Q(q)))

  /**
   * Generates a GCAL flat based on the current instrument configuration.
   */
  def flatStep: State[D, StepConfig[D]] =
    step { d =>
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