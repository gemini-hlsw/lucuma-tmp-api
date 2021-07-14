// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Atom, Step}
import lucuma.core.model.{ExecutionEvent, Observation, Program}
import cats.kernel.BoundedEnumerable
import monocle.Lens

/**
 * Tracking "last" used ids of top-level types.
 */
final case class Ids(
  event:          Long,
  atom:           Atom.Id,
  executionEvent: ExecutionEvent.Id,
  observation:    Observation.Id,
  program:        Program.Id,
  step:           Step.Id
)

object Ids extends IdsOptics {

  val zero: Ids =
    Ids(
      event          = 0L,
      atom           = BoundedEnumerable[Atom.Id].minBound,
      executionEvent = BoundedEnumerable[ExecutionEvent.Id].minBound,
      observation    = BoundedEnumerable[Observation.Id].minBound,
      program        = BoundedEnumerable[Program.Id].minBound,
      step           = BoundedEnumerable[Step.Id].minBound
    )

}

sealed trait IdsOptics { self: Ids.type =>

  val lastEvent: Lens[Ids, Long] =
    Lens[Ids, Long](_.event)(b => a => a.copy(event = b))

  val lastAtom: Lens[Ids, Atom.Id] =
    Lens[Ids, Atom.Id](_.atom)(b => a => a.copy(atom = b))

  val lastExecutionEvent: Lens[Ids, ExecutionEvent.Id] =
    Lens[Ids, ExecutionEvent.Id](_.executionEvent)(b => a => a.copy(executionEvent = b))

  val lastObservation: Lens[Ids, Observation.Id] =
    Lens[Ids, Observation.Id](_.observation)(b => a => a.copy(observation = b))

  val lastProgram: Lens[Ids, Program.Id] =
    Lens[Ids, Program.Id](_.program)(b => a => a.copy(program = b))

  val lastStep: Lens[Ids, Step.Id] =
    Lens[Ids, Step.Id](_.step)(b => a => a.copy(step = b))

}
