// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.kernel.BoundedEnumerable
import lucuma.core.model.{ Asterism, Observation, Program, Target }
import monocle.Lens
import lucuma.core.model.ConstraintSet

/**
 * Tracking "last" used ids of top-level types.
 */
final case class Ids(
  event:         Long,
  asterism:      Asterism.Id,
  constraintSet: ConstraintSet.Id,
  observation:   Observation.Id,
  program:       Program.Id,
  target:        Target.Id
)

object Ids extends IdsOptics {

  val zero: Ids =
    Ids(
      event         = 0L,
      asterism      = BoundedEnumerable[Asterism.Id].minBound,
      constraintSet = BoundedEnumerable[ConstraintSet.Id].minBound,
      observation   = BoundedEnumerable[Observation.Id].minBound,
      program       = BoundedEnumerable[Program.Id].minBound,
      target        = BoundedEnumerable[Target.Id].minBound
    )

}

sealed trait IdsOptics { self: Ids.type =>

  val lastEvent: Lens[Ids, Long] =
    Lens[Ids, Long](_.event)(b => a => a.copy(event = b))

  val lastAsterism: Lens[Ids, Asterism.Id] =
    Lens[Ids, Asterism.Id](_.asterism)(b => a => a.copy(asterism = b))

  val lastConstraintSet: Lens[Ids, ConstraintSet.Id] =
    Lens[Ids, ConstraintSet.Id](_.constraintSet)(b => a => a.copy(constraintSet = b))

  val lastObservation: Lens[Ids, Observation.Id] =
    Lens[Ids, Observation.Id](_.observation)(b => a => a.copy(observation = b))

  val lastProgram: Lens[Ids, Program.Id] =
    Lens[Ids, Program.Id](_.program)(b => a => a.copy(program = b))

  val lastTarget: Lens[Ids, Target.Id] =
    Lens[Ids, Target.Id](_.target)(b => a => a.copy(target = b))

}
