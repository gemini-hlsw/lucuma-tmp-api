// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.kernel.BoundedEnumerable
import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import monocle.Lens


/**
 * Tracking "last" used ids of top-level types.
 */
final case class Ids(
                      event:       Long,
                      asterism:    AsterismModel.Id,
                      observation: ObservationModel.Id,
                      program:     ProgramModel.Id,
                      target:      TargetModel.Id
)

object Ids extends IdsOptics {

  val zero: Ids =
    Ids(
      event       = 0L,
      asterism    = BoundedEnumerable[AsterismModel.Id].minBound,
      observation = BoundedEnumerable[ObservationModel.Id].minBound,
      program     = BoundedEnumerable[ProgramModel.Id].minBound,
      target      = BoundedEnumerable[TargetModel.Id].minBound
    )

}

sealed trait IdsOptics { self: Ids.type =>

  val lastEvent: Lens[Ids, Long] =
    Lens[Ids, Long](_.event)(b => a => a.copy(event = b))

  val lastAsterism: Lens[Ids, AsterismModel.Id] =
    Lens[Ids, AsterismModel.Id](_.asterism)(b => a => a.copy(asterism = b))

  val lastObservation: Lens[Ids, ObservationModel.Id] =
    Lens[Ids, ObservationModel.Id](_.observation)(b => a => a.copy(observation = b))

  val lastProgram: Lens[Ids, ProgramModel.Id] =
    Lens[Ids, ProgramModel.Id](_.program)(b => a => a.copy(program = b))

  val lastTarget: Lens[Ids, TargetModel.Id] =
    Lens[Ids, TargetModel.Id](_.target)(b => a => a.copy(target = b))

}