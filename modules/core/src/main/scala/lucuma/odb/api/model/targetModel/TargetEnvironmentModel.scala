// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.core.math.Coordinates
import lucuma.core.model.{Observation, Program, Target, WithId}
import cats.Eq
import eu.timepit.refined.auto._
import monocle.Lens


final case class CommonTargetEnvironment(
  explicitBase: Option[Coordinates],
  science:      Set[Target]
)

object CommonTargetEnvironment {
  implicit val EqGroupingTargetEnvironment: Eq[CommonTargetEnvironment] =
    Eq.by { a => (
      a.explicitBase,
      a.science
    )}

}

object TargetEnvironment extends WithId('v')

/**
 * The TargetEnvironmentModel with id and references to the containing
 * observation (if any) and program.  The targets themselves reference the
 * environment and have their own ids so they are not included.
 */
final case class TargetEnvironmentModel(
  id:            TargetEnvironment.Id,
  programId:     Program.Id,
  observationId: Option[Observation.Id],

  explicitBase:  Option[Coordinates]
)  {

  def toCommon(
    science: Iterable[Target]
  ): CommonTargetEnvironment =
    CommonTargetEnvironment(explicitBase, Set.from(science))

}

object TargetEnvironmentModel extends TargetEnvironmentModelOptics {

  def empty(
    id:            TargetEnvironment.Id,
    programId:     Program.Id,
    observationId: Option[Observation.Id]
  ): TargetEnvironmentModel =
    TargetEnvironmentModel(id, programId, observationId, None)

  implicit val EqTargetEnvironmentModel: Eq[TargetEnvironmentModel] =
    Eq.by { tem =>
      (
        tem.id,
        tem.programId,
        tem.observationId,
        tem.explicitBase
      )
    }

}


trait TargetEnvironmentModelOptics { self: TargetEnvironmentModel.type =>

  val explicitBase: Lens[TargetEnvironmentModel, Option[Coordinates]] =
    Lens[TargetEnvironmentModel, Option[Coordinates]](_.explicitBase)(a => _.copy(explicitBase = a))

}