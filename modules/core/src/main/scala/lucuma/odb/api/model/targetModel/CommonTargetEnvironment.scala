// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Order
import lucuma.core.math.Coordinates

import scala.collection.immutable.SortedSet

/**
 * `CommonTargetEnvironment` expresses what is the same across a collection of
 * `TargetEnvironmentModel`s (explicit base and the set of targets) along with
 * the ids of individual `TargetEnvironmentModel`s with these properties.
 */
final case class CommonTargetEnvironment(
  explicitBase: Option[Coordinates],
  science:      SortedSet[CommonTarget],
  ids:          SortedSet[TargetEnvironment.Id]
)

object CommonTargetEnvironment {

  implicit val OrderCommonTargetEnvironment: Order[CommonTargetEnvironment] =
    Order.by { a => (
      a.explicitBase,
      a.science,
      a.ids
    )}

}
