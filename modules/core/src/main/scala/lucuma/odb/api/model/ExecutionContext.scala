// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.core.model.Observation

/**
 * Pairs an observation id with an ExecutionConfigModel.
 */
sealed trait ExecutionContext extends Product with Serializable {
  def oid:  Observation.Id
  def exec: ExecutionModel
}

object ExecutionContext {

  final case class GmosNorth(
    oid:  Observation.Id,
    exec: ExecutionModel.GmosNorth
  ) extends ExecutionContext

  final case class GmosSouth(
    oid:  Observation.Id,
    exec: ExecutionModel.GmosSouth
  ) extends ExecutionContext

  implicit val EqExecutionContext: Eq[ExecutionContext] =
    Eq.by { a => (
      a.oid,
      a.exec
    )}

}

