// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.odb.api.model.Event

final case class TargetEnvironmentEvent(
  id:       Long,
  editType: Event.EditType,
  value:    TargetEnvironmentModel
) extends Event.Edit[TargetEnvironmentModel]

object TargetEnvironmentEvent {

  def created(value: TargetEnvironmentModel)(id: Long): TargetEnvironmentEvent =
    TargetEnvironmentEvent(id, Event.EditType.Created, value)

  def updated(value: TargetEnvironmentModel)(id: Long): TargetEnvironmentEvent =
    TargetEnvironmentEvent(id, Event.EditType.Updated, value)

}