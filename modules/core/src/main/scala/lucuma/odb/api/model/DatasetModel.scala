// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Order
import eu.timepit.refined.types.numeric._
import lucuma.core.model.Observation

final case class DatasetModel(
  stepId:        Step.Id,
  index:         PosInt,
  observationId: Observation.Id,
  filename:      DatasetFilename
)

object DatasetModel {

  implicit val OrderDatasetModel: Order[DatasetModel] =
    Order.by { a => (
      a.stepId,
      a.index.value,
      a.observationId,
      a.filename
    )}

}
