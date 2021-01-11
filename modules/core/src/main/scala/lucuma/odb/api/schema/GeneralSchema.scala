// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Effect
import lucuma.odb.api.model.{Existence, PlannedTimeSummaryModel}
import lucuma.odb.api.repo.OdbRepo
import sangria.schema._

object GeneralSchema {

  import FiniteDurationSchema._
  import syntax.`enum`._

  implicit val EnumTypeExistence: EnumType[Existence] =
    EnumType.fromEnumerated(
      "Existence",
      "State of being: either Deleted or Present"
    )

  val ArgumentIncludeDeleted: Argument[Boolean] =
    Argument(
      name         = "includeDeleted",
      argumentType = BooleanType,
      description  = "Set to true to include deleted values",
      defaultValue = false
    )

  def PlannedTimeSummaryType[F[_]](implicit F: Effect[F]): ObjectType[OdbRepo[F], PlannedTimeSummaryModel] =
    ObjectType(
      name = "PlannedTimeSummary",
      fieldsFn = () => fields(

        Field(
          name        = "pi",
          fieldType   = DurationType[F],
          description = Some("The portion of planned time that will be charged"),
          resolve     = _.value.piTime
        ),

        Field(
          name        = "uncharged",
          fieldType   = DurationType[F],
          description = Some("The portion of planned time that will not be charged"),
          resolve     = _.value.unchargedTime
        ),

        Field(
          name        = "execution",
          fieldType   = DurationType[F],
          description = Some("The total estimated execution time"),
          resolve     = _.value.executionTime
        )

      )
    )

}
