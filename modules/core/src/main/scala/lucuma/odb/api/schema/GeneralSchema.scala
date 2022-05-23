// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{Existence, PlannedTimeSummaryModel}
import cats.syntax.all._
import sangria.schema._
import sangria.validation.ValueCoercionViolation

import java.util.UUID

import scala.util.Try

object GeneralSchema {

  import syntax.`enum`._
  import TimeSchema._

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

  val UuidViolation: ValueCoercionViolation =
    new ValueCoercionViolation("Expected a valid UUID") {}

  implicit val UuidType: ScalarAlias[UUID, String] =
    ScalarAlias(
      StringType,
      _.toString,
      uuid => Try(UUID.fromString(uuid)).toEither.leftMap(_ => UuidViolation)
    )

  val PlannedTimeSummaryType: ObjectType[Any, PlannedTimeSummaryModel] =
    ObjectType(
      name = "PlannedTimeSummary",
      fieldsFn = () => fields(

        Field(
          name        = "pi",
          fieldType   = DurationType,
          description = Some("The portion of planned time that will be charged"),
          resolve     = _.value.piTime
        ),

        Field(
          name        = "uncharged",
          fieldType   = DurationType,
          description = Some("The portion of planned time that will not be charged"),
          resolve     = _.value.unchargedTime
        ),

        Field(
          name        = "execution",
          fieldType   = DurationType,
          description = Some("The total estimated execution time"),
          resolve     = _.value.executionTime
        )

      )
    )

}
