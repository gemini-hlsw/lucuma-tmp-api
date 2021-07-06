// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{Existence, PlannedTimeSummaryModel}
import lucuma.odb.api.repo.OdbRepo

import cats.syntax.all._
import eu.timepit.refined.types.all.NonEmptyString
import sangria.schema._
import sangria.validation.ValueCoercionViolation

object GeneralSchema {

  import TimeSchema._
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

  final case object EmptyStringViolation extends ValueCoercionViolation("Expected a non-empty string")

  implicit val NonEmptyStringType: ScalarType[NonEmptyString] =
    ScalarType[NonEmptyString](
      name            =  "NonEmptyString",
      description     = Some("A String value that cannot be empty"),
      coerceUserInput = {
        case s: String  => NonEmptyString.from(s).leftMap(_ => EmptyStringViolation)
        case _          => Left(EmptyStringViolation)
      },
      coerceOutput    = (a, _) => a.value,
      coerceInput     = {
        case sangria.ast.StringValue(s, _, _, _, _) => NonEmptyString.from(s).leftMap(_ => EmptyStringViolation)
        case _                                      => Left(EmptyStringViolation)
      }
    )

  def PlannedTimeSummaryType[F[_]]: ObjectType[OdbRepo[F], PlannedTimeSummaryModel] =
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
