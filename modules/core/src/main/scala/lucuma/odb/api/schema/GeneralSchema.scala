// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Effect
import lucuma.odb.api.model.{Existence, PlannedTimeSummaryModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._
import sangria.schema._

import scala.concurrent.duration.FiniteDuration

object GeneralSchema {

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

  def DurationType[F[_]: Effect]: ObjectType[OdbRepo[F], FiniteDuration] =
    ObjectType(
      name     = "Duration",
      fieldsFn = () => fields(

        Field(
          name        = "microseconds",
          fieldType   = LongType,
          description = Some("Duration in µs"),
          resolve     = v => v.value.toMicros
        ),

        Field(
          name        = "milliseconds",
          fieldType   = BigDecimalType,
          description = Some("Duration in ms"),
          resolve     = v => BigDecimal(v.value.toMicros, 3)
        ),

        Field(
          name        = "seconds",
          fieldType   = BigDecimalType,
          description = Some("Duration in seconds"),
          resolve     = v => BigDecimal(v.value.toMicros, 6)
        ),

        Field(
          name        = "minutes",
          fieldType   = BigDecimalType,
          description = Some("Duration in minutes"),
          resolve     = v => BigDecimal(v.value.toMicros, 6) / 60
        ),

        Field(
          name        = "hours",
          fieldType   = BigDecimalType,
          description = Some("Duration in hours"),
          resolve     = v => BigDecimal(v.value.toMicros, 6) / 3600
        )
      )
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
