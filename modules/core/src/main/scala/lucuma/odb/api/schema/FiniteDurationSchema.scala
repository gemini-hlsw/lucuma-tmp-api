// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Effect
import lucuma.odb.api.repo.OdbRepo
import sangria.schema.{BigDecimalType, Field, LongType, ObjectType, fields}

import scala.concurrent.duration.FiniteDuration

object FiniteDurationSchema {

  def DurationType[F[_]: Effect]: ObjectType[OdbRepo[F], FiniteDuration] =
    ObjectType(
      name        = "Duration",
      description = "Equivalent time amount in several unit options (e.g., 120 seconds or 2 minutes)",
      fieldsFn    = () => fields(

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

}
