// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.optics.Format
import lucuma.core.syntax.time._
import lucuma.odb.api.model.DurationModel
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.schema.syntax.scalar._
import sangria.schema._

import java.time.{Duration, Instant}

import scala.util.Try

object TimeSchema {

  val InstantFormat: Format[String, Instant] =
    Format.apply[String, Instant](
      s => Try(Instant.parse(s)).toOption,
      _.toString
    )

  implicit val InstantScalar: ScalarType[Instant] =
    ScalarType.fromScalarFormat(
      name         = "Instant",
      description  = "Instant of time in ISO-8601 representation",
      scalarFormat =  ScalarFormat(InstantFormat, "2011-12-03T10:15:30Z")
    )

  val DurationType: ObjectType[Any, Duration] =
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

  val InputObjectTypeDuration: InputObjectType[DurationModel.Input] =
    InputObjectType[DurationModel.Input](
      "DurationInput",
      "Time duration input",
      List(
        InputField("microseconds", OptionInputType(LongType),       "Time duration in µs"),
        InputField("milliseconds", OptionInputType(BigDecimalType), "Time duration in ms"),
        InputField("seconds",      OptionInputType(BigDecimalType), "Time duration in seconds"),
        InputField("minutes",      OptionInputType(BigDecimalType), "Time duration in minutes"),
        InputField("hours",        OptionInputType(BigDecimalType), "Time duration in hours")
      )
    )

}
