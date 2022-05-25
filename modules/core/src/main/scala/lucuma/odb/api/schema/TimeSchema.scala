// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, NonNegLong}
import lucuma.core.optics.Format
import lucuma.core.syntax.time._
import lucuma.odb.api.model.DurationModel
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.model.time.NonNegDuration
import lucuma.odb.api.schema.syntax.scalar._
import sangria.schema._

import java.time.Instant

import scala.util.Try

object TimeSchema {

  import RefinedSchema._

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

  val NonNegativeDurationType: ObjectType[Any, NonNegDuration] = {
    def toNonNegBigDecimal(nnd: NonNegDuration, scale: Int, f: BigDecimal => BigDecimal = identity): NonNegBigDecimal =
      NonNegBigDecimal.unsafeFrom(f(BigDecimal(nnd.value.toMicros, scale)))

    ObjectType(
      name        = "NonNegDuration",
      description = "Equivalent time amount in several unit options (e.g., 120 seconds or 2 minutes)",
      fieldsFn    = () => fields(

        Field(
          name        = "microseconds",
          fieldType   = NonNegLongType,
          description = "Duration in µs".some,
          resolve     = d => NonNegLong.unsafeFrom(d.value.value.toMicros)
        ),

        Field(
          name        = "milliseconds",
          fieldType   = NonNegBigDecimalType,
          description = "Duration in ms".some,
          resolve     = v => toNonNegBigDecimal(v.value, 3)
        ),

        Field(
          name        = "seconds",
          fieldType   = NonNegBigDecimalType,
          description = "Duration in seconds".some,
          resolve     = v => toNonNegBigDecimal(v.value, 6)
        ),

        Field(
          name        = "minutes",
          fieldType   = NonNegBigDecimalType,
          description = "Duration in minutes".some,
          resolve     = v => toNonNegBigDecimal(v.value, 6, _ / 60)
        ),

        Field(
          name        = "hours",
          fieldType   = NonNegBigDecimalType,
          description = "Duration in hours".some,
          resolve     = v => toNonNegBigDecimal(v.value, 6, _ / 3600)
        )
      )
    )
  }

  val InputObjectTypeNonNegDuration: InputObjectType[DurationModel.NonNegDurationInput] =
    InputObjectType[DurationModel.NonNegDurationInput](
      "NonNegDurationInput",
      "Time duration input",
      List(
        InputField("microseconds", OptionInputType(PosLongType),          "Time duration in µs"),
        InputField("milliseconds", OptionInputType(NonNegBigDecimalType), "Time duration in ms"),
        InputField("seconds",      OptionInputType(NonNegBigDecimalType), "Time duration in seconds"),
        InputField("minutes",      OptionInputType(NonNegBigDecimalType), "Time duration in minutes"),
        InputField("hours",        OptionInputType(NonNegBigDecimalType), "Time duration in hours")
      )
    )

}
