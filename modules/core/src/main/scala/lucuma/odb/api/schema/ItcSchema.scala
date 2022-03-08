// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.itc.client.ItcResult
import sangria.schema.{Field, _}

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

object ItcSchema {

  import RefinedSchema.PosBigDecimalType
  import TimeSchema.DurationType

  val ItcSuccessType: ObjectType[Any, ItcResult.Success] =
    ObjectType(
      name       = "ItcSuccess",
      fieldsFn   = () => fields(

        Field(
          name      = "exposureTime",
          fieldType = DurationType,
          resolve   = c => FiniteDuration(c.value.exposureTime.toNanos, TimeUnit.NANOSECONDS)
        ),

        Field(
          name      = "exposures",
          fieldType = IntType,
          resolve   = _.value.exposures
        ),

        Field(
          name      = "signalToNoise",
          fieldType = PosBigDecimalType,
          resolve   = _.value.signalToNoise
        )

      )
    )

}
