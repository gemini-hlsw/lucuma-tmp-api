// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.itc.client.ItcResult
import sangria.schema.{Field, _}

object ItcSchema {

  import RefinedSchema.{PosBigDecimalType, NonNegIntType}
  import TimeSchema.NonNegativeDurationType

  val ItcSuccessType: ObjectType[Any, ItcResult.Success] =
    ObjectType(
      name       = "ItcSuccess",
      fieldsFn   = () => fields(

        Field(
          name      = "exposureTime",
          fieldType = NonNegativeDurationType,
          resolve   = _.value.exposureTime
        ),

        Field(
          name      = "exposures",
          fieldType = NonNegIntType,
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
