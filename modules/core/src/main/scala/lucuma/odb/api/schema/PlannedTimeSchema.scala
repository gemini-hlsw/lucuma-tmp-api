// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.PlannedTime
import lucuma.odb.api.model.PlannedTime.{CategorizedTime, Category}
import lucuma.odb.api.model.time._
import sangria.schema._


object PlannedTimeSchema {

  import TimeSchema.DurationType
  import syntax.`enum`._

  implicit val EnumTypeCategory: EnumType[Category] =
    EnumType.fromEnumerated[Category](
      "StepTimeCategory",
      "Step time category"
    )

  val CategorizedTimeType: ObjectType[Any, CategorizedTime] = {

    def field(
      name: String,
      desc: String,
      f:    CategorizedTime => NonNegativeDuration
    ): Field[Any, CategorizedTime] =

      Field(
        name        = name,
        fieldType   = DurationType,
        description = Some(desc),
        resolve     = c => f(c.value).value
      )

    ObjectType(
      name        = "StepTime",
      description = "Time required for a step or steps, categorized according to use",
      fieldsFn    = () => fields (
        field("configChange", "Time spent making configuration changes", _.configChange),
        field("exposure"    , "Time spent collecting photons",           _.exposure),
        field("readout",      "Time spent reading out the detector",     _.readout),
        field("write",        "Time spent writing the dataset file",     _.write),
        field("total",        "Total time across all categories",        _.total)
      )
    )
  }

  val PlannedTimeType: ObjectType[Any, PlannedTime] =
    ObjectType(
      name        = "PlannedTime",
      description = "Time estimates for executing this configuration",
      fields[Any, PlannedTime](

        Field(
          name        = "setup",
          fieldType   = DurationType,
          description = Some("Estimated setup time"),
          resolve     = _.value.setup.value
        ),

        Field(
          name        = "acquisition",
          fieldType   = ListType(CategorizedTimeType),
          description = Some("Estimated acquisition time for each atom"),
          resolve     = _.value.acquisition
        ),

        Field(
          name        = "acquisitionTotal",
          fieldType   = CategorizedTimeType,
          description = Some("Total estimated acquisition time"),
          resolve     = _.value.acquisitionSum
        ),

        Field(
          name        = "science",
          fieldType   = ListType(CategorizedTimeType),
          description = Some("Estimated science time for each atom"),
          resolve     = _.value.acquisition
        ),

        Field(
          name        = "scienceTotal",
          fieldType   = CategorizedTimeType,
          description = Some("Total estimated science time"),
          resolve     = _.value.scienceSum
        ),

        Field(
          name        = "total",
          fieldType   = DurationType,
          description = Some("Total planned time across acquisition and science"),
          resolve     = _.value.total.value
        )
      )
    )

}
