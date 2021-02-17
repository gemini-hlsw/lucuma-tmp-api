// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.SequenceModel
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._


object SequenceSchema {

  import StepSchema.StepType
  import syntax.`enum`._

  implicit val EnumTypeBreakpoint: EnumType[SequenceModel.Breakpoint] =
    EnumType.fromEnumerated[SequenceModel.Breakpoint](
      "Breakpoint",
      "Stopping point in a series of steps"
    )


  def BreakpointStepType[F[_]: Effect, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbRepo[F], SequenceModel.BreakpointStep[D]] =
    ObjectType(
      name        = s"${typePrefix}BreakpointStep",
      description = s"$typePrefix step with potential breakpoint",
      fieldsFn    = () => fields(

        Field(
          name        = "breakpoint",
          fieldType   = EnumTypeBreakpoint,
          description = Some("Whether to pause before the execution of this step"),
          resolve     = _.value.breakpoint
        ),

        Field(
          name        = "step",
          fieldType   = StepType[F, D](typePrefix, dynamicType),
          description = Some("The sequence step itself"),
          resolve     = _.value.step
        )
      )
    )

}
