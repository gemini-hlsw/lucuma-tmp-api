// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.SequenceModel.Sequence
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import sangria.schema._


object InstrumentSequenceSchema {

  import SequenceSchema._

  def InstrumentSequenceType[F[_]: Effect, S, D](
    typePrefix:  String,
    description: String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbRepo[F], Sequence[S, D]] =
    ObjectType(
      name        = s"${typePrefix}Sequence",
      description = description,
      fieldsFn    = () => fields (

        Field(
          name        = "static",
          fieldType   = staticType,
          description = Some("Static/unchanging configuration"),
          resolve     = _.value.static
        ),

        Field(
          name        = "acquisition",
          fieldType   = ListType(ListType(BreakpointStepType[F, D](typePrefix, dynamicType))),
          description = Some("Acquisition sequence. Each inner list of steps comprise an unsplittable scheduling unit."),
          resolve     = _.value.acquisition.map(_.steps.toList)
        ),

        Field(
          name        = "science",
          fieldType   = ListType(ListType(BreakpointStepType[F, D](typePrefix, dynamicType))),
          description = Some("Science sequence. Each inner list of steps comprise an unsplittable scheduling unit."),
          resolve     = _.value.science.map(_.steps.toList)
        )

      )
    )


}
