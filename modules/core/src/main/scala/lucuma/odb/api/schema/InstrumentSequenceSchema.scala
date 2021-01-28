// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ManualSequence
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import sangria.schema._


object InstrumentSequenceSchema {

  import StepSchema._

  def InstrumentSequenceType[F[_]: Effect, S, D](
    typePrefix:  String,
    description: String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbRepo[F], ManualSequence[S, D]] =
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
          fieldType   = ListType(StepType[F, D](typePrefix, dynamicType)),
          description = Some("Acquisition sequence"),
          resolve     = _.value.acquisition
        ),

        Field(
          name        = "science",
          fieldType   = ListType(StepType[F, D](typePrefix, dynamicType)),
          description = Some("Science sequence"),
          resolve     = _.value.science
        )

      )
    )


}
