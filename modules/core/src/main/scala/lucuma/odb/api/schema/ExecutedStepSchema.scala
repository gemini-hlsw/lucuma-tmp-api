// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ExecutedStepModel
//import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.repo.OdbRepo
//import lucuma.odb.api.schema.syntax.scalar._
import cats.effect.Effect
//import lucuma.odb.api.schema.SequenceSchema.AtomType
import sangria.schema._

object ExecutedStepSchema {

  import context._

//  import DatasetSchema.DatasetType
//  import SequenceSchema.AtomIdType
  import StepSchema.{StepIdType, StepInterfaceType}

  def ExecutedStepType[F[_]: Effect](
    typePrefix:  String,
  ): ObjectType[OdbRepo[F], ExecutedStepModel] = {
    ObjectType(
      name        = s"${typePrefix}ExecutedStep",
      description = s"$typePrefix executed step",
      fieldsFn    = () => fields(

        Field(
          name        = "id",
          fieldType   = StepIdType,
          description = Some("Step id"),
          resolve     = _.value.stepId
        ),

        Field(
          name        = "step",
          fieldType   = StepInterfaceType[F],
          description = Some("The executed step itself"),
          resolve     = c => c.step(_.unsafeSelectStep(c.value.stepId))
        )

//        Field(
//          name        = "atom",
//          fieldType   = AtomType()
//        )
//        Field(
//          name        = "datasets",
//          fieldType   = ListType(DatasetType[F]),
//          description = Some("Datasets associated with this step"),
//          resolve     = c =>
//        )


      )
    )
  }

}
