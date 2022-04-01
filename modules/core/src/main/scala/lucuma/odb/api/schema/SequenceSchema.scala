// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{PlannedTime, Sequence, SequenceModel}
import lucuma.odb.api.repo.OdbCtx
import sangria.schema._

object SequenceSchema {

  import syntax.`enum`._

  import AtomSchema._
  import PlannedTimeSchema._

  implicit val EnumTypeSequenceType: EnumType[SequenceModel.SequenceType] =
    EnumType.fromEnumerated(
      "SequenceType",
      "Type of sequence, acquisition or science"
    )

  def SequenceType[F[_], D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], Sequence[D]] =
    ObjectType(
      name        = s"${typePrefix}Sequence",
      description = s"A series of $typePrefix atoms that comprise the sequence",
      fieldsFn    = () => fields(

        Field(
          name        = "atoms",
          fieldType   = ListType(AtomConcreteType[F, D](typePrefix, dynamicType)),
          description = Some("Sequence atoms"),
          resolve     = _.value.atoms
        ),

        Field(
          name        = "time",
          fieldType   = CategorizedTimeType,
          description = Some("Time required for the full execution of this sequence"),
          resolve     = c => PlannedTime.estimateSequence(c.value)
        )

      )
    )

}
