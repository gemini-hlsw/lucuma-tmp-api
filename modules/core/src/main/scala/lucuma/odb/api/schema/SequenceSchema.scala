// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.Atom
import lucuma.odb.api.model.{AtomModel, DereferencedSequence, PlannedTime, StepModel}
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

object SequenceSchema {

  import PlannedTimeSchema._
  import StepSchema.InstrumentStepType

  implicit val AtomIdType: ScalarType[Atom.Id] =
    ObjectIdSchema.idType[Atom.Id](name = "AtomId")

  def AtomType[F[_]: Effect, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbRepo[F], AtomModel[StepModel[D]]] =
    ObjectType(
      name        = s"${typePrefix}Atom",
      description = s"$typePrefix atom, a collection of steps that should be executed in their entirety",
      fieldsFn    = () => fields(

        Field(
          name        = "id",
          fieldType   = AtomIdType,
          description = Some("Atom id"),
          resolve     = _.value.id
        ),

        Field(
          name        = "steps",
          fieldType   = ListType(InstrumentStepType[F, D](typePrefix, dynamicType)),
          description = Some("Individual steps that comprise the atom"),
          resolve     = _.value.steps.toList
        ),

        Field(
          name        = "time",
          fieldType   = CategorizedTimeType[F],
          description = Some("Time estimate for this atom's execution, the sum of each step's time."),
          resolve     = c => PlannedTime.estimateAtom(c.value)
        )
      )
    )

  def SequenceType[F[_]: Effect, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbRepo[F], DereferencedSequence[D]] =
    ObjectType(
      name        = s"${typePrefix}Sequence",
      description = s"A series of $typePrefix atoms that comprise the sequence",
      fieldsFn    = () => fields(

        Field(
          name        = "atoms",
          fieldType   = ListType(AtomType[F, D](typePrefix, dynamicType)),
          description = Some("Sequence atoms"),
          resolve     = _.value.atoms
        ),

        Field(
          name        = "time",
          fieldType   = CategorizedTimeType[F],
          description = Some("Time required for the full execution of this sequence"),
          resolve     = c => PlannedTime.estimateSequence(c.value)
        )

      )
    )

}
