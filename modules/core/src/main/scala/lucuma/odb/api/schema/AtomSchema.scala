// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.Atom
import lucuma.odb.api.model.{AtomModel, PlannedTime, StepModel}

import cats.syntax.all._
import sangria.schema._

object AtomSchema {

  import PlannedTimeSchema._
  import StepSchema.StepConcreteType

  implicit val AtomIdType: ScalarType[Atom.Id] =
    ObjectIdSchema.idType[Atom.Id](name = "AtomId")

  def AtomInterfaceType[F[_]]: InterfaceType[OdbCtx[F], AtomModel[_]] =
    InterfaceType[OdbCtx[F], AtomModel[_]](
      name          = "Atom",
      description   = "Sequence atom",
      fieldsFn      = () => fields[OdbCtx[F], AtomModel[_]](

        Field(
          name        = "id",
          fieldType   = AtomIdType,
          description = "Atom id".some,
          resolve     = _.value.id
        )

      )
    )

  def AtomConcreteType[F[_], D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], AtomModel[StepModel[D]]] =
    ObjectType(
      name        = s"${typePrefix}Atom",
      description = s"$typePrefix atom, a collection of steps that should be executed in their entirety",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], AtomModel[StepModel[D]]](AtomInterfaceType[F])),
      fieldsFn    = () => fields(

        Field(
          name        = "steps",
          fieldType   = ListType(StepConcreteType[F, D](typePrefix, dynamicType)),
          description = Some("Individual steps that comprise the atom"),
          resolve     = _.value.steps.toList
        ),

        Field(
          name        = "time",
          fieldType   = CategorizedTimeType,
          description = Some("Time estimate for this atom's execution, the sum of each step's time."),
          resolve     = c => PlannedTime.estimateAtom(c.value)
        )
      )
    )

}
