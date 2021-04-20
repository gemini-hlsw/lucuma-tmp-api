// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{Breakpoint, PlannedTime, SequenceModel}
import lucuma.odb.api.model.SequenceModel._
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

object SequenceSchema {

  import FiniteDurationSchema.DurationType
  import PlannedTimeSchema._
  import StepSchema.StepType
  import syntax.`enum`._

  implicit val EnumTypeBreakpoint: EnumType[Breakpoint] =
    EnumType.fromEnumerated[Breakpoint](
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
        ),

        Field(
          name        = "time",
          fieldType   = CategorizedTimeType[F],
          description = Some("Time estimate for this step's execution"),
          resolve     = c => PlannedTime.estimateStep(c.value.step)
        )
      )
    )

  def AtomType[F[_]: Effect, D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbRepo[F], SequenceModel.Atom[D]] =
    ObjectType(
      name        = s"${typePrefix}Atom",
      description = s"$typePrefix atom, a collection of steps that should be executed in their entirety",
      fieldsFn    = () => fields(

        Field(
          name        = "steps",
          fieldType   = ListType(BreakpointStepType[F, D](typePrefix, dynamicType)),
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
  ): ObjectType[OdbRepo[F], Sequence[D]] =
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

  def instrumentConfigFields[F[_]: Effect, I <: InstrumentConfig, S, D](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D],
    config:      I => Config[S, D],
  ): List[Field[OdbRepo[F], I]] =
    List(
      Field(
        name        = "setupTime",
        fieldType   = DurationType[F],
        description = Some("Estimated setup time"),
        resolve     = c => PlannedTime.estimate(c.value).setup.value
      ),

      Field(
        name        = "static",
        fieldType   = staticType,
        description = Some("Static/unchanging configuration"),
        resolve     = c => config(c.value).static
      ),

      Field(
        name        = "acquisition",
        fieldType   = SequenceType[F, D](typePrefix, dynamicType),
        description = Some("Acquisition sequence."),
        resolve     = c => config(c.value).acquisition
      ),

      Field(
        name        = "science",
        fieldType   = SequenceType[F, D](typePrefix, dynamicType),
        description = Some("Science sequence."),
        resolve     = c => config(c.value).science
      )
    )

}
