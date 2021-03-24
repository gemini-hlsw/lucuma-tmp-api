// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.duration.NonNegativeFiniteDuration
import lucuma.odb.api.model.{SequenceModel, StepTimeModel}
import lucuma.odb.api.model.SequenceModel.Sequence
import lucuma.odb.api.model.StepTimeModel.{CategorizedTime, Category}
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._


object SequenceSchema {

  import FiniteDurationSchema.DurationType
  import StepSchema.StepType
  import syntax.`enum`._

  implicit val EnumTypeCategory: EnumType[Category] =
    EnumType.fromEnumerated[Category](
      "StepTimeCategory",
      "Step time category"
    )

  def StepTimeType[F[_]: Effect]: ObjectType[OdbRepo[F], CategorizedTime] = {

    def field(
      name: String,
      desc: String,
      f:    CategorizedTime => NonNegativeFiniteDuration
    ): Field[OdbRepo[F], CategorizedTime] =

      Field(
        name        = name,
        fieldType   = DurationType[F],
        description = Some(desc),
        resolve     = c => f(c.value).value
      )

    ObjectType(
      name        = "StepTime",
      description = "Time required for a step or steps, categorized according to use",
      fieldsFn    = () => fields (
        field("configChange", "Time spent making configuration changes", _.configChange),
        field("exposure", "Time spent collecting photons", _.exposure),
        field("readout", "Time spent reading out the detector", _.readout),
        field("write", "Time spent writing the dataset file", _.write),
        field("total", "Total time across all categories", _.total)
      )
    )
  }

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
        ),

        Field(
          name        = "time",
          fieldType   = StepTimeType[F],
          description = Some("Time estimate for this step's execution"),
          resolve     = c => StepTimeModel.estimate(c.value.step)
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
          fieldType   = StepTimeType[F],
          description = Some("Time estimate for this atom's execution, the sum of each step's time."),
          resolve     = c => (CategorizedTime.Zero :: c.value.steps.map(s => StepTimeModel.estimate(s.step))).reduce
        )
      )
    )

  def SequenceType[F[_]: Effect, S, D](
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
          fieldType   = ListType(AtomType[F, D](typePrefix, dynamicType)),
          description = Some("Acquisition sequence."),
          resolve     = _.value.acquisition
        ),

        Field(
          name        = "science",
          fieldType   = ListType(AtomType[F, D](typePrefix, dynamicType)),
          description = Some("Science sequence."),
          resolve     = _.value.science
        )

      )
    )

}
