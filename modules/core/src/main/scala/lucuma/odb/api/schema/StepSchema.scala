// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`.StepType
import lucuma.odb.api.model.StepModel
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import sangria.schema._


object StepSchema {

  import OffsetSchema._
  import syntax.`enum`._

  implicit val EnumTypeStepType: EnumType[StepType] =
    EnumType.fromEnumerated(
      "StepType",
      "Step type"
    )

  def StepType[F[_]: Effect, A](
    typePrefix: String,
    outputType: OutputType[A]
  ): ObjectType[OdbRepo[F], StepModel[A]] =
    ObjectType[OdbRepo[F], StepModel[A]](
      name         = s"${typePrefix}Step",
      description  = "Step (bias, dark, science, etc.)",
      fields[OdbRepo[F], StepModel[A]](

        Field(
          name        = "stepType",
          fieldType   = EnumTypeStepType,
          description = Some("Step type"),
          resolve     = _.value.stepType
        ),

        Field(
          name        = "instrumentConfig",
          fieldType   = outputType,
          description = Some("Instrument configuration"),
          resolve     = _.value.dynamicConfig
        ),

        Field(
          name        = "stepConfig",
          fieldType   = StepConfigType[F],
          description = Some("Step configuration"),
          resolve     = _.value
        )

      )
    )

  def StepConfigType[F[_]: Effect]: InterfaceType[OdbRepo[F], StepModel[_]] =
    InterfaceType[OdbRepo[F], StepModel[_]](
      name         = s"StepConfig",
      description  = "Step (bias, dark, science, etc.)",
      fields[OdbRepo[F], StepModel[_]](

        Field(
          name        = "stepType",
          fieldType   = EnumTypeStepType,
          description = Some("Step type"),
          resolve     = _.value.stepType
        )

      )
    ).withPossibleTypes(() => List(
      PossibleObject[OdbRepo[F], StepModel[_]](BiasStepType[F]),
      PossibleObject[OdbRepo[F], StepModel[_]](DarkStepType[F]),
      PossibleObject[OdbRepo[F], StepModel[_]](GcalStepType[F]),
      PossibleObject[OdbRepo[F], StepModel[_]](ScienceStepType[F])
    ))

  def BiasStepType[F[_]: Effect]: ObjectType[OdbRepo[F], StepModel.Bias[_]] =
    ObjectType[OdbRepo[F], StepModel.Bias[_]](
      name        = "BiasStep",
      description = "Bias calibration step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Bias[_]](StepConfigType[F])),
      fields      = Nil
    )

  def DarkStepType[F[_]: Effect]: ObjectType[OdbRepo[F], StepModel.Dark[_]] =
    ObjectType[OdbRepo[F], StepModel.Dark[_]](
      name        = "DarkStep",
      description = "Dark calibration step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Dark[_]](StepConfigType[F])),
      fields      = Nil
    )

  def GcalStepType[F[_]: Effect]: ObjectType[OdbRepo[F], StepModel.Gcal[_]] =
    ObjectType[OdbRepo[F], StepModel.Gcal[_]](
      name        = "GcalStep",
      description = "GCAL calibration step (flat / arc)",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Gcal[_]](StepConfigType[F])),
      fields      = List(

        Field(
          name        = "gcal",
          fieldType   = GcalSchema.GcalType[F],
          description = Some("GCAL configuration"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig
        )
      )
    )

  def ScienceStepType[F[_]: Effect]: ObjectType[OdbRepo[F], StepModel.Science[_]] =
    ObjectType[OdbRepo[F], StepModel.Science[_]] (
      name        = "ScienceStep",
      description = "Science step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Science[_]](StepConfigType[F])),
      fields      = List(

        Field(
          name        = "offset",
          fieldType   = OffsetType[F],
          description = Some("Offset"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Science[_]]) => ctx.value.offset
        )
      )

    )

}
