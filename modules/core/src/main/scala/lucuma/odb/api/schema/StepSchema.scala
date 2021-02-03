// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`._
import lucuma.odb.api.model.StepModel
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._


object StepSchema {

  import FiniteDurationSchema._
  import OffsetSchema._
  import syntax.`enum`._

  implicit val EnumTypeGcalContinuum: EnumType[GcalContinuum] =
    EnumType.fromEnumerated(
      "GcalContinuum",
      "GCAL continuum"
    )

  implicit val EnumTypeGcalArc: EnumType[GcalArc] =
    EnumType.fromEnumerated(
      "GcalArc",
      "GCAL arc"
    )

  implicit val EnumTypeGcalFilter: EnumType[GcalFilter] =
    EnumType.fromEnumerated(
      "GcalFilter",
      "GCAL filter"
    )

  implicit val EnumTypeGcalDiffuser: EnumType[GcalDiffuser] =
    EnumType.fromEnumerated(
      "GcalDiffuser",
      "GCAL diffuser"
    )

  implicit val EnumTypeGcalShutter: EnumType[GcalShutter] =
    EnumType.fromEnumerated(
      "GcalShutter",
      "GCAL shutter"
    )

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
          resolve     = _.value.instrumentConfig
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
      name        = "Bias",
      description = "Bias calibration step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Bias[_]](StepConfigType[F])),
      fields      = Nil
    )

  def DarkStepType[F[_]: Effect]: ObjectType[OdbRepo[F], StepModel.Dark[_]] =
    ObjectType[OdbRepo[F], StepModel.Dark[_]](
      name        = "Dark",
      description = "Dark calibration step",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Dark[_]](StepConfigType[F])),
      fields      = Nil
    )

  def GcalStepType[F[_]: Effect]: ObjectType[OdbRepo[F], StepModel.Gcal[_]] =
    ObjectType[OdbRepo[F], StepModel.Gcal[_]](
      name        = "Gcal",
      description = "GCAL calibration step (flat / arc)",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], StepModel.Gcal[_]](StepConfigType[F])),
      fields      = List(

        Field(
          name        = "continuum",
          fieldType   = OptionType(EnumTypeGcalContinuum),
          description = Some("GCAL continuum, present if no arcs are used"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig.lamp.swap.toOption
        ),

        Field(
          name        = "arcs",
          fieldType   = ListType(EnumTypeGcalArc),
          description = Some("GCAL arcs, one or more present if no continuum is used"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig.lamp.toOption.toList.flatMap(_.toList)
        ),

        Field(
          name        = "filter",
          fieldType   = EnumTypeGcalFilter,
          description = Some("GCAL filter"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig.filter
        ),

        Field(
          name        = "diffuser",
          fieldType   = EnumTypeGcalDiffuser,
          description = Some("GCAL diffuser"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig.diffuser
        ),

        Field(
          name        = "shutter",
          fieldType   = EnumTypeGcalShutter,
          description = Some("GCAL shutter"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig.shutter
        ),

        Field(
          name        = "exposure",
          fieldType   = DurationType[F],
          description = Some("GCAL exposure time"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig.exposureTime
        ),

        Field(
          name        = "coadds",
          fieldType   = IntType,
          description = Some("GCAL coadds"),
          resolve     = (ctx: Context[OdbRepo[F], StepModel.Gcal[_]]) => ctx.value.gcalConfig.coadds.toPosShort.value.toInt
        )
      )
    )

  def ScienceStepType[F[_]: Effect]: ObjectType[OdbRepo[F], StepModel.Science[_]] =
    ObjectType[OdbRepo[F], StepModel.Science[_]] (
      name        = "Science",
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
