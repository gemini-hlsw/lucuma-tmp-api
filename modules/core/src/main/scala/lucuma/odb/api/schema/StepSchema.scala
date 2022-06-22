// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.enums._
import lucuma.odb.api.model
import lucuma.odb.api.model.{PlannedTime, Step, StepConfig, StepModel}
import lucuma.odb.api.repo.OdbCtx
import sangria.schema._


object StepSchema {

  import OffsetSchema._
  import PlannedTimeSchema.CategorizedTimeType
  import syntax.`enum`._

  implicit val StepIdType: ScalarType[Step.Id] =
    ObjectIdSchema.uidType[Step.Id]("StepId")

  val ArgumentStepId: Argument[Step.Id] =
    Argument(
      name         = "stepId",
      argumentType = StepIdType,
      description  = "Step ID"
    )

  val ArgumentOptionalStepId: Argument[Option[Step.Id]] =
    Argument(
      name         = "stepId",
      argumentType = OptionInputType(StepIdType),
      description  = "Step ID"
    )

  implicit val EnumTypeBreakpoint: EnumType[model.Breakpoint] =
    EnumType.fromEnumerated[model.Breakpoint](
      "Breakpoint",
      "Stopping point in a series of steps"
    )

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

  def StepInterfaceType[F[_]]: InterfaceType[OdbCtx[F], StepModel[_]] =
    InterfaceType[OdbCtx[F], StepModel[_]](
      name        = "Step",
      description = "Sequence step",
      fieldsFn    = () => fields[OdbCtx[F], StepModel[_]](

        Field(
          name        = "id",
          fieldType   = StepIdType,
          description = Some("Step id"),
          resolve     = _.value.id
        ),

        Field(
          name        = "breakpoint",
          fieldType   = EnumTypeBreakpoint,
          description = Some("Whether to pause before the execution of this step"),
          resolve     = _.value.breakpoint
        ),

        Field(
          name        = "stepConfig",
          fieldType   = StepConfigType[F],
          description = Some("The sequence step itself"),
          resolve     = _.value.config
        ),

        Field(
          name        = "time",
          fieldType   = CategorizedTimeType,
          description = Some("Time estimate for this step's execution"),
          resolve     = c => PlannedTime.estimateStep(c.value.config)
        )

      )
    )

  def StepConcreteType[F[_], D](
    typePrefix:  String,
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], StepModel[D]] =
    ObjectType(
      name        = s"${typePrefix}Step",
      description = s"$typePrefix step with potential breakpoint",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], StepModel[D]](StepInterfaceType[F])),
      fieldsFn    = () => fields(

        Field(
          name        = "instrumentConfig",
          fieldType   = dynamicType,
          description = Some("Instrument configuration for this step"),
          resolve     = _.value.config.instrumentConfig
        )

      )
    )

  def StepConfigType[F[_]]: InterfaceType[OdbCtx[F], StepConfig[_]] =
    InterfaceType[OdbCtx[F], StepConfig[_]](
      name         = "StepConfig",
      description  = "Step (bias, dark, gcal, science, etc.)",
      fields[OdbCtx[F], StepConfig[_]](

        Field(
          name        = "stepType",
          fieldType   = EnumTypeStepType,
          description = Some("Step type"),
          resolve     = _.value.stepType
        )

      )
    ).withPossibleTypes(() => List(
      PossibleObject[OdbCtx[F], StepConfig[_]](BiasStepType[F]),
      PossibleObject[OdbCtx[F], StepConfig[_]](DarkStepType[F]),
      PossibleObject[OdbCtx[F], StepConfig[_]](GcalStepType[F]),
      PossibleObject[OdbCtx[F], StepConfig[_]](ScienceStepType[F])
    ))

  def BiasStepType[F[_]]: ObjectType[OdbCtx[F], StepConfig.Bias[_]] =
    ObjectType[OdbCtx[F], StepConfig.Bias[_]](
      name        = "Bias",
      description = "Bias calibration step",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], StepConfig.Bias[_]](StepConfigType[F])),
      fields      = Nil
    )

  def DarkStepType[F[_]]: ObjectType[OdbCtx[F], StepConfig.Dark[_]] =
    ObjectType[OdbCtx[F], StepConfig.Dark[_]](
      name        = "Dark",
      description = "Dark calibration step",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], StepConfig.Dark[_]](StepConfigType[F])),
      fields      = Nil
    )

  def GcalStepType[F[_]]: ObjectType[OdbCtx[F], StepConfig.Gcal[_]] =
    ObjectType[OdbCtx[F], StepConfig.Gcal[_]](
      name        = "Gcal",
      description = "GCAL calibration step (flat / arc)",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], StepConfig.Gcal[_]](StepConfigType[F])),
      fields      = List(

        Field(
          name        = "continuum",
          fieldType   = OptionType(EnumTypeGcalContinuum),
          description = Some("GCAL continuum, present if no arcs are used"),
          resolve     = (ctx: Context[OdbCtx[F], StepConfig.Gcal[_]]) => ctx.value.gcalConfig.lamp.swap.toOption
        ),

        Field(
          name        = "arcs",
          fieldType   = ListType(EnumTypeGcalArc),
          description = Some("GCAL arcs, one or more present if no continuum is used"),
          resolve     = (ctx: Context[OdbCtx[F], StepConfig.Gcal[_]]) => ctx.value.gcalConfig.lamp.toOption.toList.flatMap(_.toList)
        ),

        Field(
          name        = "filter",
          fieldType   = EnumTypeGcalFilter,
          description = Some("GCAL filter"),
          resolve     = (ctx: Context[OdbCtx[F], StepConfig.Gcal[_]]) => ctx.value.gcalConfig.filter
        ),

        Field(
          name        = "diffuser",
          fieldType   = EnumTypeGcalDiffuser,
          description = Some("GCAL diffuser"),
          resolve     = (ctx: Context[OdbCtx[F], StepConfig.Gcal[_]]) => ctx.value.gcalConfig.diffuser
        ),

        Field(
          name        = "shutter",
          fieldType   = EnumTypeGcalShutter,
          description = Some("GCAL shutter"),
          resolve     = (ctx: Context[OdbCtx[F], StepConfig.Gcal[_]]) => ctx.value.gcalConfig.shutter
        )

      )
    )

  def ScienceStepType[F[_]]: ObjectType[OdbCtx[F], StepConfig.Science[_]] =
    ObjectType[OdbCtx[F], StepConfig.Science[_]] (
      name        = "Science",
      description = "Science step",
      interfaces  = List(PossibleInterface.apply[OdbCtx[F], StepConfig.Science[_]](StepConfigType[F])),
      fields      = List(

        Field(
          name        = "offset",
          fieldType   = OffsetType,
          description = Some("Offset"),
          resolve     = (ctx: Context[OdbCtx[F], StepConfig.Science[_]]) => ctx.value.offset
        )
      )

    )

}
