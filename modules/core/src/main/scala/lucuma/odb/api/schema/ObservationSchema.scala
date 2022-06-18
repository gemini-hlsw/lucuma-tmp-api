// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.enums.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.Observation
import lucuma.odb.api.model.query.{WhereEqInput, WhereOrderInput}
import lucuma.odb.api.model.{Existence, ObservationModel, PlannedTimeSummaryModel, WhereObservationInput}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentType
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

import scala.collection.immutable.Seq


object ObservationSchema {

  import ConstraintSetSchema.ConstraintSetType
  import ScienceModeSchema._
  import ExecutionSchema.ExecutionType
  import ItcSchema.ItcSuccessType
  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, InputObjectTypeWhereEqExistence, PlannedTimeSummaryType}
  import PosAngleConstraintSchema._
  import ProgramSchema.{ProgramType,  InputObjectWhereOrderProgramId}
  import RefinedSchema.NonEmptyStringType
  import QuerySchema._
  import ScienceRequirementsSchema.ScienceRequirementsType
  import TimeSchema.InstantScalar

  import context._
  import syntax.`enum`._
  import syntax.inputtype._

  implicit val ObservationIdType: ScalarType[Observation.Id] =
    ObjectIdSchema.gidType[Observation.Id](name = "ObservationId")

  implicit val ObsStatusType: EnumType[ObsStatus] =
    EnumType.fromEnumerated(
      "ObsStatus",
      "Observation status options"
    )

  implicit val ObsActiveStatusType: EnumType[ObsActiveStatus] =
    EnumType.fromEnumerated(
      "ObsActiveStatus",
      "Observation operational/active status options"
    )

  implicit val InputObjectWhereOrderObservationId: InputObjectType[WhereOrderInput[Observation.Id]] =
    inputObjectWhereOrder[Observation.Id]("ObservationId", ObservationIdType)

  implicit val InputObjectWhereOrderObsStatus: InputObjectType[WhereOrderInput[ObsStatus]] =
    inputObjectWhereOrder[ObsStatus]("ObsStatus", ObsStatusType)

  implicit val InputObjectWhereOrderObsActiveStatus: InputObjectType[WhereOrderInput[ObsActiveStatus]] =
    inputObjectWhereOrder[ObsActiveStatus]("ObsActiveStatus", ObsActiveStatusType)

  implicit val InputObjectWhereObservation: InputObjectType[WhereObservationInput] =
    InputObjectType[WhereObservationInput](
      "WhereObservation",
      "Observation filter options.  All specified items must match.",
      () =>
        combinatorFields(InputObjectWhereObservation, "observation") :::
          List(
            InputObjectWhereOrderObservationId.optionField("id", "Matches the observation id."),
            InputObjectWhereOrderProgramId.optionField("programId", "Matches the id of the program associated with this observation."),
            InputObjectWhereOptionString.optionField("subtitle", "Matches the subtitle of the observation."),
            InputObjectWhereOrderObsStatus.optionField("status", "Matches the observation status."),
            InputObjectWhereOrderObsActiveStatus.optionField("activeStatus", "Matches the observation active status."),
            InputField("existence", OptionInputType(InputObjectTypeWhereEqExistence), "By default matching is limited to PRESENT observations.  Use this filter to include DELETED observations as well, for example.", WhereEqInput.EQ(Existence.Present: Existence).some)
          )
    )

  val ObservationIdArgument: Argument[Observation.Id] =
    Argument(
      name         = "observationId",
      argumentType = ObservationIdType,
      description  = "Observation ID"
    )

  val OptionalObservationIdArgument: Argument[Option[Observation.Id]] =
    Argument(
      name         = "observationId",
      argumentType = OptionInputType(ObservationIdType),
      description  = "Observation ID"
    )

  val OptionalListObservationIdArgument: Argument[Option[Seq[Observation.Id]]] =
    Argument(
      name         = "observationIds",
      argumentType = OptionInputType(ListInputType(ObservationIdType)),
      description  = "Observation IDs"
    )

  val UseItcCacheArgument: Argument[Boolean] =
    Argument(
      name         = "useCache",
      argumentType = BooleanType,
      description  = "Whether to use cached results (true) or ignore the cache and make a remote ITC call (false).",
      defaultValue = true
    )

  def ObservationType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ObservationModel] =
    ObjectType(
      name     = "Observation",
      fieldsFn = () => fields(

        Field(
          name        = "id",
          fieldType   = ObservationIdType,
          description = Some("Observation ID"),
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = Some("DELETED or PRESENT"),
          resolve     = _.value.existence
        ),

        Field(
          name        = "title",
          fieldType   = StringType,
          description = Some("Observation title generated from id and targets"),
          resolve     = c => {
            val targets = c.value.targetEnvironment.asterism.toList.flatTraverse { tid =>
              c.ctx.odbRepo.target.selectTarget(tid).map(_.map(_.target.name.value).toList)
            }
            c.unsafeToFuture(targets.map(_.mkString(", ")))
          }
        ),

        Field(
          name        = "subtitle",
          fieldType   = OptionType(NonEmptyStringType),
          description = Some("User-supplied observation-identifying detail information"),
          resolve     = _.value.subtitle
        ),

        Field(
          name        = "status",
          fieldType   = ObsStatusType,
          description = Some("Observation status"),
          resolve     = _.value.status
        ),

        Field(
          name        = "activeStatus",
          fieldType   = ObsActiveStatusType,
          description = "Observation operational status".some,
          resolve     = _.value.activeStatus
        ),

        Field(
          name        = "visualizationTime",
          fieldType   = OptionType(InstantScalar),
          description = "Reference time used by default for visualization and time-dependent calculations (e.g., average parallactic angle)".some,
          resolve     = _.value.visualizationTime
        ),

        Field(
          name        = "posAngleConstraint",
          fieldType   = OptionType(PosAngleConstraintType),
          description = "Position angle constraint, if any.".some,
          resolve     = _.value.posAngleConstraint
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeSummaryType,
          description = Some("Observation planned time calculation."),
          resolve     = c => PlannedTimeSummaryModel.forObservation(c.value)
        ),

        Field(
          name        = "program",
          fieldType   = ProgramType[F],
          description = Some("The program that contains this observation"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.program(_.unsafeSelect(c.value.programId, c.includeDeleted))
        ),

        Field(
          name        = "targetEnvironment",
          fieldType   = TargetEnvironmentType[F],
          description = "The observation's target(s)".some,
          resolve     = c => c.target(_.unsafeSelectObservationTargetEnvironment(c.value.id))
        ),

        Field(
          name        = "constraintSet",
          fieldType   = ConstraintSetType,
          description = Some("The constraint set for the observation"),
          resolve     = c => c.value.constraintSet
        ),

        Field(
          name        = "scienceRequirements",
          fieldType   = ScienceRequirementsType[F],
          description = Some("The top level science requirements"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.scienceRequirements
        ),

        Field(
          name        = "scienceMode",
          fieldType   = OptionType(ScienceModeType),
          description = Some("The science configuration"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.scienceMode
        ),

        Field(
          name        = "itc",
          fieldType   = OptionType(ItcSuccessType),
          description = "ITC execution results".some,
          arguments   = List(UseItcCacheArgument),
          resolve     = c => c.unsafeToFuture {
            c.ctx
             .itcClient
             .query(c.value.id, c.ctx.odbRepo, useCache = c.args.arg(UseItcCacheArgument))
             .map(_.toOption.map(_._2))
          }
        ),

        Field(
          name        = "manualConfig",
          fieldType   = OptionType(ManualConfigSchema.ManualConfigType[F]),
          description = Some("Manual instrument configuration"),
          resolve     = _.value.manualConfig
        ),

        Field(
          name        = "execution",
          fieldType   = ExecutionType[F],
          description = Some("Execution sequence and runtime artifacts"),
          resolve     = _.value.id
        )

      )
    )

}
