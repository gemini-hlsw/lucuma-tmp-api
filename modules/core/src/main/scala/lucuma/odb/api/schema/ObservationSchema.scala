// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.Observation
import lucuma.odb.api.model.{ObservationModel, PlannedTimeSummaryModel}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentType
import org.typelevel.log4cats.Logger
import sangria.schema._

import scala.collection.immutable.Seq


object ObservationSchema {

  import ConstraintSetSchema.ConstraintSetType
  import ScienceModeSchema._
  import ExecutionSchema.ExecutionType
  import ItcSchema.ItcSuccessType
  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, NonEmptyStringType, PlannedTimeSummaryType}
  import ProgramSchema.ProgramType
  import ScienceRequirementsSchema.ScienceRequirementsType

  import context._
  import syntax.`enum`._

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
            val targets = c.value.properties.targetEnvironment.asterism.toList.flatTraverse { tid =>
              c.ctx.odbRepo.target.selectTarget(tid).map(_.map(_.target.name.value).toList)
            }
            c.unsafeToFuture(targets.map(_.mkString(", ")))
          }
        ),

        Field(
          name        = "subtitle",
          fieldType   = OptionType(NonEmptyStringType),
          description = Some("User-supplied observation-identifying detail information"),
          resolve     = _.value.properties.subtitle
        ),

        Field(
          name        = "status",
          fieldType   = ObsStatusType,
          description = Some("Observation status"),
          resolve     = _.value.properties.status
        ),

        Field(
          name        = "activeStatus",
          fieldType   = ObsActiveStatusType,
          description = "Observation operational status".some,
          resolve     = _.value.properties.activeStatus
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
          resolve     = c => c.value.properties.constraintSet
        ),

        Field(
          name        = "scienceRequirements",
          fieldType   = ScienceRequirementsType[F],
          description = Some("The top level science requirements"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.properties.scienceRequirements
        ),

        Field(
          name        = "scienceMode",
          fieldType   = OptionType(ScienceModeType),
          description = Some("The science configuration"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.properties.scienceMode
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
          resolve     = _.value.properties.config
        ),

        Field(
          name        = "execution",
          fieldType   = ExecutionType[F],
          description = Some("Execution sequence and runtime artifacts"),
          resolve     = _.value.id
        )

      )
    )

  def ObservationEdgeType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Edge[ObservationModel]] =
    Paging.EdgeType[F, ObservationModel](
      "ObservationEdge",
      "An observation and its cursor",
      ObservationType[F]
    )

  def ObservationConnectionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Connection[ObservationModel]] =
    Paging.ConnectionType[F, ObservationModel](
      "ObservationConnection",
      "Matching observations",
      ObservationType[F],
      ObservationEdgeType[F]
    )

}
