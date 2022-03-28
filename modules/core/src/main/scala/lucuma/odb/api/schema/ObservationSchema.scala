// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.Observation
import lucuma.odb.api.model.ObservationModel
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentType
import org.typelevel.log4cats.Logger
import sangria.schema._

import scala.collection.immutable.Seq


object ObservationSchema {

  import ConstraintSetSchema.ConstraintSetType
  import ScienceConfigurationSchema._
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
          name        = "name",
          fieldType   = OptionType(NonEmptyStringType),
          description = Some("Observation name"),
          resolve     = _.value.name
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
          name        = "plannedTime",
          fieldType   = PlannedTimeSummaryType,
          description = Some("Observation planned time calculation."),
          resolve     = _.value.plannedTimeSummary
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
          name        = "scienceConfiguration",
          fieldType   = OptionType(ScienceConfigurationType),
          description = Some("The science configuration"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.scienceConfiguration
        ),

        Field(
          name        = "itc",
          fieldType   = OptionType(ItcSuccessType),
          description = "ITC execution results".some,
          arguments   = List(UseItcCacheArgument),
          resolve     = c => c.unsafeToFuture {
            for {
              ts <- c.value.targetEnvironment.asterism.toList.traverse(tid => c.ctx.odbRepo.target.unsafeSelectTarget(tid))
              rs <- ts.traverse(t => c.ctx.itcClient.query(c.value, t.target, c.args.arg(UseItcCacheArgument)))

              results   = rs.flatMap(_.toList).traverse(_.itc.toEither)
              maxResult = results.map(_.maxByOption(s => (s.exposureTime.getSeconds, s.exposureTime.getNano)))
                                 .leftMap(e => new Exception(e.msg))

              s  <- maxResult.liftTo[F]
            } yield s
          }
        ),

        Field(
          name        = "manualConfig",
          fieldType   = OptionType(InstrumentConfigSchema.ConfigType[F]),
          description = Some("Manual instrument configuration"),
          resolve     = _.value.config
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
