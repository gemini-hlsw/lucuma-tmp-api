// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AsterismModel, ObservationModel, TargetModel}
import lucuma.odb.api.repo.{OdbRepo, TableState, Tables}
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.Observation

import cats.{Monad, MonadError}
import cats.data.{OptionT, State}
import cats.effect.std.Dispatcher
import cats.syntax.all._
import sangria.schema._

import scala.collection.immutable.Seq


object ObservationSchema {

  import AsterismSchema.AsterismType
  import ConstraintSetSchema.ConstraintSetType
  import ExecutionSchema.ExecutionType
  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, NonEmptyStringType, PlannedTimeSummaryType}
  import ProgramSchema.ProgramType
  import TargetSchema.TargetType

  import context._
  import syntax.`enum`._

  implicit val ObservationIdType: ScalarType[Observation.Id] =
    ObjectIdSchema.idType[Observation.Id](name = "ObservationId")

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

  def ObservationTargetType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): OutputType[Either[AsterismModel, TargetModel]] =
    UnionType(
      name        = "ObservationTarget",
      description = Some("Either asterism or target"),
      types       = List(AsterismType[F], TargetType[F])
    ).mapValue[Either[AsterismModel, TargetModel]](_.merge)

  def ObservationType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], ObservationModel] =
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
          description = Some("Deleted or Present"),
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
          fieldType   = PlannedTimeSummaryType[F],
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
          name        = "constraintSet",
          fieldType   = ConstraintSetType[F],
          description = Some("The constraint set for the observation"),
          resolve     = c => c.value.constraintSet
        ),

        Field(
          name        = "observationTarget",
          fieldType   = OptionType(ObservationTargetType[F]),
          description = Some("The observation's asterism or target (see also `asterism` and `target` fields)"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.unsafeToFuture {
            for {
              a <- asterism[F](c)
              t <- target[F](c)
            } yield a.map(_.asLeft[TargetModel]) orElse t.map(_.asRight[AsterismModel])
          }
        ),

        Field(
          name        = "asterism",
          fieldType   = OptionType(AsterismType[F]),
          description = Some("The observation's asterism, if a multi-target observation"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.unsafeToFuture(asterism[F](c))

        ),

        Field(
          name        = "target",
          fieldType   = OptionType(TargetType[F]),
          description = Some("The observation's target, if a single-target observation"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.unsafeToFuture(target[F](c))
        ),

        Field(
          name        = "manualConfig",
          fieldType   = OptionType(InstrumentConfigSchema.ConfigType[F]),
          description = Some("Manual instrument configuration"),
          resolve     = c => c.unsafeToFuture {
            c.ctx.tables.get.map { tables =>
              c.value.config.flatMap { icm =>
                icm.dereference[State[Tables, *], Tables](TableState).runA(tables).value
              }
            }
          }
        ),

        Field(
          name        = "execution",
          fieldType   = ExecutionType[F],
          description = Some("Execution sequence and runtime artifacts"),
          resolve     = _.value.id
        )

      )
    )

  private def asterism[F[_]: Monad](
    c: Context[OdbRepo[F], ObservationModel]
  ): F[Option[AsterismModel]] =
    c.value.pointing.fold(Monad[F].pure(Option.empty[AsterismModel])) { e =>
      OptionT(e.swap.toOption.pure[F]).flatMap { aid =>
        OptionT(c.ctx.asterism.select(aid, c.includeDeleted))
      }.value
    }

  private def target[F[_]: Monad](
    c: Context[OdbRepo[F], ObservationModel]
  ): F[Option[TargetModel]] =
    c.value.pointing.fold(Monad[F].pure(Option.empty[TargetModel])) { e =>
      OptionT(e.toOption.pure[F]).flatMap { tid =>
        OptionT(c.ctx.target.select(tid, c.includeDeleted))
      }.value
    }

  def ObservationEdgeType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Edge[ObservationModel]] =
    Paging.EdgeType[F, ObservationModel](
      "ObservationEdge",
      "An observation and its cursor",
      ObservationType[F]
    )

  def ObservationConnectionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Connection[ObservationModel]] =
    Paging.ConnectionType[F, ObservationModel](
      "ObservationConnection",
      "Matching observations",
      ObservationType[F],
      ObservationEdgeType[F]
    )

}
