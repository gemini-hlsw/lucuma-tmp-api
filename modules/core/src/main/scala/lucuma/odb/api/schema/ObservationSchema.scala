// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AsterismModel, ObservationModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`.ObsStatus
import lucuma.core.model.Observation
import cats.data.OptionT
import cats.effect.Effect
import cats.effect.implicits._
import cats.syntax.all._
import sangria.schema._


object ObservationSchema {

  import AsterismSchema.AsterismType
  import ConstraintSetSchema.ConstraintSetType
  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, PlannedTimeSummaryType}
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

  def ObservationTargetType[F[_]: Effect]: OutputType[Either[AsterismModel, TargetModel]] =
    UnionType(
      name        = "ObservationTarget",
      description = Some("Either asterism or target"),
      types       = List(AsterismType[F], TargetType[F])
    ).mapValue[Either[AsterismModel, TargetModel]](_.merge)

  def ObservationType[F[_]](implicit F: Effect[F]): ObjectType[OdbRepo[F], ObservationModel] =
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
          fieldType   = OptionType(StringType),
          description = Some("Observation name"),
          resolve     = _.value.name.map(_.value)
        ),

        Field(
          name        = "status",
          fieldType   = ObsStatusType,
          description = Some("Observation status"),
          resolve     = _.value.status
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
          fieldType   = OptionType(ConstraintSetType[F]),
          description = Some("The constraint set for the observation, if any"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.constraintSet(_.selectForObservation(c.value.id))
        ),

        Field(
          name        = "observationTarget",
          fieldType   = OptionType(ObservationTargetType[F]),
          description = Some("The observation's asterism or target (see also `asterism` and `target` fields)"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => {
            for {
              a <- asterism[F](c)
              t <- target[F](c)
            } yield a.map(_.asLeft[TargetModel]) orElse t.map(_.asRight[AsterismModel])
          }.toIO.unsafeToFuture()
        ),

        Field(
          name        = "asterism",
          fieldType   = OptionType(AsterismType[F]),
          description = Some("The observation's asterism, if a multi-target observation"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => asterism[F](c).toIO.unsafeToFuture()

        ),

        Field(
          name        = "target",
          fieldType   = OptionType(TargetType[F]),
          description = Some("The observation's target, if a single-target observation"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => target[F](c).toIO.unsafeToFuture()
        ),

        Field(
          name        = "config",
          fieldType   = OptionType(ConfigSchema.ConfigType[F]),
          description = Some("Instrument configuration"),
          resolve     = _.value.config
        )

      )
    )

  private def asterism[F[_]](
    c: Context[OdbRepo[F], ObservationModel]
  )(implicit F: Effect[F]): F[Option[AsterismModel]] =
    c.value.targets.fold(F.pure(Option.empty[AsterismModel])) { e =>
      OptionT(e.swap.toOption.pure[F]).flatMap { aid =>
        OptionT(c.ctx.asterism.select(aid, c.includeDeleted))
      }.value
    }

  private def target[F[_]](
    c: Context[OdbRepo[F], ObservationModel]
  )(implicit F: Effect[F]): F[Option[TargetModel]] =
    c.value.targets.fold(F.pure(Option.empty[TargetModel])) { e =>
      OptionT(e.toOption.pure[F]).flatMap { tid =>
        OptionT(c.ctx.target.select(tid, c.includeDeleted))
      }.value
    }

  def ObservationEdgeType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Edge[ObservationModel]] =
    Paging.EdgeType[F, ObservationModel](
      "ObservationEdge",
      "An observation and its cursor",
      ObservationType[F]
    )

  def ObservationConnectionType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Connection[ObservationModel]] =
    Paging.ConnectionType[F, ObservationModel](
      "ObservationConnection",
      "Matching observations",
      ObservationType[F],
      ObservationEdgeType[F]
    )

}
