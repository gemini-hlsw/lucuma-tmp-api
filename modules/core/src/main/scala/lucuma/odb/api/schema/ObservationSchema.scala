// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AsterismModel, ObservationModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`.ObsStatus
import lucuma.core.model.Observation
import cats.implicits._
import cats.effect.Effect
import cats.effect.implicits._
import sangria.schema._


object ObservationSchema {

  import AsterismSchema.AsterismType
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
          name        = "asterism",
          fieldType   = OptionType(AsterismType[F]),
          description = Some("The observation's asterism, if any"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => asterism[F](c).toIO.unsafeToFuture()

        ),

        // Selects the observation's asterism's targets.  You can get this via
        // the asterism, but it seems like it will be a common request to get
        // the targets directly.
        Field(
          name        = "targets",
          fieldType   = ListType(TargetType),
          description = Some("The observation's targets, if any"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c =>
            asterism[F](c).flatMap {
              _.fold(F.pure(List.empty[TargetModel])) {
                _.targetIds
                 .iterator
                 .toList
                 .traverse(c.ctx.target.select(_, c.includeDeleted))
                 .map(_.flatMap(_.toList))
              }
            }
            .toIO
            .unsafeToFuture()
        )

      )
    )

  private def asterism[F[_]](
    c: Context[OdbRepo[F], ObservationModel]
  )(implicit F: Effect[F]): F[Option[AsterismModel]] =
    c.value.asterismId.fold(F.pure(Option.empty[AsterismModel])) { aid =>
      c.ctx
       .asterism
       .select(aid, c.includeDeleted)
    }

}
