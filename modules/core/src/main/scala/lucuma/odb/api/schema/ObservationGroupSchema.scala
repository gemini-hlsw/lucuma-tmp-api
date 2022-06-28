// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.core.model.Program
import lucuma.odb.api.model.{ObservationModel, WhereObservationInput}
import lucuma.odb.api.model.query.SizeLimitedResult
import lucuma.odb.api.repo.{ObservationRepo, OdbCtx}
import org.typelevel.log4cats.Logger
import sangria.schema._


object ObservationGroupSchema {

  import context._
  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ArgumentOptionOffsetObservation, ArgumentOptionWhereObservation, ObservationIdType, ObservationSelectResult}
  import ProgramSchema.ArgumentProgramId
  import QuerySchema.{ArgumentOptionLimit, SelectResultType}

  def ObservationGroupType[F[_]: Dispatcher: Async: Logger, A](
    prefix:      String,
    valueName:   String,
    outType:     OutputType[A]
  ): ObjectType[OdbCtx[F], ObservationModel.Group[A]] =

    ObjectType(
      name     = s"${prefix}Group",
      fieldsFn = () => fields(

        Field(
          name        = "observationIds",
          fieldType   = ListType(ObservationIdType),
          description = "IDs of observations that use the same constraints".some,
          resolve     = _.value.observationIds.toList
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationSelectResult[F],
          description = "Observations associated with the common value".some,
          arguments   = List(
            ArgumentIncludeDeleted,
            ArgumentOptionOffsetObservation,
            ArgumentOptionLimit
          ),
          resolve     = c =>
            c.observation(_.selectWhere(
              (o: ObservationModel) => c.value.observationIds(o.id) && (c.includeDeleted || o.existence.isPresent),
              c.arg(ArgumentOptionOffsetObservation),
              c.resultSetLimit
            ))
        ),

        Field(
          name        = valueName,
          fieldType   = outType,
          description = "Commonly held value across the observations".some,
          resolve     = _.value.value
        )

      )
    )

  def groupingField[F[_]: Dispatcher: Async: Logger, A](
    name:        String,
    description: String,
    outType:     OutputType[A],
    lookupAll:   (ObservationRepo[F], Program.Id, Option[WhereObservationInput]) => F[List[ObservationModel.Group[A]]]
  ): Field[OdbCtx[F], Unit] = {

    val groupType =
      ObservationGroupSchema.ObservationGroupType[F, A](
        name.capitalize,
        name,
        outType
      )

    val selectResultType =
      SelectResultType[ObservationModel.Group[A]](name, groupType)

    Field(
      name        = s"${name}Group",
      fieldType   = selectResultType,
      description = description.some,
      arguments   = List(
        ArgumentProgramId,
        ArgumentOptionWhereObservation,
        ArgumentOptionLimit
      ),
      resolve    = c => c.unsafeToFuture {
        lookupAll(c.ctx.odbRepo.observation, c.programId, c.arg(ArgumentOptionWhereObservation)).map { gs =>
          SizeLimitedResult.Select.fromAll(gs, c.arg(ArgumentOptionLimit))
        }
      }
    )
  }

}
