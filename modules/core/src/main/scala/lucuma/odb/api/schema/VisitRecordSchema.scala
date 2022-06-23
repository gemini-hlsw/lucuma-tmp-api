// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.odb.api.model.query.WhereEqInput
import lucuma.odb.api.model.{Visit, VisitRecord}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.ExecutionEventSchema.SequenceEventType
import lucuma.odb.api.schema.StepRecordSchema.StepRecordType
import org.typelevel.log4cats.Logger
import sangria.schema._

object VisitRecordSchema {

  import QuerySchema._
  import TimeSchema.{NonNegativeDurationType, InstantScalar}

  implicit val VisitIdType: ScalarType[Visit.Id] =
    ObjectIdSchema.uidType[Visit.Id]("VisitId")

  implicit val InputObjectWhereEqVisitId: InputObjectType[WhereEqInput[Visit.Id]] =
    inputObjectWhereEq("visitId", VisitIdType)

  val ArgumentVisitId: Argument[Visit.Id] =
    Argument(
      name         = "visitId",
      argumentType = VisitIdType,
      description  = "Visit ID"
    )



  def VisitRecordType[F[_]: Dispatcher: Async: Logger, S, D](
    typePrefix:  String,
    staticType:  OutputType[S],
    dynamicType: OutputType[D]
  ): ObjectType[OdbCtx[F], VisitRecord.Output[S, D]] =

    ObjectType(
      name         = s"${typePrefix.capitalize}VisitRecord",
      description  = s"A ${typePrefix.capitalize} visit as recorded by Observe",
      fieldsFn     = () => fields(

        Field(
          name        = "id",
          fieldType   = VisitIdType,
          description = "Visit id".some,
          resolve     = _.value.visitId
        ),

        Field(
          name        = "created",
          fieldType   = InstantScalar,
          description = "Created by Observe at time".some,
          resolve     = _.value.created
        ),

        Field(
          name        = "startTime",
          fieldType   = OptionType(InstantScalar),
          description = "Started at time".some,
          resolve     = _.value.startTime
        ),

        Field(
          name        = "endTime",
          fieldType   = OptionType(InstantScalar),
          description = "Ended at time".some,
          resolve     = _.value.endTime
        ),

        Field(
          name        = "duration",
          fieldType   = NonNegativeDurationType,
          description = "Step duration".some,
          resolve     = _.value.duration
        ),

        Field(
          name        = "staticConfig",
          fieldType   = staticType,
          description = s"$typePrefix static instrument configuration".some,
          resolve     = _.value.staticConfig
        ),

        Field(
          name        = "steps",
          fieldType   = ListType(StepRecordType[F, D](typePrefix, dynamicType)),
          description = s"$typePrefix recorded steps".some,
          resolve     = _.value.steps
        ),

        Field(
          name        = "sequenceEvents",
          fieldType   = ListType(SequenceEventType[F]),
          description = "Sequence events associated with this visit".some,
          resolve    = _.value.sequenceEvents
        )

      )
    )

}
