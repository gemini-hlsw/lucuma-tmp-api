// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import lucuma.odb.api.model.Event
import lucuma.odb.api.model.{ObservationModel, ProgramModel}
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.model.{Observation, Program}
import cats.{Applicative, Eq, MonadError}
import cats.effect.std.Dispatcher
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.functor._
import fs2.Stream
import lucuma.odb.api.model.targetModel.{TargetEnvironment, TargetEnvironmentEvent, TargetEnvironmentModel}
import sangria.schema._
import sangria.streaming.SubscriptionStream
import sangria.streaming.SubscriptionStreamLike._
import sangria.streaming.fs2._

import scala.reflect.ClassTag

object SubscriptionType {

  import ObservationSchema.OptionalObservationIdArgument
  import ProgramSchema.OptionalProgramIdArgument
  import TargetSchema.OptionalTargetEnvironmentIdArgument
  import syntax.`enum`._
  import context._

  implicit def observationType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], ObservationModel] =
    ObservationSchema.ObservationType[F]

  implicit def programType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], ProgramModel] =
    ProgramSchema.ProgramType[F]

  implicit def targetEnvironmentType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetEnvironmentModel] =
    TargetSchema.TargetEnvironmentModelType[F]

  implicit val EditTypeEnum: EnumType[Event.EditType] =
    EnumType.fromEnumerated(
      "EditType",
      "Type of edit that triggered an event"
    )

  def EventType[F[_]]: InterfaceType[OdbRepo[F], Event]  =
    InterfaceType[OdbRepo[F], Event](
      "Event",
      "Common fields shared by all events",
      fields[OdbRepo[F], Event](
        Field("id",      LongType, resolve = _.value.id)
      )
    )

  def EditEventType[F[_], T: OutputType, E <: Event.Edit[T]: ClassTag](
    name: String
  ): ObjectType[OdbRepo[F], E] =
    ObjectType[OdbRepo[F], E](
      name        = name,
      description = "Event sent when a new object is created or updated",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], E](EventType)),
      fields      = fields[OdbRepo[F], E](

        Field(
          name        = "editType",
          fieldType   = EditTypeEnum,
          description = Some("Type of edit"),
          resolve     = _.value.editType
        ),

        Field(
          name        = "value",
          fieldType   = implicitly[OutputType[T]],
          description = Some("Edited object"),
          resolve     = _.value.value
        )
      )
    )

  def subscriptionField[F[_]: Dispatcher: Async, E <: Event](
    fieldName:   String,
    description: String,
    tpe:         ObjectType[OdbRepo[F], E],
    arguments:   List[Argument[_]]
  )(
    predicate: (Context[OdbRepo[F], Unit], E) => F[Boolean]
  ): Field[OdbRepo[F], Unit] = {

    implicit val subStream: SubscriptionStream[Stream[F, *]] =
      fs2SubscriptionStream[F](
        implicitly[Dispatcher[F]],
        Async[F]
      )

    Field.subs(
      name        = fieldName,
      description = Some(description),
      fieldType   = tpe,
      arguments   = arguments,
      resolve     = (c: Context[OdbRepo[F], Unit]) => {
        c.ctx
          .eventService
          .subscribe
          .collect {
            case event if tpe.valClass.isAssignableFrom(event.getClass) =>
              event.asInstanceOf[E]
          }
          .evalFilter(e => predicate(c, e))
          .map(event => Action[OdbRepo[F], E](event))
      }
    )
  }

  // Produces a function from (context, event) to F[Boolean] that is true when
  // the event is associated with the program id provided as an argument to the
  // subscription field.
  private def pidMatcher[F[_]: Applicative, E](
    pidsExtractor: (Context[OdbRepo[F], Unit], E) => F[Set[Program.Id]]
  ): (Context[OdbRepo[F], Unit], E) => F[Boolean] = (c, e) =>
    c.optionalProgramId.fold(true.pure[F]) { pid =>
      pidsExtractor(c, e).map(_.contains(pid))
    }

  private def editedField[F[_]: Dispatcher: Async, I: Eq, T: OutputType, E <: Event.Edit[T]: ClassTag](
    name:  String,
    idArg: Argument[Option[I]],
    id:    E => I
  )(
    pids: (Context[OdbRepo[F], Unit], E) => F[Set[Program.Id]]
  ): Field[OdbRepo[F], Unit] =
    subscriptionField[F, E](
      s"${name}Edit",
      s"""
         |Subscribes to an event that is generated whenever a(n) $name is
         |created or updated.  If a(n) $name id is provided, the event is only
         |generated for edits to that particular $name.  If a program id is
         |provided then the event must correspond to a(n) $name referenced by
         |that program.
         |""".stripMargin,
      EditEventType[F, T, E](s"${name.capitalize}Edit"),
      List(idArg, OptionalProgramIdArgument)
    ) { (c, e) =>
      (c.arg(idArg).forall(_ === id(e)).pure[F], pidMatcher(pids).apply(c, e))
        .mapN(_ && _)
    }

  def apply[F[_]: Dispatcher: Async]: ObjectType[OdbRepo[F], Unit] = {

    ObjectType(
      name   = "Subscription",
      fields = fields(

        editedField[F, Observation.Id, ObservationModel, ObservationEvent](
          "observation",
          OptionalObservationIdArgument,
          _.value.id
        ) { (_, e) => Set(e.value.programId).pure[F] },

        editedField[F, TargetEnvironment.Id, TargetEnvironmentModel, TargetEnvironmentEvent](
          "targetEnvironment",
          OptionalTargetEnvironmentIdArgument,
          _.value.id
        ) { (_, e) => Set(e.value.programId).pure[F] },

        // ProgramEvent handled differently.  It would not make sense to
        // filter on program id twice.
        subscriptionField[F, ProgramEvent](
          "programEdit",
          """
            |Subscribes to an event that is generated whenever a program is created
            |or edited. A particular program id may be provided to limit events to
            |that program.
            |""".stripMargin,
          EditEventType[F, ProgramModel, ProgramEvent]("ProgramEdit"),
          List(OptionalProgramIdArgument)
        ) { (c, e) => c.optionalProgramId.fold(true)(_ === e.value.id).pure[F] }

      )
    )
  }
}
