// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.Event
import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.effect.{ConcurrentEffect, Effect}
import _root_.fs2.Stream
import lucuma.odb.api.model.AsterismModel.{AsterismCreatedEvent, AsterismEditedEvent}
import lucuma.odb.api.model.ObservationModel.{ObservationCreatedEvent, ObservationEditedEvent}
import lucuma.odb.api.model.ProgramModel.{ProgramCreatedEvent, ProgramEditedEvent}
import lucuma.odb.api.model.TargetModel.{TargetCreatedEvent, TargetEditedEvent}
import sangria.schema._
import sangria.streaming.SubscriptionStream
import sangria.streaming.SubscriptionStreamLike._
import sangria.streaming.fs2._

import scala.reflect.ClassTag

object SubscriptionType {

  import ProgramSchema.ProgramIdArgument

  implicit def asterismType[F[_]: Effect]: InterfaceType[OdbRepo[F], AsterismModel] =
    AsterismSchema.AsterismType[F]

  implicit def observationType[F[_]: Effect]: ObjectType[OdbRepo[F], ObservationModel] =
    ObservationSchema.ObservationType[F]

  implicit def programType[F[_]: Effect]: ObjectType[OdbRepo[F], ProgramModel] =
    ProgramSchema.ProgramType[F]

  implicit def targetType[F[_]: Effect]: ObjectType[OdbRepo[F], TargetModel] =
    TargetSchema.TargetType[F]

  def EventType[F[_]: Effect]: InterfaceType[OdbRepo[F], Event]  =
    InterfaceType[OdbRepo[F], Event](
      "Event",
      "Common fields shared by all events",
      fields[OdbRepo[F], Event](
        Field("id",      LongType, resolve = _.value.id)
      )
    )

  def CreatedEventType[F[_]: Effect, T: OutputType, E <: Event.Created[T]: ClassTag](
    name: String
  ): ObjectType[OdbRepo[F], E] =
    ObjectType[OdbRepo[F], E](
      name        = name,
      description = "Event sent when a new object is created",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], E](EventType)),
      fields      = fields[OdbRepo[F], E](
        Field(
          name        = "value",
          fieldType   = implicitly[OutputType[T]],
          description = Some("Newly created object"),
          resolve     = _.value.value
        )
      )
    )

  def EditedEventType[F[_]: Effect, T: OutputType, E <: Event.Edited[T]: ClassTag](
    name: String
  ): ObjectType[OdbRepo[F], E] =
    ObjectType[OdbRepo[F], E](
      name        = name,
      description = "Event sent when an object is edited",
      interfaces  = List(PossibleInterface.apply[OdbRepo[F], E](EventType)),
      fields      = fields[OdbRepo[F], E](
        Field(
          name        = "oldValue",
          fieldType   = implicitly[OutputType[T]],
          description = Some("Previous value of the edited object"),
          resolve     = _.value.oldValue
        ),

        Field(
          name        = "newValue",
          fieldType   = implicitly[OutputType[T]],
          description = Some("Updated value of the edited object"),
          resolve     = _.value.newValue
        )
      )
    )

  def subscriptionField[F[_]: ConcurrentEffect, E <: Event](
    fieldName:   String,
    description: String,
    tpe:         ObjectType[OdbRepo[F], E],
    arguments:   List[Argument[_]]
  )(
    predicate: (Context[OdbRepo[F], Unit], E) => F[Boolean]
  ): Field[OdbRepo[F], Unit] = {

    implicit val subStream: SubscriptionStream[Stream[F, *]] =
      fs2SubscriptionStream[F](ConcurrentEffect[F], scala.concurrent.ExecutionContext.global)

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
  private def pidMatcher[F[_]: ConcurrentEffect, E](
    pidsExtractor: (Context[OdbRepo[F], Unit], E) => F[Set[ProgramModel.Id]]
  ): (Context[OdbRepo[F], Unit], E) => F[Boolean] =
    (c, e) => pidsExtractor(c, e).map(_.contains(c.arg(ProgramIdArgument)))

  private def createdField[F[_]: ConcurrentEffect, T: OutputType, E <: Event.Created[T]: ClassTag](
    name: String
  )(
    pids: (Context[OdbRepo[F], Unit], E) => F[Set[ProgramModel.Id]]
  ): Field[OdbRepo[F], Unit] =
    subscriptionField[F, E](
      s"${name}Created",
      s"Subscribes to an event that is generated whenever a(n) $name associated with the provided program id is created",
      CreatedEventType[F, T, E](s"${name.capitalize}Created"),
      List(ProgramIdArgument)
    )(pidMatcher(pids))

  private def editedField[F[_]: ConcurrentEffect, T: OutputType, E <: Event.Edited[T]: ClassTag](
    name: String
  )(
    pids: (Context[OdbRepo[F], Unit], E) => F[Set[ProgramModel.Id]]
  ): Field[OdbRepo[F], Unit] =
    subscriptionField[F, E](
      s"${name}Edited",
      s"Subscribes to an event that is generated whenever a(n) $name associated with the provided program id is edited",
      EditedEventType[F, T, E](s"${name.capitalize}Edited"),
      List(ProgramIdArgument)
    )(pidMatcher(pids))

  def apply[F[_]: ConcurrentEffect]: ObjectType[OdbRepo[F], Unit] = {
    def programsForAsterism(c: Context[OdbRepo[F], Unit], aid: AsterismModel.Id): F[Set[ProgramModel.Id]] =
      c.ctx.program.selectAllForAsterism(aid).map(_.map(_.id).toSet)

    def programsForTarget(c: Context[OdbRepo[F], Unit], tid: TargetModel.Id): F[Set[ProgramModel.Id]] =
      c.ctx.program.selectAllForTarget(tid).map(_.map(_.id).toSet)

    ObjectType(
      name   = "Subscription",
      fields = fields(
        createdField[F, AsterismModel, AsterismCreatedEvent]("asterism") { (c, e) =>
          programsForAsterism(c, e.value.id)
        },

        createdField[F, ObservationModel, ObservationCreatedEvent]("observation") { (_, e) =>
          Set(e.value.pid).pure[F]
        },

        // ProgramCreatedEvent handled differently for now.  If we reserve
        // program ids ahead of time and use them in creation then it might
        // make sense to pass the id to the subscription.  If not, then we don't
        // know the program id ahead of time.
        subscriptionField[F, ProgramCreatedEvent](
          "programCreated",
          "Subscribes to an event that is generated whenever a program is created",
          CreatedEventType[F, ProgramModel, ProgramCreatedEvent]("ProgramCreated"),
          Nil
        )((_,_) => true.pure[F]),

        createdField[F, TargetModel, TargetCreatedEvent]("target") { (c, e) =>
          programsForTarget(c, e.value.id)
        },

        editedField[F, AsterismModel, AsterismEditedEvent]("asterism") { (c, e) =>
          programsForAsterism(c, e.newValue.id)
        },

        editedField[F, ObservationModel, ObservationEditedEvent]("observation") { (_, e) =>
          Set(e.newValue.pid).pure[F]
        },

        editedField[F, ProgramModel, ProgramEditedEvent]("program") { (_, e) =>
          Set(e.newValue.id).pure[F]
        },

        editedField[F, TargetModel, TargetEditedEvent]("target") { (c, e) =>
          programsForTarget(c, e.newValue.id)
        }
      )
    )
  }
}
