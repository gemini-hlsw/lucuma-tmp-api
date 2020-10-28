// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{AsterismModel, Event, Existence, InputError, ObservationModel, Sharing, TargetModel}
import lucuma.odb.api.model.AsterismModel.AsterismEvent
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.Event.EditType.{Created, Updated}
import lucuma.odb.api.repo.{LookupSupport, OdbRepo, Tables}
import cats.data.{EitherT, State}
import cats.effect.Effect
import cats.effect.implicits._
import cats.syntax.all._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._
import monocle.state.all._


/**
 *
 */
trait SharingMutation {

  import AsterismSchema.AsterismType
  import ObservationSchema.ObservationIdType
  import TargetSchema.TargetIdType

  import syntax.inputobjecttype._

  val InputObjectTargetObservationLinks: InputObjectType[Sharing.TargetObservationLinks] =
    deriveInputObjectType[Sharing.TargetObservationLinks](
      InputObjectTypeName("TargetObservationLinks"),
      InputObjectTypeDescription("Target and the observations with which they are associated")
    )

  val ArgumentTargetObservationLinks: Argument[Sharing.TargetObservationLinks] =
    InputObjectTargetObservationLinks.argument(
      "input",
      "Target/observation links"
    )

  /**
   * Returns a `State[Tables, ?]` program that will create a `Default` asterism
   * with the given target ids and associate it with the given observation.
   *
   * @param observationId observation that will contain the new asterism
   * @param targetIds targets referenced by the new asterism
   *
   * @return new `Default` asterism paired with the list of resultant events
   *         that should be published
   */
  private def createDefaultAsterism(
    observationId: ObservationModel.Id,
    targetIds:     Set[TargetModel.Id]
  ): State[Tables, (AsterismModel, List[Long => Event])] =
    for {
      i <- Tables.nextAsterismId
      a = AsterismModel.Default(i, Existence.Present, None, targetIds): AsterismModel
      _ <- Tables.asterisms.mod(_ + (i -> a))
      _ <- Tables.observation(observationId).mod(_.map(ObservationModel.asterismId.set(Some(i))))
      o <- Tables.retrieveObservation(observationId)
    } yield (a, List[Long => Event](AsterismEvent(Created, a), ObservationEvent(Updated, o)))

  private def addTargetsToAsterism(
    asterismId: AsterismModel.Id,
    targetIds:  Set[TargetModel.Id]
  ): State[Tables, (AsterismModel, List[Long => Event])] =
    for {
      a   <- Tables.retrieveAsterism(asterismId)
      tup <- Tables.asterism(asterismId).st.transform { (t, _) =>
        if (a.targetIds == targetIds)
          (t, (a, List.empty[Long => Event]))
        else  {
          val a2 = AsterismModel.Default.asterismTargetIds.modify(_ ++ targetIds)(a)
          (Tables.asterisms.modify(_.updated(asterismId, a2))(t), (a2, List[Long => Event](AsterismEvent(Updated, a2))))
        }
      }
    } yield tup

  def shareTargetsWithObservations[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareTargetsWithObservations",
      fieldType = ListType(AsterismType[F]),
      arguments = List(ArgumentTargetObservationLinks),
      resolve   = c => {
        val links   = c.arg(ArgumentTargetObservationLinks)

        val updates = c.ctx.tables.modify { t =>
          val targetList = links.targets.traverse(LookupSupport.lookupTarget(t, _))
          val obsList    = links.observations.traverse(LookupSupport.lookupObservation(t, _))
          (targetList, obsList).mapN { (_, os) =>
            os.traverse { o =>
              val targetIds = links.targets.toSet
              o.asterismId.fold(createDefaultAsterism(o.id, targetIds))(addTargetsToAsterism(_, targetIds))
            }.run(t).value
          }.fold(err => (t, InputError.Exception(err).asLeft), _.map(_.asRight))
        }

        (for {
          us <- EitherT(updates).rethrowT
          (as, es) = us.unzip
          _  <- es.flatten.traverse(c.ctx.eventService.publish)
        } yield as).toIO.unsafeToFuture()
      }
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      shareTargetsWithObservations
    )

}

object SharingMutation extends SharingMutation