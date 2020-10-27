// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.data.State
import lucuma.odb.api.model.{AsterismModel, Existence, InputError, ObservationModel, Sharing, TargetModel}
import lucuma.odb.api.repo.{LookupSupport, OdbRepo, Tables}
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

  import GeneralSchema.ArgumentIncludeDeleted

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

  def share[F[_]: Effect]: Field[OdbRepo[F], Unit] = {

    def createDefaultAsterism(
      observationId: ObservationModel.Id,
      targetIds:     Set[TargetModel.Id]
    ): State[Tables, (ObservationModel, AsterismModel)] =
      for {
        i <- Tables.nextAsterismId
        a = AsterismModel.Default(i, Existence.Present, None, targetIds): AsterismModel
        _ <- Tables.asterisms.mod(_ + (i -> a))
        _ <- Tables.observation(observationId).mod(_.map(ObservationModel.asterismId.set(Some(i))))
        o <- Tables.retrieveObservation(observationId)
      } yield (o, a)


    Field(
      name      = "share",
      fieldType = ListType(AsterismType[F]),
      arguments = List(ArgumentTargetObservationLinks, ArgumentIncludeDeleted),
      resolve   = c => {
        val links   = c.arg(ArgumentTargetObservationLinks)

        val updates = c.ctx.tables.modify { t =>
          val targetList = links.targets.traverse(LookupSupport.lookupTarget(t, _))
          val obsList    = links.observations.traverse(LookupSupport.lookupObservation(t, _))
          (targetList, obsList).mapN { (_, os) =>
            val asterisms = os.traverse { o =>
              o.asterismId.fold(
                createDefaultAsterism(o.id, links.targets.toSet).map(_.leftMap(Option(_)))
              )(
                id => Tables.retrieveAsterism(id).tupleLeft(Option.empty[ObservationModel])
              )
            }

            (for {
              as <- asterisms
              _  <- Tables.asterisms.mod { aMap =>
                as.foldLeft(aMap) { case (m, (_, a)) =>
                  m.updated(a.id, AsterismModel.Default.asterismTargetIds.set(links.targets.toSet)(a))
                }
              }
            } yield as).run(t).value
          }.fold(
            err => (t, err.asLeft[List[(Option[ObservationModel], AsterismModel)]]),
            tup => tup.map(_.asRight)
          )
        }.flatMap {
          case Left(err) => Effect[F].raiseError[List[(Option[ObservationModel], AsterismModel)]](InputError.Exception(err))
          case Right(as) => Effect[F].pure(as)
        }

        (for {
          us <- updates
          _  <- us.traverse { case (oo, a) =>
            val es = c.ctx.eventService
            oo.fold(es.publish(AsterismModel.AsterismEditedEvent(a, a))) { o =>
              es.publish(AsterismModel.AsterismCreatedEvent(a)) *>
                es.publish(ObservationModel.ObservationEditedEvent(o, o))
            }
          }
        } yield us.map(_._2)).toIO.unsafeToFuture()
      }
    )
  }

}
