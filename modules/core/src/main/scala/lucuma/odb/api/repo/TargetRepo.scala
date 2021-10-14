// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.State
import lucuma.core.model.{Observation, Program, Target}
import lucuma.core.util.Gid
import lucuma.odb.api.model.ValidatedInput
import lucuma.odb.api.model.ObservationModel.ObservationEvent
import lucuma.odb.api.model.ProgramModel.ProgramEvent
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.targetModel.{TargetEnvironment, _}
import cats.effect.{Async, Ref}
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.collection.immutable.SortedSet

sealed trait TargetRepo[F[_]] {

  def createUnaffiliatedTargetEnvironment(
    id: Program.Id,
    in: CreateTargetEnvironmentInput
  ): F[TargetEnvironmentModel]

  def selectScienceTarget(
    id: Target.Id
  ): F[Option[TargetModel]]

  def unsafeSelectScienceTarget(
    id: Target.Id
  ): F[TargetModel]

  def selectScienceTargetList(
    id: TargetEnvironment.Id
  ): F[List[TargetModel]]

  def selectScienceTargetListForObservation(
    id: Observation.Id
  ): F[List[TargetModel]]

  def selectTargetEnvironment(
    id: TargetEnvironment.Id
  ): F[Option[TargetEnvironmentModel]]

  def unsafeSelectTargetEnvironment(
    id: TargetEnvironment.Id
  ): F[TargetEnvironmentModel]

  def selectTargetEnvironmentForObservation(
    id: Observation.Id
  ): F[Option[TargetEnvironmentModel]]

  def unsafeSelectTargetEnvironmentForObservation(
    id: Observation.Id
  ): F[TargetEnvironmentModel]

  def groupBySingleScienceTarget(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[TargetEnvironmentGroup[CommonTarget]]]

  def groupByScienceTargetList(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[TargetEnvironmentGroup[SortedSet[CommonTarget]]]]

  def groupByTargetEnvironment(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[TargetEnvironmentGroup[CommonTargetEnvironment]]]

  def bulkEditScienceTarget(
    be: BulkEditTargetInput
  ): F[List[TargetListEditResult]]

  def bulkEditScienceTargetList(
    be: BulkEditTargetListInput
  ): F[List[TargetListEditResult]]

  def bulkReplaceScienceTargetList(
    be: BulkReplaceTargetListInput
  ): F[List[TargetListEditResult]]

  def bulkEditTargetEnvironment(
    be: BulkEditTargetEnvironmentInput
  ): F[List[TargetEnvironmentContext]]
}

object TargetRepo {

  def create[F[_]: Async](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  ): TargetRepo[F] =

    new TargetRepo[F] {

      override def createUnaffiliatedTargetEnvironment(
        pid: Program.Id,
        in:  CreateTargetEnvironmentInput
      ): F[TargetEnvironmentModel] = {
        val update = tablesRef.modify { t =>
          val (tʹ, v) = in.createUnaffiliated[State[Tables, *], Tables](TableState, pid).run(t).value
          (v.fold(_  => t, _  => tʹ), v.tupleRight(t.programs.get(pid)))
        }

        for {
          tup <- update.flatMap(_.liftTo[F])
          (env, prg) = tup
          _ <- prg.traverse_ { p => eventService.publish(ProgramEvent.updated(p)) }
          _ <- eventService.publish(TargetEnvironmentEvent.created(env))
        } yield env
      }

      override def selectScienceTarget(
        id: Target.Id
      ): F[Option[TargetModel]] =
        tablesRef.get.map(_.targets.get(id))

      private def unsafeSelect[I: Gid, A](
        id: I
      )(
        f:  I => F[Option[A]]
      ): F[A] =
        f(id).flatMap {
          case None    => ExecutionException.missingReference[F,I,A](id)
          case Some(a) => a.pure[F]
        }

      override def unsafeSelectScienceTarget(
        id: Target.Id
      ): F[TargetModel] =
        unsafeSelect(id)(selectScienceTarget)

      override def selectScienceTargetList(
        id: TargetEnvironment.Id
      ): F[List[TargetModel]] =
        tablesRef.get.map(_.targets.values.filter(_.targetEnvironmentId === id).toList)

      override def selectScienceTargetListForObservation(
        id: Observation.Id
      ): F[List[TargetModel]] =
        for {
          e  <- selectTargetEnvironmentForObservation(id)
          ts <- e.map(_.id).traverse(selectScienceTargetList)
        } yield ts.toList.flatten

      override def selectTargetEnvironment(
        id: TargetEnvironment.Id
      ): F[Option[TargetEnvironmentModel]] =
        tablesRef.get.map(_.targetEnvironments.get(id))

      override def unsafeSelectTargetEnvironment(
        id: TargetEnvironment.Id
      ): F[TargetEnvironmentModel] =
        unsafeSelect(id)(selectTargetEnvironment)

      override def selectTargetEnvironmentForObservation(
        id: Observation.Id
      ): F[Option[TargetEnvironmentModel]] =
        tablesRef.get.map(_.targetEnvironments.values.find(_.observationId.contains(id)))

      override def unsafeSelectTargetEnvironmentForObservation(
        id: Observation.Id
      ): F[TargetEnvironmentModel] =
        selectTargetEnvironmentForObservation(id).flatMap {
          case None    => ExecutionException(s"Couldn't find target environment for observation ${Gid[Observation.Id].show(id)}").raiseError[F, TargetEnvironmentModel]
          case Some(e) => e.pure[F]
        }

      private def filteredTargetEnvironments(
        tables:         Tables,
        pid:            Program.Id,
        includeDeleted: Boolean
      ): Map[TargetEnvironment.Id, TargetEnvironmentModel] = {

        val includeEnv: TargetEnvironmentModel => Boolean = tem =>
          (tem.programId === pid) &&
            (includeDeleted || tem.observationId.forall { oid =>
              tables.observations.get(oid).exists(_.isPresent)
            })

        tables
          .targetEnvironments
          .filter { case (_, tem) => includeEnv(tem) }

      }

      override def groupBySingleScienceTarget(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[TargetEnvironmentGroup[CommonTarget]]] =

        tablesRef.get.map { t =>
          val envs = filteredTargetEnvironments(t, pid, includeDeleted)
          val tms  = t.targets.values.filter(tm => envs.isDefinedAt(tm.targetEnvironmentId))

          tms
            .groupBy(_.target)
            .map { case (t, tms) =>
              TargetEnvironmentGroup.from(
                CommonTarget.from(t, tms.map(_.id)),
                tms.map(_.targetEnvironmentId)
              )
            }
            .toList
            .sortBy(_.targetEnvironmentIds.head)
        }

      // Generates a List of tuples: target environment id and the corresponding
      // science targets (if any) for the environment.
      private def scienceTargets(
        tables:         Tables,
        pid:            Program.Id,
        includeDeleted: Boolean
      ): List[(TargetEnvironment.Id, List[TargetModel])] = {

        val envs     = filteredTargetEnvironments(tables, pid, includeDeleted)
        val targets  = tables.targets.values.filter(tm => envs.isDefinedAt(tm.targetEnvironmentId))

        val nonEmpty =
          targets
            .groupBy(_.targetEnvironmentId)
            .view
            .mapValues(_.toList)

        val empty    =
          (envs.keySet -- nonEmpty.keySet).map(_ -> List.empty[TargetModel])

        (nonEmpty ++ empty).toList

      }

      override def groupByScienceTargetList(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[TargetEnvironmentGroup[SortedSet[CommonTarget]]]] =

        tablesRef.get.map { t =>
          scienceTargets(t, pid, includeDeleted)
            .groupBy(_._2.map(_.target).toSet) // group by Set[Target]
            .toList
            .map { case (_, vs) =>
              val (vids, tms) = vs.unzip
              TargetEnvironmentGroup.from(
                CommonTarget.extractFromTargetModels(tms.flatten),
                vids
              )
            }
            .sortBy(_.targetEnvironmentIds.head)
        }

      override def groupByTargetEnvironment(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[TargetEnvironmentGroup[CommonTargetEnvironment]]] =

        tablesRef.get.map { t =>
          scienceTargets(t, pid, includeDeleted)
            .groupBy { case (vid, targets) =>
              // Grouping by (base, Set[Target])
              (t.targetEnvironments(vid).explicitBase, targets.map(_.target).toSet)
            }
            .map { case (_, scienceTargets) =>
              val (vids, tms) = scienceTargets.unzip

              val commonEnvironment =
                CommonTargetEnvironment(
                  // all have the same coordinates because they were grouped on
                  // coordinates, so pick any
                  vids.headOption.flatMap(t.targetEnvironments.get).flatMap(_.explicitBase),
                  CommonTarget.extractFromTargetModels(tms.flatten),
                  SortedSet.from(vids)
                )

              TargetEnvironmentGroup.from(commonEnvironment, vids)
            }
            .toList
            .sortBy(_.targetEnvironmentIds.head)
        }

      private def bulkEdit[C <: TargetEnvironmentContext](
        edit: State[Tables, ValidatedInput[List[C]]]
      ): F[List[C]] = {

        val update = tablesRef.modify { t =>
          val (tʹ, v) = edit.run(t).value
          (v.fold(_  => t, _  => tʹ), v)
        }

        for {
          c <- update.flatMap(_.liftTo[F])
          _ <- c.traverse_(tec => eventService.publish(ProgramEvent.updated(tec.program)))
          _ <- c.flatMap(_.observation.toList).traverse_(o => eventService.publish(ObservationEvent.updated(o)))
          _ <- c.traverse_(tec => eventService.publish(TargetEnvironmentEvent.updated(tec.targetEnvironment)))
        } yield c

      }

      override def bulkEditScienceTarget(
        be: BulkEditTargetInput
      ): F[List[TargetListEditResult]] =
        bulkEdit(be.edit[State[Tables, *], Tables](TableState))

      override def bulkEditScienceTargetList(
        be: BulkEditTargetListInput
      ): F[List[TargetListEditResult]] =
        bulkEdit(be.edit[State[Tables, *], Tables](TableState))

      override def bulkReplaceScienceTargetList(
        be: BulkReplaceTargetListInput
      ): F[List[TargetListEditResult]] =
        bulkEdit(be.replace[State[Tables, *], Tables](TableState))

      override def bulkEditTargetEnvironment(
        be: BulkEditTargetEnvironmentInput
      ): F[List[TargetEnvironmentContext]] =
        bulkEdit(be.edit[State[Tables, *], Tables](TableState))

    }

}