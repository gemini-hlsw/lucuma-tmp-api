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
import lucuma.odb.api.model.targetModel._
import cats.effect.{Async, Ref}
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.implicits.catsKernelOrderingForOrder

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
  ): F[List[TargetEnvironmentGroup[Set[CommonTarget]]]]

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
          val envs    = filteredTargetEnvironments(t, pid, includeDeleted)
          val targets = t.targets.values.filter(tm => envs.isDefinedAt(tm.targetEnvironmentId))
          targets
           .groupMap(_.target)(_.targetEnvironmentId)
           .map { case (t, vids) => TargetEnvironmentGroup(CommonTarget(t), SortedSet.from(vids)) }
           .toList
           .sortBy(_.targetEnvironmentIds.head)
        }

      private def scienceTargets(
        tables:         Tables,
        pid:            Program.Id,
        includeDeleted: Boolean
      ): List[(TargetEnvironment.Id, Set[Target])] = {

        val relevantEnvs = filteredTargetEnvironments(tables, pid, includeDeleted)
        val targets      = tables.targets.values.filter(tm => relevantEnvs.isDefinedAt(tm.targetEnvironmentId))

        val nonEmpty     =
          targets
            .groupMap(_.targetEnvironmentId)(_.target)
            .view
            .mapValues(Set.from)

        val empty        = relevantEnvs.keySet -- nonEmpty.keySet

        (nonEmpty ++ empty.map(_ -> Set.empty[Target])).toList
      }

      override def groupByScienceTargetList(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[TargetEnvironmentGroup[Set[CommonTarget]]]] =

        tablesRef.get.map { t =>
          scienceTargets(t, pid, includeDeleted)
            .groupMap(_._2)(_._1)
            .map { case (ts, vids) => TargetEnvironmentGroup(ts.map(CommonTarget), SortedSet.from(vids)) }
            .toList
            .sortBy(_.targetEnvironmentIds.head)
        }

      override def groupByTargetEnvironment(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[TargetEnvironmentGroup[CommonTargetEnvironment]]] =
        tablesRef.get.map { t =>
          scienceTargets(t, pid, includeDeleted)
            .map { case (vid, targets) =>
              (t.targetEnvironments(vid).toCommon(targets), vid)
            }
            .groupMap(_._1)(_._2)
            .map { case (env, vids) => TargetEnvironmentGroup(env, SortedSet.from(vids)) }
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