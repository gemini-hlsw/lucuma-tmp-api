// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.{EitherT, State}
import lucuma.core.model.{Observation, Program, Target}
import lucuma.core.optics.state.all._
import lucuma.core.util.Gid
import lucuma.odb.api.model.targetModel._
import lucuma.odb.api.model.targetModel.TargetModel.TargetEvent
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.validatedinput._
import cats.effect.{Async, Ref}
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import lucuma.odb.api.model.{Event, InputError, ValidatedInput}

import scala.collection.immutable.SortedSet

sealed trait TargetRepo[F[_]] extends TopLevelRepo[F, Target.Id, TargetModel] {

  /**
   * Selects the target with the given id.
   */
  def selectTarget(
    tid:            Target.Id,
    includeDeleted: Boolean = false
  ): F[Option[TargetModel]]

  /**
   * Selects the target with the given id, assuming it exists and throwing
   * otherwise.
   */
  def unsafeSelectTarget(
    tid:            Target.Id,
    includeDeleted: Boolean = false
  ): F[TargetModel]

  /**
   * Selects all targets associated with a program.
   */
  def selectProgramTargets(
    pid:            Program.Id,
    includeDeleted: Boolean = false
  ): F[SortedSet[TargetModel]]

  /**
   * Selects the first (or next) page of targets associated with the program.
   * Like `selectProgramTargets` but with paging.
   */
  def selectPageForProgram(
    pid:            Program.Id,
    count:          Option[Int]       = None,
    afterGid:       Option[Target.Id] = None,
    includeDeleted: Boolean           = false
  ): F[ResultPage[TargetModel]]

  /**
   * Selects the first (or next) page of targets that are associated with the
   * program and which are actually referenced by one or more observations.
   * Like `selectPageForProgram` but only including targets that are used by
   * an observation.
   */
  def selectReferencedPageForProgram(
    pid:            Program.Id,
    count:          Option[Int]       = None,
    afterGid:       Option[Target.Id] = None,
    includeDeleted: Boolean           = false
  ): F[ResultPage[TargetModel]]

  /**
   * Selects the asterism associated with the given observation.
   */
  def selectObservationAsterism(
    oid:            Observation.Id,
    includeDeleted: Boolean = false
  ): F[SortedSet[TargetModel]]

  /**
   * Selects the first (or next) page of targets that are associated with the
   * given observation(s).  Like `selectObservationAsterism` but for multiple
   * observations and with paging.
   */
  def selectPageForObservations(
    oids:           Set[Observation.Id],
    count:          Option[Int]       = None,
    afterGid:       Option[Target.Id] = None,
    includeDeleted: Boolean           = false
  ): F[ResultPage[TargetModel]]

  def selectProgramAsterisms(
    pid:            Program.Id,
    includeDeleted: Boolean = false
  ): F[List[SortedSet[TargetModel]]]

  def selectObservationFirstTarget(
    oid:            Observation.Id,
    includeDeleted: Boolean = false
  ): F[Option[TargetModel]]

  def selectObservationTargetEnvironment(
    oid: Observation.Id
  ): F[Option[TargetEnvironmentModel]]

  def unsafeSelectObservationTargetEnvironment(
    oid: Observation.Id
  ): F[TargetEnvironmentModel]

  def insert(newTarget: TargetModel.Create): F[TargetModel]

  def edit(edit: TargetModel.Edit): F[TargetModel]

//  def groupBySingleScienceTarget(
//    pid:            Program.Id,
//    includeDeleted: Boolean
//  ): F[List[TargetEnvironmentGroup[CommonTarget]]]
//
//  def groupByScienceTargetList(
//    pid:            Program.Id,
//    includeDeleted: Boolean
//  ): F[List[TargetEnvironmentGroup[SortedSet[CommonTarget]]]]
//
//  def groupByTargetEnvironment(
//    pid:            Program.Id,
//    includeDeleted: Boolean
//  ): F[List[TargetEnvironmentGroup[CommonTargetEnvironment]]]
//
//  def bulkEditScienceTargetList(
//    be: BulkEditTargetListInput
//  ): F[List[TargetListEditResult]]
//
//  def bulkReplaceScienceTargetList(
//    be: BulkReplaceTargetListInput
//  ): F[List[TargetListEditResult]]
//
//  def bulkEditTargetEnvironment(
//    be: BulkEditTargetEnvironmentInput
//  ): F[List[TargetEnvironmentContext]]
}

object TargetRepo {

  def create[F[_]: Async](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  ): TargetRepo[F] =

    new TopLevelRepoBase[F, Target.Id, TargetModel](
      tablesRef,
      eventService,
      Tables.lastTargetId,
      Tables.targets,
      (editType, model) => TargetEvent(_, editType, model)
    ) with TargetRepo[F] {

      override def selectTarget(
        tid:            Target.Id,
        includeDeleted: Boolean
      ): F[Option[TargetModel]] =
        select(tid, includeDeleted)

      override def unsafeSelectTarget(
        tid:            Target.Id,
        includeDeleted: Boolean
      ): F[TargetModel] =
        unsafeSelect(tid, includeDeleted)

      override def selectProgramTargets(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[SortedSet[TargetModel]] =
        tablesRef.get.map { tab =>
          SortedSet.from {
            tab.targets.values.filter(t => t.programId === pid && (includeDeleted || t.isPresent))
          }
        }

     override def selectPageForProgram(
       pid:            Program.Id,
       count:          Option[Int]       = None,
       afterGid:       Option[Target.Id] = None,
       includeDeleted: Boolean           = false
     ): F[ResultPage[TargetModel]] =
       selectPageFiltered(count, afterGid, includeDeleted)(_.programId === pid)

     def selectReferencedPageForProgram(
       pid:            Program.Id,
       count:          Option[Int]       = None,
       afterGid:       Option[Target.Id] = None,
       includeDeleted: Boolean           = false
      ): F[ResultPage[TargetModel]] =
        selectPageFromIds(count, afterGid, includeDeleted) { tab =>
          tab
            .observations
            .values
            .filter(o => o.programId === pid && (includeDeleted || o.isPresent))
            .map(_.targets.asterism)
            .reduceOption(_.union(_))
            .getOrElse(SortedSet.empty[Target.Id])
        }

      override def selectPageForObservations(
        oids:           Set[Observation.Id],
        count:          Option[Int]       = None,
        afterGid:       Option[Target.Id] = None,
        includeDeleted: Boolean           = false
      ): F[ResultPage[TargetModel]] =
        selectPageFromIds(count, afterGid, includeDeleted) { tab =>
          oids.map { oid =>
            tab
              .observations
              .get(oid)
              .map(_.targets.asterism)
              .getOrElse(SortedSet.empty[Target.Id])
          }.reduceOption(_.union(_))
           .getOrElse(SortedSet.empty[Target.Id])
        }

      private def unsafeSelect[I: Gid, A](
        id: I
      )(
        f: I => F[Option[A]]
      ): F[A] =
        f(id).flatMap {
          case None    => ExecutionException.missingReference[F, I, A](id)
          case Some(a) => a.pure[F]
        }

      override def selectObservationAsterism(
        oid:            Observation.Id,
        includeDeleted: Boolean
      ): F[SortedSet[TargetModel]] =

        for {
          e   <- selectObservationTargetEnvironment(oid)
          tids = e.map(_.asterism).getOrElse(SortedSet.empty[Target.Id])
          ts  <- tids.toList.traverse(tid => unsafeSelectTarget(tid, includeDeleted))
        } yield SortedSet.from(ts)


      override def selectProgramAsterisms(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[SortedSet[TargetModel]]] =
        tablesRef.get.map { tab =>
          tab
            .observations
            .values
            .filter(o => o.programId === pid && (includeDeleted || o.isPresent))
            .map(_.targets.asterism)
            .toList
            .distinct
            .map(_.map(tab.targets.apply).filter(t => includeDeleted || t.isPresent))
        }

      override def selectObservationFirstTarget(
        oid:            Observation.Id,
        includeDeleted: Boolean
      ): F[Option[TargetModel]] =
        selectObservationAsterism(oid, includeDeleted).map(_.headOption)

      override def selectObservationTargetEnvironment(
        id: Observation.Id
      ): F[Option[TargetEnvironmentModel]] =
        tablesRef.get.map(_.observations.get(id).map(_.targets))

      override def unsafeSelectObservationTargetEnvironment(
        id: Observation.Id
      ): F[TargetEnvironmentModel] =
        unsafeSelect(id)(selectObservationTargetEnvironment)

      override def insert(
        newTarget: TargetModel.Create
      ): F[TargetModel] = {

        val create: F[TargetModel] =
          EitherT(
            tablesRef.modify { tables =>
              val (tablesʹ, t) = newTarget.create[State[Tables, *], Tables](TableState).run(tables).value
              t.fold(
                err => (tables, InputError.Exception(err).asLeft),
                t   => (tablesʹ, t.asRight)
              )
            }
          ).rethrowT

        for {
          t <- create
          _ <- eventService.publish(TargetEvent(_, Event.EditType.Created, t))
        } yield t
      }

      override def edit(
        edit: TargetModel.Edit
      ): F[TargetModel] = {

        val update: State[Tables, ValidatedInput[TargetModel]] =
          for {
            initial <- TableState.target.lookupValidated[State[Tables, *]](edit.targetId)
            edited   = initial.andThen(t => edit.edit(t))
            _       <- edited.fold(
              _ => State.get[Tables].void,
              t => Tables.targets.mod_(m => m + (t.id -> t))
            )
          } yield edited

        for {
          t <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _ <- eventService.publish(TargetEvent.updated(t))
        } yield t

      }



      //      override def selectTargetEnvironment(
      //        id: TargetEnvironment.Id
      //      ): F[Option[TargetEnvironmentModel]] =
      //        tablesRef.get.map(_.targetEnvironments.get(id))
      //
      //      override def unsafeSelectTargetEnvironment(
      //        id: TargetEnvironment.Id
      //      ): F[TargetEnvironmentModel] =
      //        unsafeSelect(id)(selectTargetEnvironment)

      //      override def unsafeSelectTargetEnvironment(
      //        id: Observation.Id
      //      ): F[TargetEnvironmentModel] =
      //        selectTargetEnvironment(id).flatMap {
      //          case None    => ExecutionException(s"Couldn't find target environment for observation ${Gid[Observation.Id].show(id)}").raiseError[F, TargetEnvironmentModel]
      //          case Some(e) => e.pure[F]
      //        }

      //      private def filteredTargetEnvironments(
      //        tables:         Tables,
      //        pid:            Program.Id,
      //        includeDeleted: Boolean
      //      ): Map[TargetEnvironment.Id, TargetEnvironmentModel] = {
      //
      //        val includeEnv: TargetEnvironmentModel => Boolean = tem =>
      //          (tem.programId === pid) &&
      //            (includeDeleted || tem.observationId.forall { oid =>
      //              tables.observations.get(oid).exists(_.isPresent)
      //            })
      //
      //        tables
      //          .targetEnvironments
      //          .filter { case (_, tem) => includeEnv(tem) }
      //
      //      }
      //
      //      override def groupBySingleScienceTarget(
      //        pid:            Program.Id,
      //        includeDeleted: Boolean
      //      ): F[List[TargetEnvironmentGroup[CommonTarget]]] =
      //
      //        tablesRef.get.map { t =>
      //          val envs = filteredTargetEnvironments(t, pid, includeDeleted)
      //          val tms  = t.targets.values.filter(tm => envs.isDefinedAt(tm.targetEnvironmentId))
      //
      //          tms
      //            .groupBy(_.target)
      //            .map { case (t, tms) =>
      //              TargetEnvironmentGroup.from(
      //                CommonTarget.from(t, tms.map(_.id)),
      //                tms.map(_.targetEnvironmentId)
      //              )
      //            }
      //            .toList
      //            .sortBy(_.targetEnvironmentIds.head)
      //        }
      //
      //       Generates a List of tuples: target environment id and the corresponding
      //       science targets (if any) for the environment.
      //      private def scienceTargets(
      //        tables:         Tables,
      //        pid:            Program.Id,
      //        includeDeleted: Boolean
      //      ): List[(TargetEnvironment.Id, List[TargetModel])] = {
      //
      //        val envs     = filteredTargetEnvironments(tables, pid, includeDeleted)
      //        val targets  = tables.targets.values.filter(tm => envs.isDefinedAt(tm.targetEnvironmentId))
      //
      //        val nonEmpty =
      //          targets
      //            .groupBy(_.targetEnvironmentId)
      //            .view
      //            .mapValues(_.toList)
      //
      //        val empty    =
      //          (envs.keySet -- nonEmpty.keySet).map(_ -> List.empty[TargetModel])
      //
      //        (nonEmpty ++ empty).toList
      //
      //      }
      //
      //      override def groupByScienceTargetList(
      //        pid:            Program.Id,
      //        includeDeleted: Boolean
      //      ): F[List[TargetEnvironmentGroup[SortedSet[CommonTarget]]]] =
      //
      //        tablesRef.get.map { t =>
      //          scienceTargets(t, pid, includeDeleted)
      //            .groupBy(_._2.map(_.target).toSet) // group by Set[Target]
      //            .toList
      //            .map { case (_, vs) =>
      //              val (vids, tms) = vs.unzip
      //              TargetEnvironmentGroup.from(
      //                CommonTarget.extractFromTargetModels(tms.flatten),
      //                vids
      //              )
      //            }
      //            .sortBy(_.targetEnvironmentIds.head)
      //        }
      //
      //      override def groupByTargetEnvironment(
      //        pid:            Program.Id,
      //        includeDeleted: Boolean
      //      ): F[List[TargetEnvironmentGroup[CommonTargetEnvironment]]] =
      //
      //        tablesRef.get.map { t =>
      //          scienceTargets(t, pid, includeDeleted)
      //            .groupBy { case (vid, targets) =>
      //               Grouping by (base, Set[Target])
      //              (t.targetEnvironments(vid).explicitBase, targets.map(_.target).toSet)
      //            }
      //            .map { case (_, scienceTargets) =>
      //              val (vids, tms) = scienceTargets.unzip
      //
      //              val commonEnvironment =
      //                CommonTargetEnvironment(
      //                   all have the same coordinates because they were grouped on
      //                   coordinates, so pick any
      //                  vids.headOption.flatMap(t.targetEnvironments.get).flatMap(_.explicitBase),
      //                  CommonTarget.extractFromTargetModels(tms.flatten),
      //                  SortedSet.from(vids)
      //                )
      //
      //              TargetEnvironmentGroup.from(commonEnvironment, vids)
      //            }
      //            .toList
      //            .sortBy(_.targetEnvironmentIds.head)
      //        }
      //
      //      private def bulkEdit[C <: TargetEnvironmentContext](
      //        edit: State[Tables, ValidatedInput[List[C]]]
      //      ): F[List[C]] = {
      //
      //        val update = tablesRef.modify { t =>
      //          val (tʹ, v) = edit.run(t).value
      //          (v.fold(_  => t, _  => tʹ), v)
      //        }
      //
      //        for {
      //          c <- update.flatMap(_.liftTo[F])
      //          _ <- c.traverse_(tec => eventService.publish(ProgramEvent.updated(tec.program)))
      //          _ <- c.flatMap(_.observation.toList).traverse_(o => eventService.publish(ObservationEvent.updated(o)))
      //          _ <- c.traverse_(tec => eventService.publish(TargetEnvironmentEvent.updated(tec.targetEnvironment)))
      //        } yield c
      //
      //      }
      //
      //
      //      override def bulkEditScienceTargetList(
      //        be: BulkEditTargetListInput
      //      ): F[List[TargetListEditResult]] =
      //        bulkEdit(be.edit[State[Tables, *], Tables](TableState))
      //
      //      override def bulkReplaceScienceTargetList(
      //        be: BulkReplaceTargetListInput
      //      ): F[List[TargetListEditResult]] =
      //        bulkEdit(be.replace[State[Tables, *], Tables](TableState))
      //
      //      override def bulkEditTargetEnvironment(
      //        be: BulkEditTargetEnvironmentInput
      //      ): F[List[TargetEnvironmentContext]] =
      //        bulkEdit(be.edit[State[Tables, *], Tables](TableState))
      //
      //    }
    }
}