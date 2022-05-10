// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.{EitherT, StateT}
import cats.effect.{Async, Ref}
import cats.implicits._
import lucuma.core.model.{ConstraintSet, Observation, Program, Target}
import lucuma.odb.api.model.ObservationModel.{BulkEdit, CloneInput, Create, Edit, Group, ObservationEvent}
import lucuma.odb.api.model.{ConstraintSetInput, Database, EitherInput, Event, ExecutionModel, InputError, ObservationModel, PlannedTimeSummaryModel, ScienceRequirements, ScienceRequirementsInput, Table}
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._
import lucuma.odb.api.model.targetModel.{EditAsterismInput, TargetEnvironmentInput, TargetEnvironmentModel, TargetModel}

import scala.collection.immutable.SortedSet

sealed trait ObservationRepo[F[_]] extends TopLevelRepo[F, Observation.Id, ObservationModel] {

  def selectPageForObservations(
    oids:           Set[Observation.Id],
    count:          Option[Int]            = None,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectPageForProgram(
    pid:            Program.Id,
    count:          Option[Int]            = None,
    afterGid:       Option[Observation.Id] = None,
    includeDeleted: Boolean                = false
  ): F[ResultPage[ObservationModel]]

  def selectManualConfig(
    oid:            Observation.Id,
    includeDeleted: Boolean                = false
  ): F[Option[ExecutionModel]]

  def insert(input: Create): F[ObservationModel]

  def edit(edit: Edit): F[ObservationModel]

  def clone(input: CloneInput): F[ObservationModel]

  def groupByTarget(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[Target.Id]]]

  def groupByTargetInstantiated(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[TargetModel]]]

  def groupByAsterism(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[SortedSet[Target.Id]]]]

  def groupByAsterismInstantiated(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[Seq[TargetModel]]]]

  def groupByTargetEnvironment(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[TargetEnvironmentModel]]]

  def groupByConstraintSet(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[ConstraintSet]]]

  def groupByScienceRequirements(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[ScienceRequirements]]]

  def bulkEditAsterism(
    be: BulkEdit[Seq[EditAsterismInput]]
  ): F[List[ObservationModel]]

  def bulkEditTargetEnvironment(
    be: BulkEdit[TargetEnvironmentInput]
  ): F[List[ObservationModel]]

  def bulkEditConstraintSet(
    be: BulkEdit[ConstraintSetInput]
  ): F[List[ObservationModel]]

  def bulkEditScienceRequirements(
    be: BulkEdit[ScienceRequirementsInput]
  ): F[List[ObservationModel]]

}

object ObservationRepo {

  def create[F[_]: Async](
    databaseRef:  Ref[F, Database],
    eventService: EventService[F]
  ): ObservationRepo[F] =

    new TopLevelRepoBase[F, Observation.Id, ObservationModel](
      databaseRef,
      eventService,
      Database.lastObservationId,
      Database.observations.andThen(Table.rows),
      (editType, model) => ObservationEvent(_, editType, model)
    ) with ObservationRepo[F] {

      override def selectPageForObservations(
        oids:           Set[Observation.Id],
        count:          Option[Int],
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { o => oids(o.id) }

      override def selectPageForProgram(
        pid:            Program.Id,
        count:          Option[Int],
        afterGid:       Option[Observation.Id],
        includeDeleted: Boolean
      ): F[ResultPage[ObservationModel]] =

        selectPageFiltered(count, afterGid, includeDeleted) { _.programId === pid }

      override def selectManualConfig(
        oid:            Observation.Id,
        includeDeleted: Boolean
      ): F[Option[ExecutionModel]] =
        select(oid, includeDeleted).map(_.flatMap(_.config))

      override def insert(newObs: Create): F[ObservationModel] = {

        // Create the observation
        def create(s: PlannedTimeSummaryModel): F[ObservationModel] =
          EitherT(
            for {
              st <- newObs.create[F](s)
              o  <- databaseRef.modify { db =>
                st.run(db)
                  .fold(
                    err => (db, InputError.Exception(err).asLeft),
                    _.map(_.asRight)
                  )
              }
            } yield o
          ).rethrowT

        for {
          s <- PlannedTimeSummaryModel.random[F]
          o <- create(s)
          _ <- eventService.publish(ObservationEvent(_, Event.EditType.Created, o))
        } yield o

      }

      override def edit(
        edit: Edit
      ): F[ObservationModel] = {
        val update: StateT[EitherInput, Database, ObservationModel] =
          for {
            initial   <- Database.observation.lookup(edit.observationId)
            edited    <- StateT.liftF(edit.edit.runS(initial))
            validated <- edited.validate
            _         <- Database.observation.update(edit.observationId, validated)
          } yield validated

        for {
          o <- databaseRef.modifyState(update.flipF).flatMap(_.liftTo[F])
          _ <- eventService.publish(ObservationModel.ObservationEvent.updated(o))
        } yield o
      }

      override def clone(input: CloneInput): F[ObservationModel] = {

        val doClone: F[ObservationModel] =
          EitherT(
            for {
              st <- input.clone
              o  <- databaseRef.modify { db =>
                st.run(db)
                  .fold(
                    err => (db, InputError.Exception(err).asLeft),
                    _.map(_.asRight)
                  )
              }
            } yield o
          ).rethrowT

        for {
          o <- doClone
          _ <- eventService.publish(ObservationEvent(_, Event.EditType.Created, o))
        } yield o
      }

      // Targets are different because they exist apart from observations.
      // In other words, there are targets which no observations reference.

      override def groupByTarget(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[Target.Id]]] =
        databaseRef.get.map { db =>

          val used =
            db.observations
              .rows
              .values
              .filter(o => (o.programId === pid) && (includeDeleted || o.isPresent))
              .flatMap(o => o.targetEnvironment.asterism.toList.tupleRight(o.id))
              .groupMap(_._1)(_._2)
              .view
              .filterKeys(tid => includeDeleted || db.targets.rows(tid).isPresent)
              .map { case (a, oids) => Group.from(a, oids) }
              .toList

          val allTargetIds =
            db.targets
              .rows
              .values
              .filter(t => (t.programId === pid) && (includeDeleted || t.isPresent))
              .map(_.id)
              .toSet

          val unused =
            (allTargetIds -- used.map(_.value))
              .toList
              .map(tid => Group(tid, SortedSet.empty[Observation.Id]))

          (unused ::: used).sortBy(_.value)

        }

      override def groupByTargetInstantiated(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[TargetModel]]] =

        for {
          g <- groupByTarget(pid, includeDeleted)
          d <- databaseRef.get
        } yield g.map(_.map(d.targets.rows.apply))


      private def groupBy[A](
        pid:            Program.Id,
        includeDeleted: Boolean
      )(
        f: ObservationModel => A
      ): F[List[Group[A]]] =
        groupByWithTables[A](pid, includeDeleted)((_, o) => f(o))

      private def groupByWithTables[A](
        pid:            Program.Id,
        includeDeleted: Boolean
      )(
        f: (Database, ObservationModel) => A
      ): F[List[Group[A]]] =
        databaseRef.get.map { db =>
          db.observations
            .rows
            .values
            .filter(o => (o.programId === pid) && (includeDeleted || o.isPresent))
            .map(o => (f(db, o), o.id))
            .groupMap(_._1)(_._2)
            .map { case (a, oids) => Group.from(a, oids) }
            .toList
            .sortBy(_.observationIds.head)
        }

      override def groupByAsterism(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[SortedSet[Target.Id]]]] =
        groupByWithTables(pid, includeDeleted) { (t, o) =>
          o.targetEnvironment.asterism.filter(tid => includeDeleted || t.targets.rows(tid).isPresent)
        }

      override def groupByAsterismInstantiated(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[Seq[TargetModel]]]] =

        for {
          g <- groupByAsterism(pid, includeDeleted)
          t <- databaseRef.get
        } yield g.map(_.map(_.toList.map(t.targets.rows.apply)))

      override def groupByTargetEnvironment(
       pid:            Program.Id,
       includeDeleted: Boolean
     ): F[List[Group[TargetEnvironmentModel]]] =
       groupByWithTables(pid, includeDeleted) { (t, o) =>
         TargetEnvironmentModel.asterism.modify {
           _.filter(tid => includeDeleted || t.targets.rows(tid).isPresent)
         }(o.targetEnvironment)
       }

      override def groupByConstraintSet(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[ConstraintSet]]] =
        groupBy(pid, includeDeleted)(_.constraintSet)

      override def groupByScienceRequirements(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[ScienceRequirements]]] =
        groupBy(pid, includeDeleted)(_.scienceRequirements)


      private def selectObservations(
        select: BulkEdit.Select
      ): StateT[EitherInput, Database, List[ObservationModel]] =
        for {
          p   <- select.programId.traverse(Database.program.lookup)
          all <- p.traverse(pm => StateT.inspect[EitherInput, Database, List[ObservationModel]](_.observations.rows.values.filter(_.programId === pm.id).toList))
          sel <- select.observationIds.traverse(Database.observation.lookupAll)
        } yield {
          val obsList = (all, sel) match {
            case (Some(a), Some(s)) =>
              val keep = a.map(_.id).toSet ++ s.map(_.id)
              (a ++ s).filter(o => keep(o.id))

            case _                  =>
              (all orElse sel).toList.flatten
          }

          obsList.distinctBy(_.id).sortBy(_.id)
        }

      // A bulk edit for target environment with a post-observation validation
      private def doBulkEditTargets(
        be: BulkEdit[_],
        ed: StateT[EitherInput, TargetEnvironmentModel, Unit]
      ): F[List[ObservationModel]] = {

        val update =
          for {
            ini  <- selectObservations(be.select)
            osʹ  <- StateT.liftF(ini.traverse(ObservationModel.targetEnvironment.modifyA(ed.runS)))
            vos  <- osʹ.traverse(_.validate)
            _    <- vos.traverse(o => Database.observation.update(o.id, o))
          } yield vos

        for {
          os <- databaseRef.modifyState(update.flipF).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os

      }

      override def bulkEditTargetEnvironment(
        be: BulkEdit[TargetEnvironmentInput]
      ): F[List[ObservationModel]] =
        doBulkEditTargets(be, be.edit.edit)

      override def bulkEditAsterism(
        be: BulkEdit[Seq[EditAsterismInput]]
      ): F[List[ObservationModel]] =
        doBulkEditTargets(be, EditAsterismInput.multiEditor(be.edit.toList))

      private def bulkEdit(
        initialObsList: StateT[EitherInput, Database, List[ObservationModel]],
        editor:         StateT[EitherInput, ObservationModel, Unit]
      ): F[List[ObservationModel]] = {

        val update: StateT[EitherInput, Database, List[ObservationModel]] =
          for {
            initial <- initialObsList
            edited  <- StateT.liftF(initial.traverse(editor.runS))
            _       <- edited.traverse(o => Database.observation.update(o.id, o))
          } yield edited

        for {
          os <- databaseRef.modifyState(update.flipF).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os

      }

      override def bulkEditConstraintSet(
        be: BulkEdit[ConstraintSetInput]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select),
          ObservationModel.constraintSet.transform(be.edit.edit)
        )

      override def bulkEditScienceRequirements(
        be: BulkEdit[ScienceRequirementsInput]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select),
          ObservationModel.scienceRequirements.transform(be.edit.edit)
        )

    }
}
