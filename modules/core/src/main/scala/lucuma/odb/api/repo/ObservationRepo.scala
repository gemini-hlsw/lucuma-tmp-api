// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.{EitherT, StateT}
import cats.effect.{Async, Ref}
import cats.implicits._
import eu.timepit.refined.types.all.NonNegInt
import lucuma.core.model.{ConstraintSet, Observation, Program, Target}
import lucuma.odb.api.model.ObservationModel.{BulkEdit, CloneInput, CreateInput, EditInput, Group, ObservationEvent}
import lucuma.odb.api.model.query.SelectResult
import lucuma.odb.api.model.{Database, EitherInput, Event, ExecutionModel, InputError, ObservationModel, ScienceMode, ScienceRequirements, Table, WhereObservationInput}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.databasestate._
import lucuma.odb.api.model.syntax.eitherinput._
import lucuma.odb.api.model.targetModel.{EditAsterismPatchInput, TargetEnvironmentModel, TargetModel}

import scala.collection.immutable.SortedSet

sealed trait ObservationRepo[F[_]] extends TopLevelRepo[F, Observation.Id, ObservationModel] {

  def selectForProgram(
    pid:            Program.Id,
    includeDeleted: Boolean                = false,
    offset:         Option[Observation.Id] = None,
    limit:          Option[NonNegInt]      = None
  ): F[SelectResult[ObservationModel]]

  def selectManualConfig(
    oid:            Observation.Id,
    includeDeleted: Boolean                = false
  ): F[Option[ExecutionModel]]

  def insert(input: CreateInput): F[ObservationModel.CreateResult]

  def edit(edit: EditInput): F[ObservationModel.EditResult]

  def clone(input: CloneInput): F[ObservationModel.CloneResult]

  def groupByTarget(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[Target.Id]]]

  def groupByTargetInstantiated(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[TargetModel]]]

  def groupByAsterism(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[SortedSet[Target.Id]]]]

  def groupByAsterismInstantiated(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[Seq[TargetModel]]]]

  def groupByTargetEnvironment(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[TargetEnvironmentModel]]]

  def groupByConstraintSet(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[ConstraintSet]]]

  def groupByScienceMode(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[Option[ScienceMode]]]]

  def groupByScienceRequirements(
    pid:   Program.Id,
    WHERE: Option[WhereObservationInput]
  ): F[List[Group[ScienceRequirements]]]

  def editAsterism(
    be: BulkEdit[Seq[EditAsterismPatchInput]]
  ): F[ObservationModel.EditResult]

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

      override def selectForProgram(
        pid:            Program.Id,
        includeDeleted: Boolean,
        offset:         Option[Observation.Id],
        limit:          Option[NonNegInt]
      ): F[SelectResult[ObservationModel]] =
        selectWhere(
          (a: ObservationModel) => a.programId === pid && (includeDeleted || a.existence.isPresent),
          offset,
          limit
        )

      override def selectManualConfig(
        oid:            Observation.Id,
        includeDeleted: Boolean
      ): F[Option[ExecutionModel]] =
        select(oid, includeDeleted).map(_.flatMap(_.manualConfig))

      override def insert(newObs: CreateInput): F[ObservationModel.CreateResult] = {

        // Create the observation
        val create: F[ObservationModel] =
          EitherT(
            for {
              st <- newObs.create[F]
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
          o <- create
          _ <- eventService.publish(ObservationEvent(_, Event.EditType.Created, o))
        } yield ObservationModel.CreateResult(o)

      }

      override def edit(editInput: ObservationModel.EditInput): F[ObservationModel.EditResult] = {
        val editAndValidate =
          for {
            os  <- editInput.editor
            osʹ <- os.traverse(_.validate)
          } yield osʹ

        for {
          os <- databaseRef.modifyState(editAndValidate.flipF).flatMap(_.liftTo[F])
          _  <- os.traverse(o => eventService.publish(ObservationEvent.updated(o)))
        } yield ObservationModel.EditResult(os)
      }

      override def clone(
        cloneInput: CloneInput
      ): F[ObservationModel.CloneResult] =
        for {
          r <- databaseRef.modifyState(cloneInput.go.flipF).flatMap(_.liftTo[F])
          _ <- eventService.publish(ObservationEvent(_, Event.EditType.Created, r.newObservation))
        } yield r

      // Targets are different because they exist apart from observations.
      // In other words, there are targets which no observations reference.

      override def groupByTarget(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[Target.Id]]] =
        databaseRef.get.map { db =>

          val used =
            db.observations
              .rows
              .values
              .filter(o => (o.programId === pid) && WHERE.getOrElse(WhereObservationInput.MatchPresent).matches(o))
              .flatMap(o => o.targetEnvironment.asterism.toList.tupleRight(o.id))
              .groupMap(_._1)(_._2)
              .view
              .filterKeys(tid => db.targets.rows(tid).isPresent)
              .map { case (a, oids) => Group.from(a, oids) }
              .toList

          val allTargetIds =
            db.targets
              .rows
              .values
              .filter(t => (t.programId === pid) && t.isPresent)
              .map(_.id)
              .toSet

          val unused =
            (allTargetIds -- used.map(_.value))
              .toList
              .map(tid => Group(tid, SortedSet.empty[Observation.Id]))

          (unused ::: used).sortBy(_.value)

        }

      override def groupByTargetInstantiated(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[TargetModel]]] =

        for {
          g <- groupByTarget(pid, WHERE)
          d <- databaseRef.get
        } yield g.map(_.map(d.targets.rows.apply))


      private def groupBy[A](
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      )(
        f: ObservationModel => A
      ): F[List[Group[A]]] =
        groupByWithTables[A](pid, WHERE)((_, o) => f(o))

      private def groupByWithTables[A](
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      )(
        f: (Database, ObservationModel) => A
      ): F[List[Group[A]]] =
        databaseRef.get.map { db =>
          db.observations
            .rows
            .values
            .filter(o => (o.programId === pid) && WHERE.getOrElse(WhereObservationInput.MatchPresent).matches(o))
            .map(o => (f(db, o), o.id))
            .groupMap(_._1)(_._2)
            .map { case (a, oids) => Group.from(a, oids) }
            .toList
            .sortBy(_.observationIds.head)
        }

      override def groupByAsterism(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[SortedSet[Target.Id]]]] =
        groupByWithTables(pid, WHERE) { (t, o) =>
          o.targetEnvironment.asterism.filter(tid => t.targets.rows(tid).isPresent)
        }

      override def groupByAsterismInstantiated(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[Seq[TargetModel]]]] =

        for {
          g <- groupByAsterism(pid, WHERE)
          t <- databaseRef.get
        } yield g.map(_.map(_.toList.map(t.targets.rows.apply)))

      override def groupByTargetEnvironment(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[TargetEnvironmentModel]]] =
       groupByWithTables(pid, WHERE) { (t, o) =>
         TargetEnvironmentModel.asterism.modify {
           _.filter(tid => t.targets.rows(tid).isPresent)
         }(o.targetEnvironment)
       }

      override def groupByConstraintSet(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[ConstraintSet]]] =
        groupBy(pid, WHERE)(_.constraintSet)

      override def groupByScienceMode(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[Option[ScienceMode]]]] =
        groupBy(pid, WHERE)(_.scienceMode)

      override def groupByScienceRequirements(
        pid:   Program.Id,
        WHERE: Option[WhereObservationInput]
      ): F[List[Group[ScienceRequirements]]] =
        groupBy(pid, WHERE)(_.scienceRequirements)

      // A bulk edit for target environment with a post-observation validation
      private def doBulkEditTargets(
        be: BulkEdit[_],
        ed: StateT[EitherInput, TargetEnvironmentModel, Unit]
      ): F[ObservationModel.EditResult] = {

        val update =
          for {
            ini  <- be.select.go
            osʹ  <- StateT.liftF(ini.traverse(ObservationModel.targetEnvironment.modifyA(ed.runS)))
            vos  <- osʹ.traverse(_.validate)
            _    <- vos.traverse(o => Database.observation.update(o.id, o))
          } yield vos

        for {
          os <- databaseRef.modifyState(update.flipF).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield ObservationModel.EditResult(os)

      }

      override def editAsterism(
        be: BulkEdit[Seq[EditAsterismPatchInput]]
      ): F[ObservationModel.EditResult] =
        doBulkEditTargets(be, EditAsterismPatchInput.multiEditor(be.patch.toList))

    }
}
