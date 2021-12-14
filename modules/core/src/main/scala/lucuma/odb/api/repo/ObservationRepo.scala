// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Observation, Program, Target}
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.ObservationModel.{BulkEdit, Create, Edit, Group, ObservationEvent}
import lucuma.odb.api.model.{ConstraintSetModel, Event, InputError, InstrumentConfigModel, ObservationModel, PlannedTimeSummaryModel, ScienceRequirements, ScienceRequirementsModel, ValidatedInput}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.targetModel.{TargetEnvironmentModel, TargetModel}
import cats.data.{EitherT, State}
import cats.effect.{Async, Ref}
import cats.implicits._

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
    oid: Observation.Id
  ): F[Option[InstrumentConfigModel]]

  def insert(input: Create): F[ObservationModel]

  def edit(edit: Edit): F[ObservationModel]

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
  ): F[List[Group[ConstraintSetModel]]]

  def groupByScienceRequirements(
    pid:            Program.Id,
    includeDeleted: Boolean
  ): F[List[Group[ScienceRequirements]]]

  def bulkEditTargetEnvironment(
    be: BulkEdit[TargetEnvironmentModel.Edit]
  ): F[List[ObservationModel]]

  def bulkEditConstraintSet(
    be: BulkEdit[ConstraintSetModel.Edit]
  ): F[List[ObservationModel]]

  def bulkEditScienceRequirements(
    be: BulkEdit[ScienceRequirementsModel.Edit]
  ): F[List[ObservationModel]]

}

object ObservationRepo {

  def create[F[_]: Async](
    tablesRef:    Ref[F, Tables],
    eventService: EventService[F]
  ): ObservationRepo[F] =

    new TopLevelRepoBase[F, Observation.Id, ObservationModel](
      tablesRef,
      eventService,
      Tables.lastObservationId,
      Tables.observations,
      (editType, model) => ObservationEvent(_, editType, model)
    ) with ObservationRepo[F]
      with LookupSupport {

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
        oid: Observation.Id
      ): F[Option[InstrumentConfigModel]] =
        tablesRef.get.map { tables =>
          for {
            o <- tables.observations.get(oid)
            c <- o.config
            m <- c.dereference[State[Tables, *], Tables](TableState).runA(tables).value
          } yield m
        }

      override def insert(newObs: Create): F[ObservationModel] = {

        // Create the observation
        def create(s: PlannedTimeSummaryModel): F[ObservationModel] =
          EitherT(
            tablesRef.modify { tables =>
              val (tablesʹ, o) = newObs.create[State[Tables, *], Tables](TableState, s).run(tables).value

              o.fold(
                err => (tables,  InputError.Exception(err).asLeft),
                o   => (tablesʹ, o.asRight)
              )

            }
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
        val update: State[Tables, ValidatedInput[ObservationModel]] =
          for {
            ed      <- edit.editor[State[Tables, *], Tables](TableState)
            initial <- TableState.observation.lookupValidated[State[Tables, *]](edit.observationId)
            edited   = (ed, initial).mapN { (e, i) => e.runS(i).value }
            _       <- edited.fold(
              _ => State.get[Tables].void,
              o => Tables.observations.mod_(obsMap => obsMap + (o.id -> o))

            )
          } yield edited

        for {
          o <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _ <- eventService.publish(ObservationModel.ObservationEvent.updated(o))
        } yield o
      }

      // Targets are different because they exist apart from observations.
      // In other words, there are targets which no observations reference.

      override def groupByTarget(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[Target.Id]]] =
        tablesRef.get.map { t =>

          val used =
            t.observations
             .values
             .filter(o => (o.programId === pid) && (includeDeleted || o.isPresent))
             .flatMap(o => o.targets.asterism.toList.tupleRight(o.id))
             .groupMap(_._1)(_._2)
             .map { case (a, oids) => Group.from(a, oids) }
             .toList

          val unused =
            (t.targets.keySet -- used.map(_.value))
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
          t <- tablesRef.get
        } yield g.map(_.map(t.targets.apply))


      private def groupBy[A](
        pid:            Program.Id,
        includeDeleted: Boolean
      )(
        f: ObservationModel => A
      ): F[List[Group[A]]] =
        tablesRef.get.map { t =>
          t.observations
           .values
           .filter(o => (o.programId === pid) && (includeDeleted || o.isPresent))
           .map(o => (f(o), o.id))
           .groupMap(_._1)(_._2)
           .map { case (a, oids) => Group.from(a, oids) }
           .toList
           .sortBy(_.observationIds.head)
        }

      override def groupByAsterism(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[SortedSet[Target.Id]]]] =
        groupBy(pid, includeDeleted)(_.targets.asterism)

      override def groupByAsterismInstantiated(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[Seq[TargetModel]]]] =

        for {
          g <- groupByAsterism(pid, includeDeleted)
          t <- tablesRef.get
        } yield g.map(_.map(_.toList.map(t.targets.apply)))

      override def groupByTargetEnvironment(
       pid:            Program.Id,
       includeDeleted: Boolean
     ): F[List[Group[TargetEnvironmentModel]]] =
       groupBy(pid, includeDeleted)(_.targets)

      override def groupByConstraintSet(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[ConstraintSetModel]]] =
        groupBy(pid, includeDeleted)(_.constraintSet)

      override def groupByScienceRequirements(
        pid:            Program.Id,
        includeDeleted: Boolean
      ): F[List[Group[ScienceRequirements]]] =
        groupBy(pid, includeDeleted)(_.scienceRequirements)


      private def selectObservations(
        programId:      Option[Program.Id],
        observationIds: Option[List[Observation.Id]]
      ): State[Tables, ValidatedInput[List[ObservationModel]]] =
        for {
          p   <- programId.traverse(pid => TableState.program.lookupValidated[State[Tables, *]](pid))
          all <- p.traverse(_.traverse(p => Tables.observations.st.map(_.values.filter(_.programId === p.id).toList)))
          sel <- observationIds.traverse(oids => TableState.observation.lookupAllValidated[State[Tables, *]](oids))
        } yield {
          val obsList = (all, sel) match {
            case (Some(a), Some(s)) =>
              (a, s).mapN { case (a, s) =>
                val keep = a.map(_.id).toSet ++ s.map(_.id)
                (a ++ s).filter(o => keep(o.id))
              }

            case _                  =>
              (all orElse sel).getOrElse(List.empty[ObservationModel].validNec[InputError])
          }

          obsList.map(_.distinctBy(_.id).sortBy(_.id))
        }

      override def bulkEditTargetEnvironment(
        be: BulkEdit[TargetEnvironmentModel.Edit]
      ): F[List[ObservationModel]] = {

        val update =
          for {
            ini <- selectObservations(be.selectProgram, be.selectObservations)
            ed  <- be.edit.editor[State[Tables, *], Tables](TableState)
            osʹ  = (ini, ed).mapN { (os, e) =>
              os.map(ObservationModel.targetEnvironment.modify(env => e.runS(env).value))
            }
            _   <- osʹ.traverse { os =>
              Tables.observations.mod_(_ ++ os.fproductLeft(_.id))
            }
          } yield osʹ

        for {
          os <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os
      }

      private def bulkEdit(
        initialObsList: State[Tables, ValidatedInput[List[ObservationModel]]],
        editor:         ValidatedInput[ObservationModel => ObservationModel]
      ): F[List[ObservationModel]] = {

        val update: State[Tables, ValidatedInput[List[ObservationModel]]] =
          for {
            initial <- initialObsList
            edited   = (initial, editor).mapN { case (os, ed) => os.map(ed) }
            _       <- edited.traverse { os =>
              Tables.observations.mod_(_ ++ os.fproductLeft(_.id))
            }
          } yield edited

        for {
          os <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os

      }

      override def bulkEditConstraintSet(
        be: BulkEdit[ConstraintSetModel.Edit]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.selectProgram, be.selectObservations),
          be.edit.editor.map { ed =>
            ObservationModel.constraintSet.modify(cs => ed.runS(cs).value)
          }
        )

      override def bulkEditScienceRequirements(
        be: BulkEdit[ScienceRequirementsModel.Edit]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.selectProgram, be.selectObservations),
          be.edit.editor.map { ed =>
            ObservationModel.scienceRequirements.modify(sr => ed.runS(sr).value)
          }
        )

    }
}
