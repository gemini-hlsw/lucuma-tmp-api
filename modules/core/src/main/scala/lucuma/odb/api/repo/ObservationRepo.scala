// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.{EitherT, Nested, State, StateT}
import cats.effect.{Async, Ref}
import cats.implicits._
import lucuma.core.model.{Observation, Program, Target}
import lucuma.odb.api.model.ObservationModel.{BulkEdit, Create, Edit, Group, ObservationEvent}
import lucuma.odb.api.model.{ConstraintSetModel, EitherInput, Event, InputError, InstrumentConfigModel, ObservationModel, PlannedTimeSummaryModel, ScienceRequirements, ScienceRequirementsModel, ValidatedInput}
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.targetModel.{EditAsterismInput, TargetEnvironmentModel, TargetModel}

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

  def bulkEditAsterism(
    be: BulkEdit[Seq[EditAsterismInput]]
  ): F[List[ObservationModel]]

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
            initial   <- TableState.observation.lookupValidated[State[Tables, *]](edit.observationId)
            edited     = initial.andThen(o => edit.editor.runS(o).toValidated )
            validated <- edited
                           .traverse(_.validate[State[Tables, *], Tables](TableState))
                           .map(_.andThen(identity))
            _         <- validated.fold(
              _ => State.get[Tables].void,
              o => State.modify[Tables](Tables.observations.modify(_ + (o.id -> o)))
            )
          } yield validated

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

          val allTargetIds =
            t.targets
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
          all <- p.traverse(_.traverse(p => State.inspect[Tables, List[ObservationModel]](_.observations.values.filter(_.programId === p.id).toList)))
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

      // A bulk edit for target environment with a post-observation validation
      private def doBulkEditTargets(
        be: BulkEdit[_],
        ed: StateT[EitherInput, TargetEnvironmentModel, Unit]
      ): F[List[ObservationModel]] = {

        val update =
          for {
            ini  <- selectObservations(be.selectProgram, be.selectObservations)
            osʹ   = ini.andThen(_.traverse(ObservationModel.targetEnvironment.modifyA(env => ed.runS(env).toValidated)))
            nos  <- Nested(osʹ).traverse(_.validate[State[Tables, *], Tables](TableState))
            vos   = nos.value.map(_.sequence).andThen(identity)
            _    <- vos.traverse { os =>
              State.modify[Tables](Tables.observations.modify(_ ++ os.fproductLeft(_.id)))
            }
          } yield vos

        for {
          os <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os

      }

      override def bulkEditTargetEnvironment(
        be: BulkEdit[TargetEnvironmentModel.Edit]
      ): F[List[ObservationModel]] =
        doBulkEditTargets(be, be.edit.editor)

      override def bulkEditAsterism(
        be: BulkEdit[Seq[EditAsterismInput]]
      ): F[List[ObservationModel]] =
        doBulkEditTargets(be, EditAsterismInput.multiEditor(be.edit.toList))

      private def bulkEdit(
        initialObsList: State[Tables, ValidatedInput[List[ObservationModel]]],
        editor:         StateT[EitherInput, ObservationModel, Unit]
      ): F[List[ObservationModel]] = {

        val update: State[Tables, ValidatedInput[List[ObservationModel]]] =
          for {
            initial <- initialObsList
            edited   = initial.andThen(_.traverse(editor.runS).toValidated)
            _       <- edited.traverse { os =>
              State.modify[Tables](Tables.observations.modify(_ ++ os.fproductLeft(_.id)))
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
          ObservationModel.constraintSet.transform(be.edit.editor)
        )

      override def bulkEditScienceRequirements(
        be: BulkEdit[ScienceRequirementsModel.Edit]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.selectProgram, be.selectObservations),
          ObservationModel.scienceRequirements.transform(be.edit.editor)
        )

    }
}
