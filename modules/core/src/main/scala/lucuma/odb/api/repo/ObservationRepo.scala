// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Observation, Program, Target}
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.ObservationModel.{BulkEdit, Create, Edit, Group, ObservationEvent, ObservationSelector}
import lucuma.odb.api.model.{ConstraintSetModel, Event, InputError, InstrumentConfigModel, ObservationModel, PlannedTimeSummaryModel, ScienceRequirements, ScienceRequirementsModel, TargetEnvironmentModel, TargetModel, ValidatedInput}
import lucuma.odb.api.model.syntax.validatedinput._
import cats.data.{EitherT, State}
import cats.effect.{Async, Ref}
import cats.implicits._

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


  def groupBySingleScienceTarget(pid: Program.Id): F[List[Group[Target]]]

  def groupByAllScienceTargets(pid: Program.Id): F[List[Group[List[Target]]]]

  def groupByTargetEnvironment(pid: Program.Id): F[List[Group[TargetEnvironmentModel]]]

  def groupByConstraintSet(pid: Program.Id): F[List[Group[ConstraintSetModel]]]

  def groupByScienceRequirements(pid: Program.Id): F[List[Group[ScienceRequirements]]]

  def bulkEditScienceTargetName(
    be: BulkEdit[ObservationSelector, TargetModel.EditName]
  ): F[List[ObservationModel]]

  def bulkEditSiderealScienceTarget(
    be: BulkEdit[ObservationSelector, TargetModel.EditSidereal]
  ): F[List[ObservationModel]]

  def bulkEditAllScienceTargets(
    be: BulkEdit[ObservationSelector, TargetModel.EditTargetList]
  ): F[List[ObservationModel]]

  def bulkEditTargetEnvironment(
    be: BulkEdit[ObservationSelector, TargetEnvironmentModel.Edit]
  ): F[List[ObservationModel]]

  def bulkEditConstraintSet(
    be: BulkEdit[ObservationSelector, ConstraintSetModel.Edit]
  ): F[List[ObservationModel]]

  def bulkEditScienceRequirements(
    be: BulkEdit[ObservationSelector, ScienceRequirementsModel.Edit]
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
                tup => (tablesʹ, tup.asRight)
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
            initial <- TableState.observation.lookupValidated[State[Tables, *]](edit.observationId)
            edited   = initial.andThen(edit.edit)
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

      private def groupBy[A](
         pid: Program.Id
       )(
         f: ObservationModel => A
       ): F[List[Group[A]]] =
         tablesRef.get.map { t =>
           t.observations
            .filter { case (_, o) => o.programId === pid }
            .groupMap(tup => f(tup._2))(_._1)
            .toList
            .map { case (a, oids) => Group.from(a, oids) }
            .sortBy(_.observationIds.head)
         }

      override def groupBySingleScienceTarget(
        pid: Program.Id
      ): F[List[Group[Target]]] =
        tablesRef.get.map { t =>
          t.observations
           .filter { case (_, o) => o.programId === pid }
           .toList
           .flatMap { case (k, o) => o.targets.science.values.toList.tupleRight(k) }
           .groupMap(_._1)(_._2)
           .map { case (t, oids) => ObservationModel.Group.from(t, oids)}
           .toList
           .sortBy(_.observationIds.head)
        }

      override def groupByAllScienceTargets(
        pid: Program.Id
      ): F[List[Group[List[Target]]]] =
        groupBy(pid) { _.targets.science.values.toList }

      override def groupByTargetEnvironment(
        pid: Program.Id
      ): F[List[Group[TargetEnvironmentModel]]] =
        groupBy(pid) { _.targets }

      override def groupByConstraintSet(
        pid: Program.Id
      ): F[List[Group[ConstraintSetModel]]] =
        groupBy(pid)(_.constraintSet)

      override def groupByScienceRequirements(
        pid: Program.Id
      ): F[List[Group[ScienceRequirements]]] =
        groupBy(pid)(_.scienceRequirements)


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

      private def bulkEdit(
        initialObsList: State[Tables, ValidatedInput[List[ObservationModel]]],
        editor:         ObservationModel => ValidatedInput[ObservationModel]
      ): F[List[ObservationModel]] = {

        val update: State[Tables, ValidatedInput[List[ObservationModel]]] =
          for {
            initial <- initialObsList
            edited   = initial.andThen(_.traverse(editor))
            _       <- edited.traverse { os =>
              Tables.observations.mod_(_ ++ os.fproductLeft(_.id))
            }
          } yield edited

        for {
          os <- tablesRef.modifyState(update).flatMap(_.liftTo[F])
          _  <- os.traverse_(o => eventService.publish(ObservationModel.ObservationEvent.updated(o)))
        } yield os

      }

      override def bulkEditScienceTargetName(
        be: BulkEdit[ObservationSelector, TargetModel.EditName]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select.programId, be.select.observationIds),
          o => be.edit.editTargetMap(o.targets.science, "science", o.id).map { m =>
            ObservationModel.scienceTargets.replace(m)(o)
          }
        )

      override def bulkEditSiderealScienceTarget(
        be: BulkEdit[ObservationSelector, TargetModel.EditSidereal]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select.programId, be.select.observationIds),
          o => be.edit.editTargetMap(o.targets.science, "science", o.id).map { m =>
            ObservationModel.scienceTargets.replace(m)(o)
          }
        )

      override def bulkEditAllScienceTargets(
        be: BulkEdit[ObservationSelector, TargetModel.EditTargetList]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select.programId, be.select.observationIds),
          o => be.edit.edit(o.targets.science, "science", o.id).map { m =>
            ObservationModel.scienceTargets.replace(m)(o)
          }
        )

      override def bulkEditTargetEnvironment(
        be: BulkEdit[ObservationSelector, TargetEnvironmentModel.Edit]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select.programId, be.select.observationIds),
          o => be.edit.edit(o.targets, o.id).map { tem =>
            ObservationModel.targets.replace(tem)(o)
          }
        )

      override def bulkEditConstraintSet(
        be: BulkEdit[ObservationSelector, ConstraintSetModel.Edit]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select.programId, be.select.observationIds),
          o => be.edit.editor.map { s =>
            ObservationModel.constraintSet.replace(s.runS(o.constraintSet).value)(o)
          }
        )

      override def bulkEditScienceRequirements(
        be: BulkEdit[ObservationSelector, ScienceRequirementsModel.Edit]
      ): F[List[ObservationModel]] =

        bulkEdit(
          selectObservations(be.select.programId, be.select.observationIds),
          o => be.edit.editor.map { s =>
            ObservationModel.scienceRequirements.replace(s.runS(o.scienceRequirements).value)(o)
          }
        )

    }
}
