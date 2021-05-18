// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.{Asterism, Atom, ConstraintSet, Observation, Program, Step, Target}
import lucuma.odb.api.model.{AsterismModel, AtomModel, ConstraintSetModel, DatabaseState, Dataset, DatasetModel, ExecutionEvent, ExecutionEventModel, ObservationModel, ProgramModel, RepoState, SharingState, StepModel, TargetModel}
import cats.data.State
import cats.mtl.Stateful
import monocle.Lens
import monocle.state.all._

trait TableState extends DatabaseState[Tables] {

  val nextEventId: State[Tables, Long] =
    Tables.lastEventId.mod(_ + 1L)

  override val atom: RepoState[Tables, Atom.Id, AtomModel[Step.Id]] =
    RepoState.fromLenses(Tables.lastAtomId, Tables.atoms)

  override val asterism: RepoState[Tables, Asterism.Id, AsterismModel] =
    RepoState.fromLenses(Tables.lastAsterismId, Tables.asterisms)

  override val constraintSet: RepoState[Tables, ConstraintSet.Id, ConstraintSetModel] =
    RepoState.fromLenses(Tables.lastConstraintSetId, Tables.constraintSets)

  override val dataset: RepoState[Tables, Dataset.Id, DatasetModel] =
    RepoState.fromLenses(Tables.lastDatasetId, Tables.datasets)

  override val executionEvent: RepoState[Tables, ExecutionEvent.Id, ExecutionEventModel] =
    RepoState.fromLenses(Tables.lastExecutionEventId, Tables.executionEvents)

  override val observation: RepoState[Tables, Observation.Id, ObservationModel] =
    RepoState.fromLenses(Tables.lastObservationId, Tables.observations)

  override val program: RepoState[Tables, Program.Id, ProgramModel] =
    RepoState.fromLenses(Tables.lastProgramId, Tables.programs)

  override val step: RepoState[Tables, Step.Id, StepModel[_]] =
    RepoState.fromLenses(Tables.lastStepId, Tables.steps)

  override val target: RepoState[Tables, Target.Id, TargetModel] =
    RepoState.fromLenses(Tables.lastTargetId, Tables.targets)

  override val programAsterism: SharingState[Tables, Program.Id, Asterism.Id] =
    new SharingStateManyToMany[Program.Id, Asterism.Id](Tables.programAsterism)

}

final class SharingStateManyToMany[A, B](lens: Lens[Tables, ManyToMany[A, B]]) extends SharingState[Tables, A, B] {
  def update[F[_]](f: ManyToMany[A, B] => ManyToMany[A, B])(implicit S: Stateful[F, Tables]): F[Unit] =
    S.modify(lens.modify(f))

  override def link[F[_]](link: (A, B))(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ + link)

  override def unlink[F[_]](link: (A, B))(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ - link)

  override def linkAll[F[_]](links: List[(A, B)])(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ ++ links)

  override def unlinkAll[F[_]](links: List[(A, B)])(implicit S: Stateful[F, Tables]): F[Unit] =
    update(_ -- links)

}

object TableState extends TableState
