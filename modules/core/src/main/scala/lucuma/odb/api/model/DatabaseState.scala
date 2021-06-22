// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Asterism, Atom, Observation, Program, Step, Target}

trait DatabaseState[T] extends DatabaseReader[T] {

  def atom:           RepoState[T, Atom.Id, AtomModel[Step.Id]]

  def asterism:       RepoState[T, Asterism.Id, AsterismModel]

  def executionEvent: RepoState[T, ExecutionEvent.Id, ExecutionEventModel]

  def observation:    RepoState[T, Observation.Id, ObservationModel]

  def program:        RepoState[T, Program.Id, ProgramModel]

  def step:           RepoState[T, Step.Id, StepModel[_]]

  def target:         RepoState[T, Target.Id, TargetModel]

  def programAsterism: SharingState[T, Program.Id, Asterism.Id]

}
