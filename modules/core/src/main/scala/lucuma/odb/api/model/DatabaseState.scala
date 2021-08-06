// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Atom, ExecutionEvent, Observation, Program, Step}

trait DatabaseState[T] extends DatabaseReader[T] {

  def atom:           RepoState[T, Atom.Id, AtomModel[Step.Id]]

  def executionEvent: RepoState[T, ExecutionEvent.Id, ExecutionEventModel]

  def observation:    RepoState[T, Observation.Id, ObservationModel]

  def program:        RepoState[T, Program.Id, ProgramModel]

  def step:           RepoState[T, Step.Id, StepModel[_]]

}
