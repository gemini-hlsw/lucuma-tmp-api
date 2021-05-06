// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Asterism, ConstraintSet, Observation, Program, Target}

trait DatabaseReader[T] {

  def atom:          RepoReader[T, Atom.Id, AtomModel[Step.Id]]

  def asterism:      RepoReader[T, Asterism.Id, AsterismModel]

  def constraintSet: RepoReader[T, ConstraintSet.Id, ConstraintSetModel]

  def observation:   RepoReader[T, Observation.Id, ObservationModel]

  def program:       RepoReader[T, Program.Id, ProgramModel]

  def step:          RepoReader[T, Step.Id, StepModel[_]]

  def target:        RepoReader[T, Target.Id, TargetModel]

}
