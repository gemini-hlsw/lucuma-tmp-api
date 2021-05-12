// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Asterism, Atom, ConstraintSet, Observation, Program, Step, Target}
import cats.data.State
import cats.syntax.all._

trait Database[T] {

  val atom:          Store[T, Atom.Id, AtomModel[_]]

  val asterism:      Store[T, Asterism.Id, AsterismModel]

  val constraintSet: Store[T, ConstraintSet.Id, ConstraintSetModel]

  val observation:   Store[T, Observation.Id, ObservationModel]

  val program:       Store[T, Program.Id, ProgramModel]

  val step:          Store[T, Step.Id, StepModel[_]]

  val target:        Store[T, Target.Id, TargetModel]

  def error[A](m: => String): State[T, ValidatedInput[A]] =
    State.pure[T, ValidatedInput[A]](InputError.fromMessage(m).invalidNec[A])

}
