// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen

import cats.data.{NonEmptyList, State}
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import lucuma.odb.api.model.{Atom, AtomModel, Breakpoint, Sequence, SequenceModel, Step, StepConfig, StepModel}

/**
 * Helper for `Generator` implementations.  Provides utilities for creating
 * steps and atoms along with their IDs.
 */
private[gen] trait GeneratorHelper[D] {

  def createSequence[F[_]: Sync](
    atom: NonEmptyList[StepConfig[D]]
  ): F[Sequence[D]] =
    createAtom(atom).map(a => SequenceModel(List(a)))

  def createSequence[F[_]: Sync](
    atoms: List[NonEmptyList[StepConfig[D]]]
  ): F[Sequence[D]] =
    atoms.traverse(nel => createAtom(nel)).map(as => SequenceModel(as))

  def createAtom[F[_]: Sync](
    step0: F[StepModel[D]],
    steps: F[StepModel[D]]*
  ): F[AtomModel[StepModel[D]]] =
    for {
      aid   <- Atom.Id.random[F]
      steps <- NonEmptyList(step0, steps.toList).sequence
    } yield AtomModel(aid, steps)

  def createAtom[F[_]: Sync](
    steps: NonEmptyList[StepConfig[D]]
  ): F[AtomModel[StepModel[D]]] =
    for {
      aid   <- Atom.Id.random[F]
      steps <- steps.traverse(createStep(_))
    } yield AtomModel(aid, steps)

  def createStep[F[_]: Sync](
    stepConfig: StepConfig[D],
    breakpoint: Breakpoint = Breakpoint.disabled
  ): F[StepModel[D]] =
    Step.Id.random[F].map { sid => StepModel(sid, breakpoint, stepConfig) }

  def remainingSteps[A](
    wholeSequence: List[A],
    recordedSteps: List[RecordedStep[D]]
  )(
    atom: A => NonEmptyList[StepConfig[D]]
  ): List[A] = {

    // We remove atoms from the wholeSequence when a matching, contiguous,
    // executed series of steps is found in the recorded list.  This is
    // still too simplistic since "contiguous" could be separated by days
    // of time. TBD.

    // Also, we need to filter out any steps that were taken under different
    // static configurations.

    def isExecuted(a: A): State[List[RecordedStep[D]], Boolean] = {
      val steps = atom(a).toList.map(sc => RecordedStep(sc, successfullyExecuted = true))

      for {
        rs <- State.get[List[RecordedStep[D]]]
        i   = rs.indexOfSlice(steps, 0)
        _  <- State.set[List[RecordedStep[D]]] {
          if (i < 0) rs else rs.patch(i, Nil, steps.length)
        }
      } yield i > -1
    }

    wholeSequence
      .zip(wholeSequence.traverse(isExecuted).runA(recordedSteps).value)
      .collect { case (a, false) => a }
  }

}