// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.eq._
import cats.syntax.traverse._
import cats.syntax.option._
import lucuma.core.enums.DatasetQaState
import lucuma.core.util.Enumerated
import monocle.Iso

sealed abstract class StepQaState(
  val tag: String
) extends Product with Serializable {

  def isPass: Boolean =
    this match {
      case StepQaState.Pass => true
      case StepQaState.Fail => false
    }

  def isFail: Boolean =
    !isPass

  def fold[A](pass: => A, fail: => A): A =
    this match {
      case StepQaState.Pass => pass
      case StepQaState.Fail => fail
    }

  def shortName: String =
    tag

  def longName: String =
    tag

}

object StepQaState {

  /** @group Constructors */ case object Pass extends StepQaState("Pass")
  /** @group Constructors */ case object Fail extends StepQaState("Fail")

  lazy val all: List[StepQaState] =
    List(Pass, Fail)

  def fromTag(s: String): Option[StepQaState] =
    all.find(_.tag === s)

  def unsafeFromTag(s: String): StepQaState =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"StepQaState: Invalid tag: '$s'"))

  val ToBoolean: Iso[StepQaState, Boolean] =
    Iso[StepQaState, Boolean](_.isPass)(v => if (v) StepQaState.Pass else StepQaState.Fail)

  /** @group Typeclass Instances */
  implicit val EnumeratedStepQaState: Enumerated[StepQaState] =
    new Enumerated[StepQaState] {
      override def all: List[StepQaState] =
        StepQaState.all

      override def tag(a: StepQaState): String =
        a.tag

      override def unsafeFromTag(s: String): StepQaState =
        StepQaState.unsafeFromTag(s)
    }

  def rollup(dsets: List[Option[DatasetQaState]]): Option[StepQaState] =
    if (dsets.exists(_.exists(_ =!= DatasetQaState.Pass))) StepQaState.Fail.some
    else Option.when(dsets.nonEmpty && dsets.sequence.isDefined)(StepQaState.Pass)

}

