// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Functor}

import scala.collection.immutable.SortedSet

/**
 * TargetEnvironments grouped by some common value.
 */
final case class TargetEnvironmentGroup[A](
  value:                A,
  targetEnvironmentIds: SortedSet[TargetEnvironment.Id]
)

object TargetEnvironmentGroup {

  implicit def EqGroup[A: Eq]: Eq[TargetEnvironmentGroup[A]] =
    Eq.by { tem => (
      tem.value,
      tem.targetEnvironmentIds
    )}

  implicit val FunctorTargetEnvironmentGroup: Functor[TargetEnvironmentGroup] =
    new Functor[TargetEnvironmentGroup] {
      override def map[A, B](fa: TargetEnvironmentGroup[A])(f: A => B): TargetEnvironmentGroup[B] =
        TargetEnvironmentGroup[B](f(fa.value), fa.targetEnvironmentIds)
    }

}
