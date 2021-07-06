// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.{Eq, Monoid}
import cats.effect.Async
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.concurrent.duration._
import scala.util.Random

final case class PlannedTimeSummaryModel(
  piTime:        FiniteDuration,
  unchargedTime: FiniteDuration
) {

  def executionTime: FiniteDuration =
    piTime + unchargedTime

}

object PlannedTimeSummaryModel {

  val Zero: PlannedTimeSummaryModel =
    PlannedTimeSummaryModel(0.microseconds, 0.microseconds)

  def random[F[_]: Async]: F[PlannedTimeSummaryModel] =
    for {
      p <- Async[F].delay(Random.between(5L, 120L))
      u <- Async[F].delay(Random.between(1L,  15L))
    } yield PlannedTimeSummaryModel(p.minutes, u.minutes)


  implicit val MonoidPlannedTimeSummaryModel: Monoid[PlannedTimeSummaryModel] =
    Monoid.instance(
      Zero,
      (a, b) =>
        PlannedTimeSummaryModel(
          a.piTime + b.piTime,
          a.unchargedTime + b.unchargedTime
        )
    )

  implicit val EqPlannedTimeSummaryModel: Eq[PlannedTimeSummaryModel] =
    Eq.by(t => (t.piTime.toNanos, t.unchargedTime.toNanos))

}
