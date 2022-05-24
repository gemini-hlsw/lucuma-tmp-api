// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.{Eq, Monoid}
import eu.timepit.refined.cats._
import lucuma.core.syntax.time._
import lucuma.odb.api.model.time.NonNegDuration
import lucuma.odb.api.model.syntax.nonnegduration._


final case class PlannedTimeSummaryModel(
  piTime:        NonNegDuration,
  unchargedTime: NonNegDuration
) {

  def executionTime: NonNegDuration =
    piTime + unchargedTime

}

object PlannedTimeSummaryModel {

  val Zero: PlannedTimeSummaryModel =
    PlannedTimeSummaryModel(NonNegDuration.zero, NonNegDuration.zero)

  implicit val MonoidPlannedTimeSummaryModel: Monoid[PlannedTimeSummaryModel] =
    Monoid.instance(
      Zero,
      (a, b) =>
        PlannedTimeSummaryModel(
          a.piTime        + b.piTime,
          a.unchargedTime + b.unchargedTime
        )
    )

  implicit val EqPlannedTimeSummaryModel: Eq[PlannedTimeSummaryModel] =
    Eq.by(t => (t.piTime.toNanos, t.unchargedTime.toNanos))

  // "calculate" the planned time.  want a stable value so ... here's something
  // random for now
  def forObservation(o: ObservationModel): PlannedTimeSummaryModel = {
    val l = o.id.hashCode()
    PlannedTimeSummaryModel(
      NonNegDuration.unsafeFrom((((l % 20).abs + 1) * 5).toLong.minutes),
      NonNegDuration.unsafeFrom((15 - (l % 15).abs).toLong.minutes)
    )
  }

}
