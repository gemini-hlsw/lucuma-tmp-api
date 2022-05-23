// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import lucuma.odb.api.model.time._
import shapeless.tag
import shapeless.tag.@@

import java.time.Duration

package object gen {

  sealed trait AcqExposureTimeTag
  sealed trait SciExposureTimeTag
  sealed trait ReacquirePeriodTag

  trait NonNegDurationCompanionOps[U, T] {
    def from(fd: Duration): Option[NonNegDuration @@ T] =
      NonNegDuration.from(fd).toOption.map(tag[T][NonNegDuration](_))

    def unsafeFrom(fd: Duration): NonNegDuration @@ T =
      from(fd).get
  }

  type AcqExposureTime = NonNegDuration @@ AcqExposureTimeTag
  object AcqExposureTime extends NonNegDurationCompanionOps[AcqExposureTime, AcqExposureTimeTag]

  type SciExposureTime = NonNegDuration @@ SciExposureTimeTag
  object SciExposureTime extends NonNegDurationCompanionOps[SciExposureTime, SciExposureTimeTag]

  type ReacquirePeriod = NonNegDuration @@ ReacquirePeriodTag
  object ReacquirePeriod extends NonNegDurationCompanionOps[ReacquirePeriod, ReacquirePeriodTag]

}
