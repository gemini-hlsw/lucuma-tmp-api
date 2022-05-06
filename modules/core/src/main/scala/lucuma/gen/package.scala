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
    def from(fd: Duration): Option[NonNegativeDuration @@ T] =
      NonNegativeDuration.from(fd).toOption.map(tag[T][NonNegativeDuration](_))

    def unsafeFrom(fd: Duration): NonNegativeDuration @@ T =
      from(fd).get
  }

  type AcqExposureTime = NonNegativeDuration @@ AcqExposureTimeTag
  object AcqExposureTime extends NonNegDurationCompanionOps[AcqExposureTime, AcqExposureTimeTag]

  type SciExposureTime = NonNegativeDuration @@ SciExposureTimeTag
  object SciExposureTime extends NonNegDurationCompanionOps[SciExposureTime, SciExposureTimeTag]

  type ReacquirePeriod = NonNegativeDuration @@ ReacquirePeriodTag
  object ReacquirePeriod extends NonNegDurationCompanionOps[ReacquirePeriod, ReacquirePeriodTag]

}
