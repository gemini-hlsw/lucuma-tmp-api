// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.numeric._
import shapeless.Nat._0
import shapeless.tag
import shapeless.tag.@@

import scala.concurrent.duration.FiniteDuration


package object gen {

  type NonNegFiniteDuration = FiniteDuration Refined NonNegative
  object NonNegFiniteDuration extends RefinedTypeOps[NonNegFiniteDuration, FiniteDuration]

  implicit val nonNegFiniteDurationValidate: Plain[FiniteDuration, Not[Less[_0]]] =
    Validate.fromPredicate(
      (fd: FiniteDuration) => fd.length >= 0,
      (fd: FiniteDuration) => s"$fd is non-negative",
      Not(Less(shapeless.nat._0))
    )

  sealed trait AcqExposureTimeTag
  sealed trait SciExposureTimeTag
  sealed trait ReacquirePeriodTag

  trait NonNegFiniteDurationCompanionOps[U, T] {
    def from(fd: FiniteDuration): Option[NonNegFiniteDuration @@ T] =
      NonNegFiniteDuration.from(fd).toOption.map(tag[T][NonNegFiniteDuration](_))

    def unsafeFrom(fd: FiniteDuration): NonNegFiniteDuration @@ T =
      from(fd).get
  }

  type AcqExposureTime = NonNegFiniteDuration @@ AcqExposureTimeTag
  object AcqExposureTime extends NonNegFiniteDurationCompanionOps[AcqExposureTime, AcqExposureTimeTag]

  type SciExposureTime = NonNegFiniteDuration @@ SciExposureTimeTag
  object SciExposureTime extends NonNegFiniteDurationCompanionOps[SciExposureTime, SciExposureTimeTag]

  type ReacquirePeriod = NonNegFiniteDuration @@ ReacquirePeriodTag
  object ReacquirePeriod extends NonNegFiniteDurationCompanionOps[ReacquirePeriod, ReacquirePeriodTag]

}
