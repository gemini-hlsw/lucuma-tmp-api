// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import AngleModel.AngleInput
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbAngleModel {

  implicit val arbAngleInput: Arbitrary[AngleInput] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[Long             ].map(AngleInput.fromMicroarcseconds),
        arbitrary[BigDecimal       ].map(AngleInput.fromMicroseconds),
        arbitrary[BigDecimal       ].map(AngleInput.fromMilliarcseconds),
        arbitrary[BigDecimal       ].map(AngleInput.fromMilliseconds),
        arbitrary[BigDecimal       ].map(AngleInput.fromArcseconds),
        arbitrary[BigDecimal       ].map(AngleInput.fromSeconds),
        arbitrary[BigDecimal       ].map(AngleInput.fromArcminutes),
        arbitrary[BigDecimal       ].map(AngleInput.fromMinutes),
        arbitrary[BigDecimal       ].map(AngleInput.fromDegrees),
        arbitrary[BigDecimal       ].map(AngleInput.fromHours)
      )
    }

  implicit val cogAngleInput: Cogen[AngleInput] =
    Cogen[(
      Option[Long],       // µas
      Option[BigDecimal], // µs
      Option[BigDecimal], // mas
      Option[BigDecimal], // ms
      Option[BigDecimal], // as
      Option[BigDecimal], // s
      Option[BigDecimal], // am
      Option[BigDecimal], // m
      Option[BigDecimal], // deg
      Option[BigDecimal]  // h
    )].contramap { in => (
      in.microarcseconds,
      in.microseconds,
      in.milliarcseconds,
      in.milliseconds,
      in.arcseconds,
      in.seconds,
      in.arcminutes,
      in.minutes,
      in.degrees,
      in.hours
    )}

}

object ArbAngleModel extends ArbAngleModel
