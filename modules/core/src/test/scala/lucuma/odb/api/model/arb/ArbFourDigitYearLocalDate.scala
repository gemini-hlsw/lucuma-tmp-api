// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb


import lucuma.odb.api.model.time.FourDigitYearLocalDate
import org.scalacheck._

import java.time.LocalDate


trait ArbFourDigitYearLocalDate {

  implicit val arbFourDigitYearLocalDate: Arbitrary[FourDigitYearLocalDate] =
    Arbitrary {
      Gen.chooseNum(
        FourDigitYearLocalDate.MinDate.value.toEpochDay,
        FourDigitYearLocalDate.MaxDate.value.toEpochDay
      ).map { epochDay =>
        FourDigitYearLocalDate.unsafeFrom(LocalDate.ofEpochDay(epochDay))
      }
    }

}

object ArbFourDigitYearLocalDate extends ArbFourDigitYearLocalDate
