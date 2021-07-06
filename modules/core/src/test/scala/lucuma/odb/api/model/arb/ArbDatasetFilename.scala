// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.Site
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.time.FourDigitYearLocalDate
import eu.timepit.refined.scalacheck.all.greaterArbitrary
import eu.timepit.refined.types.all.PosInt
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbDatasetFilename {

  import ArbEnumerated._
  import ArbFourDigitYearLocalDate._

  implicit val arbDatasetFilename: Arbitrary[DatasetFilename] =
    Arbitrary {
      for {
        s <- arbitrary[Site]
        d <- arbitrary[FourDigitYearLocalDate]
        i <- arbitrary[PosInt]
      } yield DatasetFilename(s, d, i)
    }

  implicit val cogDatasetFilename: Cogen[DatasetFilename] =
    Cogen[String].contramap(_.format)

  val genDatasetFilenameString: Gen[String] =
    Gen.oneOf(
      arbitrary[DatasetFilename].map(_.format),
      arbitrary[(DatasetFilename, Int)].map { case (f, n) => f.format.replace("1", (n % 10).abs.toString) },
      arbitrary[DatasetFilename].map(_.format.replace("S0", "S"))
    )

}

object ArbDatasetFilename extends ArbDatasetFilename
