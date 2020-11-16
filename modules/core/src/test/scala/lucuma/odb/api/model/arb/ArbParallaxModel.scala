// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import NumericUnits.{LongInput, DecimalInput}

import lucuma.core.util.Enumerated
import org.scalacheck._

trait ArbParallaxModel {
  import GenNumericUnitsInput._

  import ParallaxModel.{Input, Units}

  implicit val arbParallaxModelUnits: Arbitrary[ParallaxModel.Units] =
    Arbitrary { Gen.oneOf(Enumerated[ParallaxModel.Units].all) }

  private val parallaxTestRangeLong: Gen[Long] =
    Gen.choose(-1L, 1000000L)

  private val parallaxTestRangeBigDecimal: Gen[BigDecimal] =
    genBigDecimal(parallaxTestRangeLong, 6)

  implicit val arbParallaxModelInput: Arbitrary[ParallaxModel.Input] =
    Arbitrary {
      Gen.oneOf(
        parallaxTestRangeLong.map(Input.fromMicroarcseconds),
        parallaxTestRangeBigDecimal.map(Input.fromMilliarcseconds),
        genLongInput[Units](parallaxTestRangeLong).map(Input.fromLong),
        genDecimalInput[Units](parallaxTestRangeBigDecimal).map(Input.fromDecimal)
      )
    }

  implicit val cogUnits: Cogen[Units] =
    Cogen[String].contramap(_.angleUnit.name)

  implicit val cogParallaxModelInput: Cogen[ParallaxModel.Input] =
    Cogen[(Option[Long], Option[BigDecimal], Option[LongInput[Units]], Option[DecimalInput[Units]])].contramap { pmi =>
      (pmi.microarcseconds, pmi.milliarcseconds, pmi.fromLong, pmi.fromDecimal)

    }
}

object ArbParallaxModel extends ArbParallaxModel
