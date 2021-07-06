// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import NumericUnits.{DecimalInput, LongInput}
import ParallaxModel.{Input, Units}

import lucuma.core.math.Parallax
import lucuma.core.math.arb.ArbParallax
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.math.MathContext

trait ArbParallaxModel {

  import ArbEnumerated._
  import ArbParallax._
  import GenNumericUnitsInput._

  private[this] val microarcseconds: Gen[Long] =
    arbitrary[Parallax].map(_.μas.value.value)

  private[this] val milliarcseconds: Gen[BigDecimal] =
    arbitrary[Parallax].map(_.mas.value.toBigDecimal(MathContext.UNLIMITED))

  val genParallaxModelInputFromLong: Gen[Input] =
    Gen.oneOf(
      genLongInput(microarcseconds, Units.microarcseconds),
      genLongInput(milliarcseconds, Units.milliarcseconds)
    ).map(Input.fromLong)

  val genParallaxModelInputFromDecimal: Gen[Input] =
    Gen.oneOf(
      genLongDecimalInput(microarcseconds, Units.microarcseconds),
      genDecimalInput(milliarcseconds, Units.milliarcseconds)
    ).map(Input.fromDecimal)

  implicit val arbParallaxModelInput: Arbitrary[ParallaxModel.Input] =
    Arbitrary {
      Gen.oneOf(
        microarcseconds.map(Input.fromMicroarcseconds),
        milliarcseconds.map(Input.fromMilliarcseconds),
        genParallaxModelInputFromLong,
        genParallaxModelInputFromDecimal
      )
    }

  implicit val cogParallaxModelInput: Cogen[ParallaxModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[LongInput[Units]],
      Option[DecimalInput[Units]]
    )].contramap { in =>
      (in.microarcseconds, in.milliarcseconds, in.fromLong, in.fromDecimal)
    }
}

object ArbParallaxModel extends ArbParallaxModel
