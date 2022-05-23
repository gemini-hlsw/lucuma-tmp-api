// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import ParallaxModel.Input

import lucuma.core.math.Parallax
import lucuma.core.math.arb.ArbParallax
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.math.MathContext

trait ArbParallaxModel {

  import ArbParallax._

  private[this] val microarcseconds: Gen[Long] =
    arbitrary[Parallax].map(_.μas.value.value)

  private[this] val milliarcseconds: Gen[BigDecimal] =
    arbitrary[Parallax].map(_.mas.value.toBigDecimal(MathContext.UNLIMITED))

  implicit val arbParallaxModelInput: Arbitrary[ParallaxModel.Input] =
    Arbitrary {
      Gen.oneOf(
        microarcseconds.map(Input.fromMicroarcseconds),
        milliarcseconds.map(Input.fromMilliarcseconds)
      )
    }

  implicit val cogParallaxModelInput: Cogen[ParallaxModel.Input] =
    Cogen[(
      Option[Long],
      Option[BigDecimal]
    )].contramap { in => (in.microarcseconds, in.milliarcseconds) }
}

object ArbParallaxModel extends ArbParallaxModel
