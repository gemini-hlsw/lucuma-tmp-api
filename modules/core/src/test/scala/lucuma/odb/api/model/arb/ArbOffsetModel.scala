// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import OffsetModel.{ComponentInput, Input}
import lucuma.core.math.{Angle, Offset}
import lucuma.core.math.arb.ArbOffset
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbOffsetModel {

  import ArbOffset._

  private[this] def microarcseconds: Gen[Long] =
    arbitrary[Offset.Component[Unit]].map(c => Angle.signedMicroarcseconds.get(c.toAngle))

  private[this] def milliseconds: Gen[BigDecimal] =
    arbitrary[Offset.Component[Unit]].map(c => Angle.signedDecimalMilliarcseconds.get(c.toAngle))

  private[this] def arcseconds: Gen[BigDecimal] =
    arbitrary[Offset.Component[Unit]].map(c => Angle.signedDecimalArcseconds.get(c.toAngle))

  implicit val arbOffsetComponentInput: Arbitrary[ComponentInput] =
    Arbitrary {
      Gen.oneOf(
        microarcseconds.map(ComponentInput.fromMicroarcseconds),
        milliseconds.map(ComponentInput.fromMilliarcseconds),
        arcseconds.map(ComponentInput.fromArcseconds)
      )
    }

  implicit val cogOffsetComponentInput: Cogen[ComponentInput] =
    Cogen[(
      Option[Long],
      Option[BigDecimal],
      Option[BigDecimal]
    )].contramap { in =>
      (
        in.microarcseconds,
        in.milliarcseconds,
        in.arcseconds
      )
    }

  implicit val arbOffsetModelInput: Arbitrary[Input] =
    Arbitrary {
      for {
        p <- arbitrary[ComponentInput]
        q <- arbitrary[ComponentInput]
      } yield Input(p, q)
    }

  implicit val cogOffsetModelInput: Cogen[Input] =
    Cogen[(ComponentInput, ComponentInput)].contramap { in =>
      (in.p, in.q)
    }

}

object ArbOffsetModel extends ArbOffsetModel
