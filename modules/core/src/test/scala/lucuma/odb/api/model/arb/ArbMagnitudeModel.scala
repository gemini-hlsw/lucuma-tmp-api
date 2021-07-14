// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.{MagnitudeBand, MagnitudeSystem}
import lucuma.core.model.Magnitude
import lucuma.core.model.arb.ArbMagnitude
import lucuma.core.math.MagnitudeValue
import lucuma.core.util.arb.ArbEnumerated

import cats.syntax.all._
import clue.data.Input
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbMagnitudeModel {

  import ArbEnumerated._
  import ArbInput._
  import ArbMagnitude._

  implicit val arbMagnitudeModelCreate: Arbitrary[MagnitudeModel.Create] =
    Arbitrary {
      arbitrary[Magnitude].map { m =>
        MagnitudeModel.Create(m.band, m.value.toDoubleValue, Some(m.system), m.error.map(_.toDoubleValue))
      }
    }

  implicit val cogMagnitudeModelCreate: Cogen[MagnitudeModel.Create] =
    Cogen[Magnitude].contramap { in =>
      Magnitude(
        MagnitudeValue.fromBigDecimal.unsafeGet(in.value),
        in.band,
        in.error.flatMap(MagnitudeValue.fromBigDecimal.getOption),
        in.system.getOrElse(MagnitudeSystem.Vega)
      )
    }

  implicit val arbMagnitudeModelEdit: Arbitrary[MagnitudeModel.Edit] =
    Arbitrary {
      for {
        b <- arbitrary[MagnitudeBand]
        v <- arbitrary[Input[BigDecimal]]
        s <- arbitrary[Input[MagnitudeSystem]]
        e <- arbitrary[Input[BigDecimal]]
      } yield MagnitudeModel.Edit(b, v, s, e)
    }

  implicit val cogMagnitudeModelEdit: Cogen[MagnitudeModel.Edit] =
    Cogen[(
      MagnitudeBand,
      Input[BigDecimal],
      Input[MagnitudeSystem],
      Input[BigDecimal]
    )].contramap { in => (
      in.band,
      in.value,
      in.system,
      in.error
    )}

  implicit val arbMagnitudeEditAction: Arbitrary[MagnitudeModel.EditAction] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[MagnitudeModel.Create].map(c => MagnitudeModel.EditAction(c.some, None, None)),
        arbitrary[MagnitudeBand].map(b => MagnitudeModel.EditAction(None, b.some, None)),
        arbitrary[MagnitudeModel.Edit].map(e => MagnitudeModel.EditAction(None, None, e.some))
      )
    }

  implicit val cogMagnitudeEditAction: Cogen[MagnitudeModel.EditAction] =
    Cogen[(
      Option[MagnitudeModel.Create],
      Option[MagnitudeBand],
      Option[MagnitudeModel.Edit]
    )].contramap { in => (
      in.add,
      in.delete,
      in.edit
    )}

  implicit val arbMagnitudeModelEditList: Arbitrary[MagnitudeModel.EditList] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[Option[List[MagnitudeModel.Create]]].map(c => MagnitudeModel.EditList(c, None)),
        arbitrary[Option[List[MagnitudeModel.EditAction]]].map(e => MagnitudeModel.EditList(None, e))
      )
    }

  implicit val cogMagnitudeModelEditList: Cogen[MagnitudeModel.EditList] =
    Cogen[(
      Option[List[MagnitudeModel.Create]],
      Option[List[MagnitudeModel.EditAction]]
    )].contramap { in => (
      in.replaceList,
      in.editList
    )}
}

object ArbMagnitudeModel extends ArbMagnitudeModel
