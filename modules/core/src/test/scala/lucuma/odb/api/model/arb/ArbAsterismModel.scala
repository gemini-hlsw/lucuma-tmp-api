// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.odb.api.model.AsterismModel.{Create, Edit}
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.model.{Asterism, Program}
import lucuma.core.util.arb.ArbEnumerated

import clue.data.Input
import eu.timepit.refined.types.all.NonEmptyString
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbAsterismModel {

  import ArbCoordinates._
  import ArbCoordinatesModel._
  import ArbEnumerated._
  import lucuma.core.util.arb.ArbGid._
  import ArbInput._

  implicit val arbAsterismModel: Arbitrary[AsterismModel] =
    Arbitrary {
      for {
        id <- arbitrary[Asterism.Id]
        ex <- arbitrary[Existence]
        nm <- Gen.option(Gen.alphaNumStr.suchThat(!_.isEmpty).map(NonEmptyString.unsafeFrom))
        eb <- arbitrary[Option[Coordinates]]
      } yield AsterismModel(id, ex, nm, eb)
    }

  implicit val cogAsterismModel: Cogen[AsterismModel] =
    Cogen[(
      Asterism.Id,
      Existence,
      Option[String],
      Option[Coordinates]
    )].contramap { in => (
      in.id,
      in.existence,
      in.name.map(_.value),
      in.explicitBase
    )}

  implicit val arbCreateDefault: Arbitrary[Create] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Asterism.Id]]
        nm <- Gen.option(Gen.alphaNumStr.suchThat(!_.isEmpty))
        ps <- arbitrary[List[Program.Id]]
        eb <- arbitrary[Option[CoordinatesModel.Input]]
      } yield Create(
        id,
        nm,
        ps,
        eb
      )
    }

  implicit val cogCreateDefault: Cogen[Create] =
    Cogen[(
      Option[Asterism.Id],
      Option[String],
      List[Program.Id],
      Option[CoordinatesModel.Input]
    )].contramap { in => (
      in.asterismId,
      in.name,
      in.programIds,
      in.explicitBase
    )}

  implicit val arbEditDefault: Arbitrary[Edit] =
    Arbitrary {
      for {
        id <- arbitrary[Asterism.Id]
        ex <- arbNotNullableInput[Existence].arbitrary
        nm <- arbitrary[Input[String]]
        eb <- arbitrary[Input[CoordinatesModel.Input]]
      } yield Edit(
        id,
        ex,
        nm,
        eb
      )
    }

  implicit val cogEditDefault: Cogen[Edit] =
    Cogen[(
      Asterism.Id,
      Input[Existence],
      Input[String],
      Input[CoordinatesModel.Input]
    )].contramap { in => (
      in.asterismId,
      in.existence,
      in.name,
      in.explicitBase
    )}

}

object ArbAsterismModel extends ArbAsterismModel
