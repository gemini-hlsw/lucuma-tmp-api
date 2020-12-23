// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.odb.api.model.AsterismModel.{CreateDefault, EditDefault}
import lucuma.core.model.{Asterism, Program, Target}
import lucuma.core.util.arb.ArbEnumerated

import clue.data.Input
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbAsterismModel {

  import ArbCoordinatesModel._
  import ArbEnumerated._
  import lucuma.core.util.arb.ArbGid._
  import ArbInput._

  implicit val arbCreateDefault: Arbitrary[CreateDefault] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Asterism.Id]]
        nm <- Gen.option(Gen.alphaNumStr.suchThat(!_.isEmpty))
        ps <- arbitrary[List[Program.Id]]
        eb <- arbitrary[Option[CoordinatesModel.Input]]
        ts <- arbitrary[List[Target.Id]].map(_.toSet)
      } yield CreateDefault(
        id,
        nm,
        ps,
        eb,
        ts
      )
    }

  implicit val cogCreateDefault: Cogen[CreateDefault] =
    Cogen[(
      Option[Asterism.Id],
      Option[String],
      List[Program.Id],
      Option[CoordinatesModel.Input],
      List[Target.Id]
    )].contramap { in => (
      in.asterismId,
      in.name,
      in.programIds,
      in.explicitBase,
      in.targetIds.toList
    )}

  implicit val arbEditDefault: Arbitrary[EditDefault] =
    Arbitrary {
      for {
        id <- arbitrary[Asterism.Id]
        ex <- arbNotNullableInput[Existence].arbitrary
        nm <- arbitrary[Input[String]]
        eb <- arbitrary[Input[CoordinatesModel.Input]]
        ts <- arbitrary[Option[List[Target.Id]]].map(_.map(_.toSet))
      } yield EditDefault(
        id,
        ex,
        nm,
        eb,
        ts
      )
    }

  implicit val cogEditDefault: Cogen[EditDefault] =
    Cogen[(
      Asterism.Id,
      Input[Existence],
      Input[String],
      Input[CoordinatesModel.Input],
      Option[List[Target.Id]]
    )].contramap { in => (
      in.asterismId,
      in.existence,
      in.name,
      in.explicitBase,
      in.targetIds.map(_.toList)
    )}

}

object ArbAsterismModel extends ArbAsterismModel
