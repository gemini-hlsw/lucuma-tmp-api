// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import cats.Order.catsKernelOrderingForOrder
import clue.data.Input
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.math.{Coordinates, Epoch}
import lucuma.core.math.arb.{ArbCoordinates, ArbEpoch}
import lucuma.core.model.{Program, Target}
import lucuma.core.model.arb.ArbTarget
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import lucuma.odb.api.model.targetModel.SourceProfileModel.SourceProfileInput
import lucuma.odb.api.model.targetModel._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import scala.collection.immutable.SortedSet


trait ArbTargetModel {

  import ArbCatalogInfoInput._
  import ArbCoordinates._
  import ArbCoordinatesModel._
  import ArbDeclinationModel._
  import ArbEnumerated._
  import ArbEpoch._
  import ArbGid._
  import ArbInput._
  import ArbParallaxModel._
  import ArbProperMotionModel._
  import ArbRadialVelocityModel._
  import ArbRightAscensionModel._
  import ArbSourceProfileModel._
  import ArbTarget._

  implicit val arbTargetModel: Arbitrary[TargetModel] =
    Arbitrary {
      for {
        id  <- arbitrary[Target.Id]
        ex  <- arbitrary[Existence]
        pid <- arbitrary[Program.Id]
        t   <- arbitrary[Target]
        o   <- arbitrary[Boolean]
      } yield TargetModel(id, ex, pid, t, o)
    }

  implicit val cogTargetModel: Cogen[TargetModel] =
    Cogen[(
      Target.Id,
      Existence,
      Program.Id,
      Target,
      Boolean
    )].contramap { in => (
      in.id,
      in.existence,
      in.programId,
      in.target,
      in.observed
    )}

  implicit val arbTargetEnvironmentModel: Arbitrary[TargetEnvironmentModel] =
    Arbitrary {
      for {
        a <- arbitrary[List[Target.Id]].map(ts => SortedSet.from(ts))
        b <- arbitrary[Option[Coordinates]]
      } yield TargetEnvironmentModel(a, b)
    }

  implicit val cogTargetEnvironmentModel: Cogen[TargetEnvironmentModel] =
    Cogen[(
      List[Target.Id],
      Option[Coordinates]
    )].contramap { in => (
      in.asterism.toList,
      in.explicitBase
    )}

  implicit val arbNonsiderealInput: Arbitrary[NonsiderealInput] =
    Arbitrary {
      for {
        keyType <- arbitrary[Input[EphemerisKeyType]]
        des     <- arbitrary[Input[NonEmptyString]]
        key     <- arbitrary[Input[NonEmptyString]]
      } yield NonsiderealInput(
        keyType,
        des,
        key
      )
    }

  implicit val cogNonsidereal: Cogen[NonsiderealInput] =
    Cogen[(
      Input[EphemerisKeyType],
      Input[String],
      Input[String]
    )].contramap { a => (
      a.keyType,
      a.des.map(_.value),
      a.key.map(_.value),
    )}

  implicit val arbSiderealInput: Arbitrary[SiderealInput] =
    Arbitrary {
      for {
        ra    <- arbNotNullableInput[RightAscensionModel.Input].arbitrary
        dec   <- arbNotNullableInput[DeclinationModel.Input].arbitrary
        epoch <- arbNotNullableInput[Epoch].arbitrary
        pm    <- arbitrary[Input[ProperMotionModel.Input]]
        rv    <- arbitrary[Input[RadialVelocityModel.Input]]
        px    <- arbitrary[Input[ParallaxModel.Input]]
        cat   <- arbitrary[Input[CatalogInfoInput]]
      } yield SiderealInput(
        ra,
        dec,
        epoch,
        pm,
        rv,
        px,
        cat
      )
    }

  implicit val cogSiderealInput: Cogen[SiderealInput] =
    Cogen[(
      Input[RightAscensionModel.Input],
      Input[DeclinationModel.Input],
      Input[Epoch],
      Input[ProperMotionModel.Input],
      Input[RadialVelocityModel.Input],
      Input[ParallaxModel.Input],
      Input[CatalogInfoInput]
    )].contramap { a => (
      a.ra,
      a.dec,
      a.epoch,
      a.properMotion,
      a.radialVelocity,
      a.parallax,
      a.catalogInfo
    )}

  implicit val arbCreateTarget: Arbitrary[TargetModel.Create] =
    Arbitrary {
      for {
        t <- arbitrary[Option[Target.Id]]
        m <- arbitrary[NonEmptyString]
        s <- arbitrary[Option[SiderealInput]]
        n <- arbitrary[Option[NonsiderealInput]]
        p <- arbitrary[SourceProfileInput]
      } yield TargetModel.Create(t, m, s, n, p)
    }

  implicit val cogCreateTarget: Cogen[TargetModel.Create] =
    Cogen[(
      Option[Target.Id],
      String,
      Option[SiderealInput],
      Option[NonsiderealInput],
      SourceProfileInput
    )].contramap { a => (
      a.targetId,
      a.name.value,
      a.sidereal,
      a.nonsidereal,
      a.sourceProfile
    )}

  implicit val arbEditAsterismInput: Arbitrary[EditAsterismInput] =
    Arbitrary {
      for {
        a <- arbitrary[Option[Target.Id]]
        d <- arbitrary[Option[Target.Id]]
      } yield EditAsterismInput(a, d)
    }

  implicit val cogEditAsterismInput: Cogen[EditAsterismInput] =
    Cogen[(
      Option[Target.Id],
      Option[Target.Id]
    )].contramap { in => (
      in.add,
      in.delete
    )}

  implicit val arbCreateTargetEnvironmentInput: Arbitrary[TargetEnvironmentModel.Create] =
    Arbitrary {
      for {
        a <- arbitrary[Option[List[Target.Id]]]
        e <- arbitrary[Option[CoordinatesModel.Input]]
      } yield TargetEnvironmentModel.Create(a, e)
    }

  implicit val cogCreateTargetEnvironmentInput: Cogen[TargetEnvironmentModel.Create] =
    Cogen[(
      Option[List[Target.Id]],
      Option[CoordinatesModel.Input]
    )].contramap { in => (
      in.asterism,
      in.explicitBase
    )}

}

object ArbTargetModel extends ArbTargetModel
