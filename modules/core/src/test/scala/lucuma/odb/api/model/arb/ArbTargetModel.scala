// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.arb.ArbEphemerisKey
import lucuma.core.model.{EphemerisKey, Program, Target}
import lucuma.core.model.arb.ArbTarget
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import lucuma.odb.api.model.targetModel._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import scala.collection.immutable.SortedSet


trait ArbTargetModel {

  import ArbCatalogIdModel._
  import ArbCoordinates._
  import ArbCoordinatesModel._
  import ArbDeclinationModel._
  import ArbEnumerated._
  import ArbEphemerisKey._
  import ArbEpoch._
  import ArbGid._
  import ArbInput._
  import ArbMagnitudeModel._
  import ArbParallaxModel._
  import ArbProperMotionModel._
  import ArbRadialVelocityModel._
  import ArbRightAscensionModel._
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

  implicit val arbCreateSidereal: Arbitrary[CreateSiderealInput] =
    Arbitrary {
      for {
        name  <- arbitrary[NonEmptyString]
        cat   <- arbitrary[Option[CatalogIdModel.Input]]
        ra    <- arbitrary[RightAscensionModel.Input]
        dec   <- arbitrary[DeclinationModel.Input]
        epoch <- arbitrary[Option[Epoch]]
        pm    <- arbitrary[Option[ProperMotionModel.Input]]
        rv    <- arbitrary[Option[RadialVelocityModel.Input]]
        px    <- arbitrary[Option[ParallaxModel.Input]]
        mags  <- arbitrary[Option[List[MagnitudeModel.Create]]]
      } yield CreateSiderealInput(
        name,
        cat,
        ra,
        dec,
        epoch,
        pm,
        rv,
        px,
        mags
      )
    }

  implicit val cogCreateSidereal: Cogen[CreateSiderealInput] =
    Cogen[(
      String,
      Option[CatalogIdModel.Input],
      RightAscensionModel.Input,
      DeclinationModel.Input,
      Option[Epoch],
      Option[ProperMotionModel.Input],
      Option[RadialVelocityModel.Input],
      Option[ParallaxModel.Input],
      Option[List[MagnitudeModel.Create]]
    )].contramap { in => (
      in.name.value,
      in.catalogId,
      in.ra,
      in.dec,
      in.epoch,
      in.properMotion,
      in.radialVelocity,
      in.parallax,
      in.magnitudes
    )}

  implicit val arbCreateNonSidereal: Arbitrary[CreateNonsiderealInput] =
    Arbitrary {
      for {
        name <- arbitrary[NonEmptyString]
        key  <- arbitrary[EphemerisKeyType]
        des  <- arbitrary[NonEmptyString]
        mags <- arbitrary[Option[List[MagnitudeModel.Create]]]
      } yield CreateNonsiderealInput(
        name,
        key,
        des.value,
        mags
      )
    }

  implicit val cogCreateNonSidereal: Cogen[CreateNonsiderealInput] =
    Cogen[(
      String,
      EphemerisKeyType,
      String,
      Option[List[MagnitudeModel.Create]]
    )].contramap { in => (
      in.name.value, in.keyType, in.des, in.magnitudes
    )}

  implicit val arbCreateTarget: Arbitrary[TargetModel.Create] =
    Arbitrary {
      for {
        t <- arbitrary[Option[Target.Id]]
        p <- arbitrary[Program.Id]
        s <- arbitrary[Option[CreateSiderealInput]]
        n <- arbitrary[Option[CreateNonsiderealInput]]
      } yield TargetModel.Create(t, p, s, n)
    }

  implicit val cogCreateTarget: Cogen[TargetModel.Create] =
    Cogen[(
      Option[Target.Id],
      Program.Id,
      Option[CreateSiderealInput],
      Option[CreateNonsiderealInput]
    )].contramap { in => (
      in.targetId,
      in.programId,
      in.sidereal,
      in.nonsidereal
    )}

//          name  <- arbitrary[Input[NonEmptyString]]

  implicit val arbEditSidereal: Arbitrary[EditSiderealInput] =
    Arbitrary {
      for {
        cat   <- arbitrary[Input[CatalogIdModel.Input]]
        ra    <- arbNotNullableInput[RightAscensionModel.Input].arbitrary
        dec   <- arbNotNullableInput[DeclinationModel.Input].arbitrary
        epoch <- arbNotNullableInput[Epoch].arbitrary
        pm    <- arbitrary[Input[ProperMotionModel.Input]]
        rv    <- arbitrary[Input[RadialVelocityModel.Input]]
        px    <- arbitrary[Input[ParallaxModel.Input]]
        mags  <- arbitrary[Option[MagnitudeModel.EditList]]
      } yield EditSiderealInput(
        cat,
        ra,
        dec,
        epoch,
        pm,
        rv,
        px,
        mags
      )
    }

  implicit val cogEditSidereal: Cogen[EditSiderealInput] =
    Cogen[(
      Input[CatalogIdModel.Input],
      Input[RightAscensionModel.Input],
      Input[DeclinationModel.Input],
      Input[Epoch],
      Input[ProperMotionModel.Input],
      Input[RadialVelocityModel.Input],
      Input[ParallaxModel.Input]
    )].contramap { in => (
      in.catalogId,
      in.ra,
      in.dec,
      in.epoch,
      in.properMotion,
      in.radialVelocity,
      in.parallax
    )}

  implicit val arbEditNonSidereal: Arbitrary[EditNonsiderealInput] =
    Arbitrary {
      for {
        key  <- arbitrary[Input[EphemerisKey]]
      } yield EditNonsiderealInput(
        key
      )
    }

  implicit val cogEditNonSidereal: Cogen[EditNonsiderealInput] =
    Cogen[(
      Input[EphemerisKey]
    )].contramap { in => (
      in.key
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
