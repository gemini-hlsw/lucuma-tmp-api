// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import TargetModel.{Create, CreateNonsidereal, CreateSidereal, Edit, EditNonsidereal, EditSidereal, EditTargetAction, EditTargetList}
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.model.EphemerisKey
import lucuma.core.model.arb.ArbEphemerisKey
import lucuma.core.math.Epoch
import lucuma.core.math.arb.ArbEpoch
import lucuma.core.util.arb.ArbEnumerated

import clue.data.Input
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbTargetModel {

  import ArbCatalogIdModel._
  import ArbDeclinationModel._
  import ArbEnumerated._
  import ArbEphemerisKey._
  import ArbEpoch._
  import ArbInput._
  import ArbMagnitudeModel._
  import ArbParallaxModel._
  import ArbProperMotionModel._
  import ArbRadialVelocityModel._
  import ArbRightAscensionModel._

  implicit val arbCreateSidereal: Arbitrary[CreateSidereal] =
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
      } yield CreateSidereal(
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

  implicit val cogCreateSidereal: Cogen[CreateSidereal] =
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

  implicit val arbCreateNonSidereal: Arbitrary[CreateNonsidereal] =
    Arbitrary {
      for {
        name <- arbitrary[NonEmptyString]
        key  <- arbitrary[EphemerisKeyType]
        des  <- arbitrary[NonEmptyString]
        mags <- arbitrary[Option[List[MagnitudeModel.Create]]]
      } yield CreateNonsidereal(
        name,
        key,
        des.value,
        mags
      )
    }

  implicit val cogCreateNonSidereal: Cogen[CreateNonsidereal] =
    Cogen[(
      String,
      EphemerisKeyType,
      String,
      Option[List[MagnitudeModel.Create]]
    )].contramap { in => (
      in.name.value, in.key, in.des, in.magnitudes
    )}

  implicit val arbCreateTarget: Arbitrary[Create] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[CreateNonsidereal].map(Create.nonsidereal),
        arbitrary[CreateSidereal].map(Create.sidereal)
      )
    }

  implicit val cogCreateTarget: Cogen[Create] =
    Cogen[(
      Option[CreateNonsidereal],
      Option[CreateSidereal]
    )].contramap { in => (
      in.nonsidereal,
      in.sidereal
    )}

  implicit val arbEditSidereal: Arbitrary[EditSidereal] =
    Arbitrary {
      for {
        name  <- arbitrary[Input[NonEmptyString]]
        cat   <- arbitrary[Input[CatalogIdModel.Input]]
        ra    <- arbNotNullableInput[RightAscensionModel.Input].arbitrary
        dec   <- arbNotNullableInput[DeclinationModel.Input].arbitrary
        epoch <- arbNotNullableInput[Epoch].arbitrary
        pm    <- arbitrary[Input[ProperMotionModel.Input]]
        rv    <- arbitrary[Input[RadialVelocityModel.Input]]
        px    <- arbitrary[Input[ParallaxModel.Input]]
        mags  <- arbitrary[Option[MagnitudeModel.EditList]]
      } yield EditSidereal(
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

  implicit val cogEditSidereal: Cogen[EditSidereal] =
    Cogen[(
      Input[String],
      Input[CatalogIdModel.Input],
      Input[RightAscensionModel.Input],
      Input[DeclinationModel.Input],
      Input[Epoch],
      Input[ProperMotionModel.Input],
      Input[RadialVelocityModel.Input],
      Input[ParallaxModel.Input]
    )].contramap { in => (
      in.name.map(_.value),
      in.catalogId,
      in.ra,
      in.dec,
      in.epoch,
      in.properMotion,
      in.radialVelocity,
      in.parallax
    )}

  implicit val arbEditNonSidereal: Arbitrary[EditNonsidereal] =
    Arbitrary {
      for {
        name <- arbitrary[Input[NonEmptyString]]
        key  <- arbitrary[Input[EphemerisKey]]
      } yield EditNonsidereal(
        name,
        key
      )
    }

  implicit val cogEditNonSidereal: Cogen[EditNonsidereal] =
    Cogen[(
      Input[String],
      Input[EphemerisKey]
    )].contramap { in => (
      in.name.map(_.value),
      in.key
    )}

  implicit val arbEditTarget: Arbitrary[Edit] =
    Arbitrary {
      for {
        sel <- arbitrary[NonEmptyString]
        ed  <- Gen.oneOf(
                arbitrary[EditNonsidereal].map(Edit.nonsidereal(sel, _)),
                arbitrary[EditSidereal].map(Edit.sidereal(sel, _))
              )
      } yield ed
    }

  implicit val cogEditTarget: Cogen[Edit] =
    Cogen[(
      String,
      Option[EditNonsidereal],
      Option[EditSidereal]
    )].contramap { in => (
      in.selectTarget.value,
      in.nonsidereal,
      in.sidereal
    )}

  implicit val arbEditTargetAction: Arbitrary[EditTargetAction] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[Create].map(EditTargetAction.add),
        arbitrary[NonEmptyString].map(EditTargetAction.delete),
        arbitrary[Edit].map(EditTargetAction.edit)
      )

    }

  implicit val cogEditTargetAction: Cogen[EditTargetAction] =
    Cogen[(
      Option[Create],
      Option[String],
      Option[Edit]
    )].contramap { in => (
      in.add,
      in.delete.map(_.value),
      in.edit
    )}

  implicit val arbEditTargetList: Arbitrary[EditTargetList] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[List[TargetModel.Create]].map(EditTargetList.replace),
        arbitrary[List[EditTargetAction]].map(EditTargetList.edit)
      )
    }

  implicit val cogEditTargetList: Cogen[EditTargetList] =
    Cogen[(
      Option[List[TargetModel.Create]],
      Option[List[EditTargetAction]]
    )].contramap { in => (
      in.replaceList,
      in.editList
    )}

}

object ArbTargetModel extends ArbTargetModel
