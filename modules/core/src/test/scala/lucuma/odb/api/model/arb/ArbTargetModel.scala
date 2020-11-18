// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import TargetModel.{CreateNonsidereal, CreateSidereal, EditNonsidereal, EditSidereal}
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.model.{EphemerisKey, Program, Target}
import lucuma.core.model.arb.{ArbEphemerisKey, ArbTarget}
import lucuma.core.math.Epoch
import lucuma.core.math.arb.ArbEpoch
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbTargetModel {

  import ArbCatalogIdModel._
  import ArbDeclinationModel._
  import ArbEnumerated._
  import ArbEphemerisKey._
  import ArbEpoch._
  import lucuma.core.util.arb.ArbGid._
  import ArbMagnitudeModel._
  import ArbParallaxModel._
  import ArbProperMotionModel._
  import ArbRadialVelocityModel._
  import ArbRightAscensionModel._
  import ArbTarget._

  implicit val arbCreateSidereal: Arbitrary[CreateSidereal] =
    Arbitrary {
      for {
        id    <- arbitrary[Option[Target.Id]]
        pids  <- arbitrary[Option[List[Program.Id]]]
        name  <- Gen.alphaNumStr.suchThat(!_.isEmpty)
        cat   <- arbitrary[Option[CatalogIdModel.Input]]
        ra    <- arbitrary[RightAscensionModel.Input]
        dec   <- arbitrary[DeclinationModel.Input]
        epoch <- arbitrary[Option[Epoch]]
        pm    <- arbitrary[Option[ProperMotionModel.Input]]
        rv    <- arbitrary[Option[RadialVelocityModel.Input]]
        px    <- arbitrary[Option[ParallaxModel.Input]]
        mags  <- arbitrary[Option[List[MagnitudeModel.Input]]]
      } yield CreateSidereal(
        id,
        pids,
        name,
        cat,
        ra,
        dec,
        epoch,
        pm,
        None,
        rv,
        px,
        mags
      )
    }

  implicit val cogCreateSidereal: Cogen[CreateSidereal] =
    Cogen[(
      Option[Target.Id],
      Option[List[Program.Id]],
      String,
      Option[CatalogIdModel.Input],
      RightAscensionModel.Input,
      DeclinationModel.Input,
      Option[Epoch],
      Option[ProperMotionModel.Input],
      Option[RadialVelocityModel.Input],
      Option[ParallaxModel.Input],
      Option[List[MagnitudeModel.Input]]
    )].contramap { in => (
      in.targetId,
      in.programIds,
      in.name,
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
        id   <- arbitrary[Option[Target.Id]]
        pids <- arbitrary[Option[List[Program.Id]]]
        name <- Gen.alphaNumStr.suchThat(!_.isEmpty)
        key  <- arbitrary[EphemerisKeyType]
        des  <- Gen.alphaNumStr.suchThat(!_.isEmpty)
        mags <- arbitrary[Option[List[MagnitudeModel.Input]]]
      } yield CreateNonsidereal(
        id,
        pids,
        name,
        key,
        des,
        mags
      )
    }

  implicit val cogCreateNonSidereal: Cogen[CreateNonsidereal] =
    Cogen[(
      Option[Target.Id],
      Option[List[Program.Id]],
      String,
      EphemerisKeyType,
      String,
      Option[List[MagnitudeModel.Input]]
    )].contramap { in => (
      (in.targetId, in.programIds, in.name, in.key, in.des, in.magnitudes)
    )}

  implicit val arbEditSidereal: Arbitrary[EditSidereal] =
    Arbitrary {
      for {
        id    <- arbitrary[Target.Id]
        ex    <- arbitrary[Option[Existence]]
        name  <- Gen.option(Gen.alphaNumStr.suchThat(!_.isEmpty))
        cat   <- arbitrary[Option[Option[CatalogIdModel.Input]]]
        ra    <- arbitrary[Option[RightAscensionModel.Input]]
        dec   <- arbitrary[Option[DeclinationModel.Input]]
        epoch <- arbitrary[Option[Epoch]]
        pm    <- arbitrary[Option[Option[ProperMotionModel.Input]]]
        rv    <- arbitrary[Option[Option[RadialVelocityModel.Input]]]
        px    <- arbitrary[Option[Option[ParallaxModel.Input]]]
      } yield EditSidereal(
        id,
        ex,
        name,
        cat,
        ra,
        dec,
        epoch,
        pm,
        None,
        rv,
        px
      )
    }

  implicit val cogEditSidereal: Cogen[EditSidereal] =
    Cogen[(
      Target.Id,
      Option[Existence],
      Option[String],
      Option[Option[CatalogIdModel.Input]],
      Option[RightAscensionModel.Input],
      Option[DeclinationModel.Input],
      Option[Epoch],
      Option[Option[ProperMotionModel.Input]],
      Option[Option[RadialVelocityModel.Input]],
      Option[Option[ParallaxModel.Input]]
    )].contramap { in => (
      in.targetId,
      in.existence,
      in.name,
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
        id   <- arbitrary[Target.Id]
        ex   <- arbitrary[Option[Existence]]
        name <- Gen.option(Gen.alphaNumStr.suchThat(!_.isEmpty))
        key  <- arbitrary[Option[EphemerisKey]]
      } yield EditNonsidereal(
        id,
        ex,
        name,
        key
      )
    }

  implicit val cogEditNonSidereal: Cogen[EditNonsidereal] =
    Cogen[(
      Target.Id,
      Option[Existence],
      Option[String],
      Option[EphemerisKey]
    )].contramap { in => (
      (in.targetId, in.existence, in.name, in.key)
    )}

  implicit val arbTargetModel: Arbitrary[TargetModel] =
    Arbitrary {
      for {
        id <- arbitrary[Target.Id]
        ex <- arbitrary[Existence]
        tg <- arbitrary[Target]
      } yield TargetModel(id, ex, tg)
    }

  implicit val cogTargetModel: Cogen[TargetModel] =
    Cogen[(Target.Id, Existence, Target)].contramap { in =>
      (in.id, in.existence, in.target)
    }

}

object ArbTargetModel extends ArbTargetModel
