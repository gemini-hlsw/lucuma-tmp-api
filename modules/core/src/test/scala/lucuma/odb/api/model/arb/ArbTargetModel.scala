// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.math.{Coordinates, Epoch}
import lucuma.core.math.arb.{ArbCoordinates, ArbEpoch}
import lucuma.core.model.arb.ArbEphemerisKey
import lucuma.core.model.{EphemerisKey, Observation, Program, Target, TargetEnvironment}
import lucuma.core.model.arb.ArbTarget
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import lucuma.odb.api.model.targetModel._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._


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
        vid <- arbitrary[TargetEnvironment.Id]
        t   <- arbitrary[Target]
      } yield TargetModel(id, vid, t)
    }

  implicit val cogTargetModel: Cogen[TargetModel] =
    Cogen[(
      Target.Id,
      TargetEnvironment.Id,
      Target
    )].contramap { in => (
      in.id,
      in.targetEnvironmentId,
      in.target
    )}

  implicit val arbTargetEnvironmentModel: Arbitrary[TargetEnvironmentModel] =
    Arbitrary {
      for {
        id  <- arbitrary[TargetEnvironment.Id]
        pid <- arbitrary[Program.Id]
        oid <- arbitrary[Option[Observation.Id]]
        b   <- arbitrary[Option[Coordinates]]
      } yield TargetEnvironmentModel(id, pid, oid, b)
    }

  implicit val cogTargetEnvironmentModel: Cogen[TargetEnvironmentModel] =
    Cogen[(
      TargetEnvironment.Id,
      Program.Id,
      Option[Observation.Id],
      Option[Coordinates]
    )].contramap { in => (
      in.id,
      in.programId,
      in.observationId,
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

  implicit val arbCreateTarget: Arbitrary[CreateTargetInput] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[CreateNonsiderealInput].map(CreateTargetInput.nonsidereal),
        arbitrary[CreateSiderealInput].map(CreateTargetInput.sidereal)
      )
    }

  implicit val cogCreateTarget: Cogen[CreateTargetInput] =
    Cogen[(
      Option[CreateNonsiderealInput],
      Option[CreateSiderealInput]
    )].contramap { in => (
      in.nonsidereal,
      in.sidereal
    )}

  implicit val arbSelectTargetInput: Arbitrary[SelectTargetInput] =
    Arbitrary {
      for {
        ns <- arbitrary[Option[List[NonEmptyString]]]
        ts <- arbitrary[Option[List[Target.Id]]]
      } yield SelectTargetInput(ns, ts)
    }

  implicit val cogSelectTargetInput: Cogen[SelectTargetInput] =
    Cogen[(
      Option[List[String]],
      Option[List[Target.Id]]
    )].contramap { in => (
      in.names.map(_.map(_.value)),
      in.targetIds
    )}

  implicit val arbEditSidereal: Arbitrary[EditSiderealInput] =
    Arbitrary {
      for {
        sel   <- arbitrary[SelectTargetInput]
        name  <- arbitrary[Input[NonEmptyString]]
        cat   <- arbitrary[Input[CatalogIdModel.Input]]
        ra    <- arbNotNullableInput[RightAscensionModel.Input].arbitrary
        dec   <- arbNotNullableInput[DeclinationModel.Input].arbitrary
        epoch <- arbNotNullableInput[Epoch].arbitrary
        pm    <- arbitrary[Input[ProperMotionModel.Input]]
        rv    <- arbitrary[Input[RadialVelocityModel.Input]]
        px    <- arbitrary[Input[ParallaxModel.Input]]
        mags  <- arbitrary[Option[MagnitudeModel.EditList]]
      } yield EditSiderealInput(
        sel,
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

  implicit val cogEditSidereal: Cogen[EditSiderealInput] =
    Cogen[(
      SelectTargetInput,
      Input[String],
      Input[CatalogIdModel.Input],
      Input[RightAscensionModel.Input],
      Input[DeclinationModel.Input],
      Input[Epoch],
      Input[ProperMotionModel.Input],
      Input[RadialVelocityModel.Input],
      Input[ParallaxModel.Input]
    )].contramap { in => (
      in.select,
      in.name.map(_.value),
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
        sel  <- arbitrary[SelectTargetInput]
        name <- arbitrary[Input[NonEmptyString]]
        key  <- arbitrary[Input[EphemerisKey]]
      } yield EditNonsiderealInput(
        sel,
        name,
        key
      )
    }

  implicit val cogEditNonSidereal: Cogen[EditNonsiderealInput] =
    Cogen[(
      SelectTargetInput,
      Input[String],
      Input[EphemerisKey]
    )].contramap { in => (
      in.select,
      in.name.map(_.value),
      in.key
    )}

  implicit val arbEditTargetInput: Arbitrary[EditAsterismInput] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[CreateSiderealInput].map(EditAsterismInput.addSidereal),
        arbitrary[CreateNonsiderealInput].map(EditAsterismInput.addNonsidereal),
        arbitrary[EditSiderealInput].map(EditAsterismInput.editSidereal),
        arbitrary[EditNonsiderealInput].map(EditAsterismInput.editNonsidereal),
        arbitrary[SelectTargetInput].map(EditAsterismInput.delete)
      )
    }

  implicit val cogEditTargetInput: Cogen[EditAsterismInput] =
    Cogen[(
      Option[CreateSiderealInput],
      Option[CreateNonsiderealInput],
      Option[EditSiderealInput],
      Option[EditNonsiderealInput],
      Option[SelectTargetInput]
    )].contramap { in => (
      in.newSidereal,
      in.newNonsidereal,
      in.editSidereal,
      in.editNonsidereal,
      in.delete
    )}

  implicit val arbCreateTargetEnvironmentInput: Arbitrary[CreateTargetEnvironmentInput] =
    Arbitrary {
      for {
        i <- arbitrary[Option[TargetEnvironment.Id]]
        b <- arbitrary[Option[CoordinatesModel.Input]]
        s <- arbitrary[Option[List[CreateTargetInput]]]
      } yield CreateTargetEnvironmentInput(i, b, s)
    }

  implicit val cogCreateTargetEnvironmentInput: Cogen[CreateTargetEnvironmentInput] =
    Cogen[(
      Option[CoordinatesModel.Input],
      Option[List[CreateTargetInput]]
    )].contramap { in => (
      in.explicitBase,
      in.asterism
    )}

  implicit val arbSelectTargetEnvironmentInput: Arbitrary[SelectTargetEnvironmentInput] =
    Arbitrary {
      for {
        a <- arbitrary[Option[Program.Id]]
        p <- arbitrary[Option[Program.Id]]
        o <- arbitrary[Option[List[Observation.Id]]]
        t <- arbitrary[Option[List[TargetEnvironment.Id]]]
      } yield SelectTargetEnvironmentInput(a, p, o, t)
    }

  implicit val cogSelectTargetEnvironmentInput: Cogen[SelectTargetEnvironmentInput] =
    Cogen[(
      Option[Program.Id],
      Option[Program.Id],
      Option[List[Observation.Id]],
      Option[List[TargetEnvironment.Id]]
    )].contramap { in => (
      in.all,
      in.program,
      in.observations,
      in.targetEnvironments
    )}

  implicit val arbBulkEditTargetInput: Arbitrary[BulkEditTargetInput] =
    Arbitrary {
      for {
        sel <- arbitrary[Option[SelectTargetEnvironmentInput]]
        as  <- arbitrary[Option[CreateSiderealInput]]
        an  <- arbitrary[Option[CreateNonsiderealInput]]
        es  <- arbitrary[Option[EditSiderealInput]]
        en  <- arbitrary[Option[EditNonsiderealInput]]
        dl  <- arbitrary[Option[SelectTargetInput]]
      } yield BulkEditTargetInput(sel, as, an, es, en, dl)
    }

  implicit val cogBulkEditTargetInput: Cogen[BulkEditTargetInput] =
    Cogen[(
      Option[SelectTargetEnvironmentInput],
      Option[CreateSiderealInput],
      Option[CreateNonsiderealInput],
      Option[EditSiderealInput],
      Option[EditNonsiderealInput],
      Option[SelectTargetInput]
    )].contramap { in => (
      in.select,
      in.addSidereal,
      in.addNonsidereal,
      in.editSidereal,
      in.editNonsidereal,
      in.delete
    )}

  implicit val arbBulkEditTargetListInput: Arbitrary[BulkEditTargetListInput] =
    Arbitrary {
      for {
        s <- arbitrary[Option[SelectTargetEnvironmentInput]]
        e <- arbitrary[List[EditAsterismInput]]
      } yield BulkEditTargetListInput(s, e)
    }

  implicit val cogBulkEditTargetListInput: Cogen[BulkEditTargetListInput] =
    Cogen[(
      Option[SelectTargetEnvironmentInput],
      List[EditAsterismInput]
    )].contramap { in => (
      in.select,
      in.edits
    )}

  implicit val arbBulkEditTargetEnvironmentInput: Arbitrary[BulkEditTargetEnvironmentInput] =
    Arbitrary {
      for {
        s <- arbitrary[SelectTargetEnvironmentInput]
        e <- arbitrary[Input[CoordinatesModel.Input]]
      } yield BulkEditTargetEnvironmentInput(s, e)
    }

  implicit val cogBulkEditTargetEnvironmentInput: Cogen[BulkEditTargetEnvironmentInput] =
    Cogen[(
      SelectTargetEnvironmentInput,
      Input[CoordinatesModel.Input]
    )].contramap { in => (
      in.select,
      in.explicitBase
    )}
}

object ArbTargetModel extends ArbTargetModel
