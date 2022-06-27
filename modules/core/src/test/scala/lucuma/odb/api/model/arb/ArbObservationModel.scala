// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import lucuma.core.enums.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{ConstraintSet, Observation, Program}
import lucuma.core.model.arb.ArbConstraintSet
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonEmptyString
import lucuma.odb.api.model.targetModel.{TargetEnvironmentInput, TargetEnvironmentModel}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant

trait ArbObservationModel {

  import ArbInput._
  import ArbConstraintSet._
  import ArbConstraintSetInput._
  import ArbPosAngleConstraint._
  import ArbScienceRequirements._
  import ArbEnumerated._
  import ArbGid._
  import ArbTargetModel._

  def arbObservationModelWithPid(pid: Program.Id): Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
        os <- arbitrary[ObsStatus]
        as <- arbitrary[ObsActiveStatus]
        vt <- arbitrary[Option[Instant]]
        pc <- arbitrary[Option[PosAngleConstraint]]
        ts <- arbitrary[TargetEnvironmentModel]
        cs <- arbitrary[ConstraintSet]
        sr <- arbitrary[ScienceRequirements]
      } yield ObservationModel(id, pid, ex, nm, os, as, vt, pc, ts, cs, sr, None, None)
    }

  implicit val arbObservationModel: Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        p <- arbitrary[Program.Id]
        o <- arbObservationModelWithPid(p).arbitrary
      } yield o
    }

  implicit val cogObservationModel: Cogen[ObservationModel] =
    Cogen[(
      Observation.Id,
      Program.Id,
      Existence,
      Option[String],
      ObsStatus,
      ObsActiveStatus,
      Option[Instant],
      Option[PosAngleConstraint],
      TargetEnvironmentModel,
      ConstraintSet,
      ScienceRequirements
    )].contramap { in => (
      in.id,
      in.programId,
      in.existence,
      in.subtitle.map(_.value),
      in.status,
      in.activeStatus,
      in.visualizationTime,
      in.posAngleConstraint,
      in.targetEnvironment,
      in.constraintSet,
      in.scienceRequirements
    )}

  implicit val arbObservationModelPropertiesInput: Arbitrary[ObservationModel.PropertiesInput] =
    Arbitrary {
      for {
        nm <- arbitrary[Input[NonEmptyString]]
        st <- arbitrary[Input[ObsStatus]]
        as <- arbitrary[Input[ObsActiveStatus]]
        vt <- arbitrary[Input[Instant]]
        pc <- arbitrary[Input[PosAngleConstraintInput]]
        ts <- arbitrary[Input[TargetEnvironmentInput]]
        cs <- arbitrary[Input[ConstraintSetInput]]
      } yield ObservationModel.PropertiesInput(
        nm,
        st,
        as,
        vt,
        pc,
        ts,
        cs,
        Input.ignore,
        Input.ignore,
        Input.ignore
      )
    }

  implicit val cogObservationModelPropertiesInput: Cogen[ObservationModel.PropertiesInput] =
    Cogen[(
      Input[String],
      Input[ObsStatus],
      Input[ObsActiveStatus],
      Input[Instant],
      Input[PosAngleConstraintInput],
      Input[TargetEnvironmentInput],
      Input[ConstraintSetInput]
    )].contramap { in => (
      in.subtitle.map(_.value),
      in.status,
      in.activeStatus,
      in.visualizationTime,
      in.posAngleConstraint,
      in.targetEnvironment,
      in.constraintSet
    )}

  implicit val arbObservationModelCreate: Arbitrary[ObservationModel.CreateInput] =
    Arbitrary {
      for {
        pd <- arbitrary[Program.Id]
        pr <- arbitrary[Option[ObservationModel.PropertiesInput]]
      } yield ObservationModel.CreateInput(pd, pr)
    }

  implicit val cogObservationModelCreate: Cogen[ObservationModel.CreateInput] =
    Cogen[(
      Program.Id,
      Option[ObservationModel.PropertiesInput]
    )].contramap { in => (
      in.programId,
      in.properties
    )}

  implicit val arbObservationModelCloneInput: Arbitrary[ObservationModel.CloneInput] =
    Arbitrary {
      for {
        ex <- arbitrary[Observation.Id]
        pi <- arbitrary[Option[ObservationModel.PropertiesInput]]
      } yield ObservationModel.CloneInput(ex, pi)
    }

  implicit val cogObservationModelCloneInput: Cogen[ObservationModel.CloneInput] =
    Cogen[(
      Observation.Id,
      Option[ObservationModel.PropertiesInput]
    )].contramap { in => (
      in.observationId,
      in.SET
    )}
}

object ArbObservationModel extends ArbObservationModel
