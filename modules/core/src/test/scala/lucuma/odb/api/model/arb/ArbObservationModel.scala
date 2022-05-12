// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{ConstraintSet, Observation, Program}
import lucuma.core.model.arb.ArbConstraintSet
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonEmptyString
import lucuma.odb.api.model.targetModel.{TargetEnvironmentInput, TargetEnvironmentModel}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbObservationModel {

  import ArbConstraintSet._
  import ArbConstraintSetInput._
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
        ts <- arbitrary[TargetEnvironmentModel]
        cs <- arbitrary[ConstraintSet]
        sr <- arbitrary[ScienceRequirements]
      } yield ObservationModel(id, ex, pid, nm, os, as, ts, cs, sr, None, None)
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
      Existence,
      Program.Id,
      Option[String],
      ObsStatus,
      ObsActiveStatus,
      ConstraintSet,
      ScienceRequirements
    )].contramap { in => (
      in.id,
      in.existence,
      in.programId,
      in.subtitle.map(_.value),
      in.status,
      in.activeStatus,
      in.constraintSet,
      in.scienceRequirements
    )}

  implicit val arbObservationModelCreate: Arbitrary[ObservationModel.Create] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Observation.Id]]
        pd <- arbitrary[Program.Id]
        nm <- arbitrary[Option[NonEmptyString]]
        st <- arbitrary[Option[ObsStatus]]
        as <- arbitrary[Option[ObsActiveStatus]]
        ts <- arbitrary[Option[TargetEnvironmentInput]]
        cs <- arbitrary[Option[ConstraintSetInput]]
      } yield ObservationModel.Create(
        id,
        pd,
        nm,
        st,
        as,
        ts,
        cs,
        None,
        None,
        None
      )
    }

  implicit val cogObservationModelCreate: Cogen[ObservationModel.Create] =
    Cogen[(
      Option[Observation.Id],
      Program.Id,
      Option[String],
      Option[ObsStatus],
      Option[ObsActiveStatus],
      Option[TargetEnvironmentInput],
      Option[ConstraintSetInput]
    )].contramap { in => (
      in.observationId,
      in.programId,
      in.subtitle.map(_.value),
      in.status,
      in.activeStatus,
      in.targetEnvironment,
      in.constraintSet
    )}

  implicit val arbObservationModelCloneInput: Arbitrary[ObservationModel.CloneInput] =
    Arbitrary {
      for {
        ex <- arbitrary[Observation.Id]
        sg <- arbitrary[Option[Observation.Id]]
        pd <- arbitrary[Option[Program.Id]]
        nm <- arbitrary[Option[NonEmptyString]]
        st <- arbitrary[Option[ObsStatus]]
        as <- arbitrary[Option[ObsActiveStatus]]
        ts <- arbitrary[Option[TargetEnvironmentInput]]
        cs <- arbitrary[Option[ConstraintSetInput]]
      } yield ObservationModel.CloneInput(
        ex,
        sg,
        pd,
        nm,
        st,
        as,
        ts,
        cs,
        None,
        None,
        None
      )
    }

  implicit val cogObservationModelCloneInput: Cogen[ObservationModel.CloneInput] =
    Cogen[(
      Observation.Id,
      Option[Observation.Id],
      Option[Program.Id],
      Option[String],
      Option[ObsStatus],
      Option[ObsActiveStatus],
      Option[TargetEnvironmentInput],
      Option[ConstraintSetInput]
    )].contramap { in => (
      in.existingObservationId,
      in.suggestedCloneId,
      in.programId,
      in.subtitle.map(_.value),
      in.status,
      in.activeStatus,
      in.targetEnvironment,
      in.constraintSet
    )}
}

object ArbObservationModel extends ArbObservationModel
