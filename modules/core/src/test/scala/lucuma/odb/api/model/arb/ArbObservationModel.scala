// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{Asterism, Observation, Program, Target}
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}

import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonEmptyString
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbObservationModel {

  import ArbConstraintSetModel._
  import ArbScienceRequirements._
  import ArbEnumerated._
  import ArbGid._

  def arbObservationModelWithPid(pid: Program.Id): Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
        os <- arbitrary[ObsStatus]
        as <- arbitrary[ObsActiveStatus]
        ts <- arbitrary[Option[Either[Asterism.Id, Target.Id]]]
        cs <- arbitrary[ConstraintSetModel]
        sr <- arbitrary[ScienceRequirements]
      } yield ObservationModel(id, ex, pid, nm, os, as, ts, cs, sr, PlannedTimeSummaryModel.Zero, None)
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
      Option[Either[Asterism.Id, Target.Id]],
      ConstraintSetModel,
      ScienceRequirements
    )].contramap { in => (
      in.id,
      in.existence,
      in.programId,
      in.name.map(_.value),
      in.status,
      in.activeStatus,
      in.pointing,
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
        ts <- arbitrary[Option[Either[Asterism.Id, Target.Id]]]
        cs <- arbitrary[Option[ConstraintSetModel.Create]]
      } yield ObservationModel.Create(
        id,
        pd,
        nm,
        st,
        as,
        ts.flatMap(_.swap.toOption),
        ts.flatMap(_.toOption),
        cs,
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
      Option[Asterism.Id],
      Option[Target.Id],
      Option[ConstraintSetModel.Create]
    )].contramap { in => (
      in.observationId,
      in.programId,
      in.name.map(_.value),
      in.status,
      in.activeStatus,
      in.asterismId,
      in.targetId,
      in.constraintSet
    )}

}

object ArbObservationModel extends ArbObservationModel
