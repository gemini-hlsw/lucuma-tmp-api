// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.ObsStatus
import lucuma.core.model.{Asterism, Observation, Program, Target}
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}

import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonEmptyString
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbObservationModel {

  import ArbConstraintSetModel._
  import ArbEnumerated._
  import ArbGid._

  def arbObservationModelWithPid(pid: Program.Id): Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
        os <- arbitrary[ObsStatus]
        ts <- arbitrary[Option[Either[Asterism.Id, Target.Id]]]
        cs <- arbitrary[ConstraintSetModel]
      } yield ObservationModel(id, ex, pid, nm, os, ts, cs, PlannedTimeSummaryModel.Zero, None)
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
      Option[Either[Asterism.Id, Target.Id]],
      ConstraintSetModel
    )].contramap { in => (
      in.id,
      in.existence,
      in.programId,
      in.name.map(_.value),
      in.status,
      in.pointing,
      in.constraintSet
    )}

  implicit val arbObservationModelCreate: Arbitrary[ObservationModel.Create] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Observation.Id]]
        pd <- arbitrary[Program.Id]
        nm <- arbitrary[Option[NonEmptyString]]
        ts <- arbitrary[Option[Either[Asterism.Id, Target.Id]]]
        cs <- arbitrary[Option[ConstraintSetModel.Create]]
        st <- arbitrary[Option[ObsStatus]]
      } yield ObservationModel.Create(
        id,
        pd,
        nm,
        ts.flatMap(_.swap.toOption),
        ts.flatMap(_.toOption),
        cs,
        st,
        None
      )
    }

  implicit val cogObservationModelCreate: Cogen[ObservationModel.Create] =
    Cogen[(
      Option[Observation.Id],
      Program.Id,
      Option[String],
      Option[Asterism.Id],
      Option[Target.Id],
      Option[ConstraintSetModel.Create]
    )].contramap { in => (
      in.observationId,
      in.programId,
      in.name.map(_.value),
      in.asterismId,
      in.targetId,
      in.constraintSet
    )}

}

object ArbObservationModel extends ArbObservationModel
