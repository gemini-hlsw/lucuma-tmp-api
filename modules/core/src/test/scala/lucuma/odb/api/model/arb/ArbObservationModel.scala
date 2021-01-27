// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.{Asterism, Observation, Program, Target}
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import eu.timepit.refined.types.all.NonEmptyString
import lucuma.core.`enum`.ObsStatus
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbObservationModel {

  import ArbEnumerated._
  import ArbGid._

  def arbObservationModelWithPid(pid: Program.Id): Arbitrary[ObservationModel] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        ex <- arbitrary[Existence]
        nm <- Gen.option(Gen.alphaNumStr.suchThat(!_.isEmpty).map(NonEmptyString.unsafeFrom))
        os <- arbitrary[ObsStatus]
        ts <- arbitrary[Option[Either[Asterism.Id, Target.Id]]]
      } yield ObservationModel(id, ex, pid, nm, os, ts, PlannedTimeSummaryModel.Zero, None)
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
      Option[Either[Asterism.Id, Target.Id]]
    )].contramap { in => (
      in.id,
      in.existence,
      in.programId,
      in.name.map(_.value),
      in.status,
      in.targets
    )}

}

object ArbObservationModel extends ArbObservationModel
