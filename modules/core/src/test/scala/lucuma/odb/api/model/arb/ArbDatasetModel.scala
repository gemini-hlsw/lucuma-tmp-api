// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.model.{Observation, Step}
import lucuma.core.util.arb.ArbGid

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant

trait ArbDatasetModel {

  import ArbDatasetFilename._
  import ArbGid._
  import ArbTime._

  implicit val arbDatasetModel: Arbitrary[DatasetModel] =
    Arbitrary {
      for {
        d <- arbitrary[Dataset.Id]
        o <- arbitrary[Observation.Id]
        s <- arbitrary[Step.Id]
        i <- arbitrary[Instant]
        f <- arbitrary[DatasetFilename]
      } yield DatasetModel(d, o, s, i, f)
    }

  implicit val cogDatasetModel: Cogen[DatasetModel] =
    Cogen[(
      Dataset.Id,
      Observation.Id,
      Step.Id,
      Instant,
      DatasetFilename
    )].contramap { a => (
      a.id,
      a.observationId,
      a.stepId,
      a.timestamp,
      a.filename
    )}

  implicit val arbDatasetModelCreate: Arbitrary[DatasetModel.Create] =
    Arbitrary {
      for {
        d <- arbitrary[Option[Dataset.Id]]
        o <- arbitrary[Observation.Id]
        s <- arbitrary[Step.Id]
        t <- arbitrary[Instant]
        f <- arbitrary[DatasetFilename]
      } yield DatasetModel.Create(d, o, s, t, f)
    }

  implicit val cogDatasetModelCreate: Cogen[DatasetModel.Create] =
    Cogen[(
      Option[Dataset.Id],
      Observation.Id,
      Step.Id,
      Instant,
      DatasetFilename
    )].contramap { a => (
      a.datasetId,
      a.observationId,
      a.stepId,
      a.timestamp,
      a.filename
    )}

}

object ArbDatasetModel extends ArbDatasetModel
