// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.`enum`.DatasetQaState
import lucuma.core.model.Observation
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbDatasetModel {

  import ArbEnumerated._
  import ArbDatasetFilename._
  import ArbGid._
  import ArbStepModel.{arbStepId, cogStepId}

  implicit val arbDatasetModelId: Arbitrary[DatasetModel.Id] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        s <- arbitrary[Step.Id]
        i <- Gen.posNum[Int]
      } yield DatasetModel.Id(o, s, PosInt.unsafeFrom(i))
    }

  implicit val cogDatasetModelId: Cogen[DatasetModel.Id] =
    Cogen[(
      Observation.Id,
      Step.Id,
      Int
    )].contramap { a => (
      a.observationId,
      a.stepId,
      a.index.value
    )}

  implicit val arbDataset: Arbitrary[DatasetModel.Dataset] =
    Arbitrary {
      for {
        f <- arbitrary[DatasetFilename]
        q <- arbitrary[Option[DatasetQaState]]
      } yield DatasetModel.Dataset(f, q)
    }

  implicit val cogDataset: Cogen[DatasetModel.Dataset] =
    Cogen[(
      DatasetFilename,
      Option[DatasetQaState]
    )].contramap { a => (
      a.filename,
      a.qaState
    )}


  implicit val arbDatasetModel: Arbitrary[DatasetModel] =
    Arbitrary {
      for {
        i <- arbitrary[DatasetModel.Id]
        d <- arbitrary[DatasetModel.Dataset]
      } yield DatasetModel(i, d)
    }

  implicit val cogDatasetModel: Cogen[DatasetModel] =
    Cogen[(
      DatasetModel.Id,
      DatasetModel.Dataset
    )].contramap { a => (
      a.id,
      a.dataset
    )}

}

object ArbDatasetModel extends ArbDatasetModel
