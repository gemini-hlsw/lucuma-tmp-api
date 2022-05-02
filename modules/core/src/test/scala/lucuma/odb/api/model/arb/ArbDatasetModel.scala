// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid
import eu.timepit.refined.types.numeric.PosInt
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbDatasetModel {

  import ArbDatasetFilename._
  import ArbGid._
  import ArbStepModel.{arbStepId, cogStepId}

  implicit val arbDatasetModelId: Arbitrary[DatasetModel.Id] =
    Arbitrary {
      for {
        s <- arbitrary[Step.Id]
        i <- Gen.posNum[Int]
      } yield DatasetModel.Id(s, PosInt.unsafeFrom(i))
    }

  implicit val cogDatasetModelId: Cogen[DatasetModel.Id] =
    Cogen[(
      Step.Id,
      Int
    )].contramap { a => (
      a.stepId,
      a.index.value
    )}

  implicit val arbDatasetModel: Arbitrary[DatasetModel] =
    Arbitrary {
      for {
        i <- arbitrary[DatasetModel.Id]
        o <- arbitrary[Observation.Id]
        f <- arbitrary[DatasetFilename]
      } yield DatasetModel(i, o, f)
    }

  implicit val cogDatasetModel: Cogen[DatasetModel] =
    Cogen[(
      DatasetModel.Id,
      Observation.Id,
      DatasetFilename
    )].contramap { a => (
      a.id,
      a.observationId,
      a.filename
    )}

}

object ArbDatasetModel extends ArbDatasetModel
