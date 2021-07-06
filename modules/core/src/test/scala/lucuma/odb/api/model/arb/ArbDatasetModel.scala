// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.{Observation, Step}
import lucuma.core.util.arb.ArbGid

import eu.timepit.refined.types.numeric.PosInt
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbDatasetModel {

  import ArbDatasetFilename._
  import ArbGid._

  implicit val arbDatasetModel: Arbitrary[DatasetModel] =
    Arbitrary {
      for {
        s <- arbitrary[Step.Id]
        i <- Gen.posNum[Int]
        o <- arbitrary[Observation.Id]
        f <- arbitrary[DatasetFilename]
      } yield DatasetModel(s, PosInt.unsafeFrom(i), o, f)
    }

  implicit val cogDatasetModel: Cogen[DatasetModel] =
    Cogen[(
      Step.Id,
      Int,
      Observation.Id,
      DatasetFilename
    )].contramap { a => (
      a.stepId,
      a.index.value,
      a.observationId,
      a.filename
    )}

}

object ArbDatasetModel extends ArbDatasetModel
