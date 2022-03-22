// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import eu.timepit.refined.types.all.PosInt
import eu.timepit.refined.scalacheck.all._
import lucuma.odb.api.model.Step
import lucuma.odb.api.model.arb._
import lucuma.odb.api.schema.Paging.Cursor
import lucuma.odb.api.schema.arb._
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


final class DatasetSchemaSuite extends DisciplineSuite {

  import ArbCursor._
  import ArbUid._

  implicit val arbStepAndIndex: Arbitrary[DatasetSchema.StepAndIndex] =
    Arbitrary {
      for {
        s <- arbitrary[Step.Id]
        i <- arbitrary[PosInt]
      } yield DatasetSchema.StepAndIndex(s, i)
    }

  implicit val cogStepAndIndex: Cogen[DatasetSchema.StepAndIndex] =
    Cogen[(
      Step.Id,
      PosInt
    )].contramap { a => (
      a.stepId,
      a.index
    )}

  implicit val arbStepIndexCursor: Arbitrary[Cursor] =
    arbCursor[DatasetSchema.StepAndIndex]

  checkAll("StepAndIndexCursor", PrismTests(DatasetSchema.StepAndIndexCursor))

}
