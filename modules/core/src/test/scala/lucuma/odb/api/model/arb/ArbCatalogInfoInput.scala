// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import clue.data.Input
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.all._
import lucuma.core.`enum`.CatalogName
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.CatalogInfoInput
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbCatalogInfoInput {

  import ArbEnumerated._
  import ArbInput._

  implicit val arbCatalogIdModelInput: Arbitrary[CatalogInfoInput] =
    Arbitrary {
      for {
        n <- arbitrary[Input[CatalogName]]
        i <- arbitrary[Input[NonEmptyString]]
        t <- arbitrary[Input[NonEmptyString]]
      } yield CatalogInfoInput(n, i, t)
    }

  implicit val cogCatalogIdModelInput: Cogen[CatalogInfoInput] =
    Cogen[(
      Input[CatalogName],
      Input[NonEmptyString],
      Input[NonEmptyString]
    )].contramap { a => (
      a.name,
      a.id,
      a.objectType
    )}

}

object ArbCatalogInfoInput extends ArbCatalogInfoInput
