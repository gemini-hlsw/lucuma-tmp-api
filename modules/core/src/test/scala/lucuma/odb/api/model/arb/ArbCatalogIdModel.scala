// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.CatalogId
import lucuma.core.model.arb.ArbCatalogId._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbCatalogIdModel {

  implicit val arbCatalogIdModelInput: Arbitrary[CatalogIdModel.Input] =
    Arbitrary {
      arbitrary[CatalogId].map(id => CatalogIdModel.Input(id.catalog, id.id.value))
    }

  implicit val cogCatalogIdModelInput: Cogen[CatalogIdModel.Input] =
    Cogen[CatalogId].contramap(i => CatalogId(i.name, i.id).get)
}

object ArbCatalogIdModel extends ArbCatalogIdModel
