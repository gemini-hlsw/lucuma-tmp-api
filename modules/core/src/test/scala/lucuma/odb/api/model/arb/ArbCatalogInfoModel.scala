// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.CatalogInfo
import lucuma.core.model.arb.ArbCatalogInfo._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbCatalogInfoModel {

  implicit val arbCatalogIdModelInput: Arbitrary[CatalogInfoModel.Input] =
    Arbitrary {
      arbitrary[CatalogInfo].map(id => CatalogInfoModel.Input(id.catalog, id.id.value))
    }

  implicit val cogCatalogIdModelInput: Cogen[CatalogInfoModel.Input] =
    Cogen[CatalogInfo].contramap(i => CatalogInfo(i.name, i.id).get)
}

object ArbCatalogInfoModel extends ArbCatalogInfoModel
