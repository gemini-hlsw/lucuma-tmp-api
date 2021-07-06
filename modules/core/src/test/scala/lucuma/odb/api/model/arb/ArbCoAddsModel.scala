// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck._


trait ArbCoAddsModel {

  implicit val arbCoAddsModel: Arbitrary[CoAddsModel] =
    Arbitrary {
      Gen.choose(1.toShort, Short.MaxValue)
        .map(CoAddsModel.fromShort.getOption)
        .map(_.get)
    }

  implicit val cogCoAddsModel: Cogen[CoAddsModel] =
    Cogen[Short].contramap(_.toPosShort.value)

  implicit val arbCoAddsModelInput: Arbitrary[CoAddsModel.Input] =
    Arbitrary {
      Gen.choose(1, Short.MaxValue.toInt)
        .map(CoAddsModel.Input.apply)
    }

  implicit val cogCoAddsModelInput: Cogen[CoAddsModel.Input] =
    Cogen[Int].contramap(_.coadds)

}

object ArbCoAddsModel extends ArbCoAddsModel
