// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import org.scalacheck.Gen

// Sequences have some number of atoms, each composed of some number of
// acquisition and some number of science steps.  Allowing arbitrary nested list
// lengths creates long test run times. These helpers limit the sizes to keep
// them reasonable.

trait Helper {

  def tinyPositiveSize: Gen[Int] =
    Gen.chooseNum(1, 3)

  def tinySize: Gen[Int] =
    Gen.chooseNum(0, 3)

  def smallSize: Gen[Int] =
    Gen.chooseNum(0, 6)

}
