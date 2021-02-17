// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._

import cats.kernel.laws.discipline.EqTests
import monocle.law.discipline._
import munit.DisciplineSuite


final class ManualSequenceSuite extends DisciplineSuite {

  import ArbManualSequence._
  import ArbSequenceModel._

  checkAll("ManualSequence",        EqTests[ManualSequence[Int, String]].eqv)
  checkAll("ManualSequence.Create", EqTests[ManualSequence.Create[Int, String]].eqv)

  checkAll("GmosModel.North.static",      LensTests(ManualSequence.static[Int, String]))
  checkAll("GmosModel.North.acquisition", LensTests(ManualSequence.acquisition[Int, String]))
  checkAll("GmosModel.North.science",     LensTests(ManualSequence.science[Int, String]))

}
