// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.laws.discipline._
import lucuma.odb.api.model.arb._
import munit._

final class ElevationRangeModelSuite extends DisciplineSuite {
  import ArbElevationRangeInput._

  // Laws
  checkAll("Eq[AirmassRangeInput]", EqTests[AirMassRangeInput].eqv)
  checkAll("Eq[HourAngleRangeInput]", EqTests[HourAngleRangeInput].eqv)
  checkAll("Eq[ElevationRangeInput]", EqTests[ElevationRangeInput].eqv)

}
