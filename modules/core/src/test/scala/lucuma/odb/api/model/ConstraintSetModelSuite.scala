// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.kernel.laws.discipline._
import lucuma.core.util.arb._
import lucuma.odb.api.model.arb._
import munit._

final class ConstraintSetModelSuite extends DisciplineSuite {
  import ArbConstraintSetInput._
  import ArbEnumerated._

  checkAll("Eq[ConstraintSetInput]", EqTests[ConstraintSetInput].eqv)

}
