// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.all._
import eu.timepit.refined.types.all.PosBigDecimal
import eu.timepit.refined.types.all.PosInt
import sangria.schema.{InputType, IntType, BigDecimalType, ScalarAlias}
import sangria.validation.ValueCoercionViolation

trait RefinedSchema {

  case object PosIntCoercionViolation extends ValueCoercionViolation("A positive integer is expected")

  implicit val InputObjectPosInt: InputType[PosInt] =
    ScalarAlias[PosInt, Int](IntType, _.value, v => PosInt.from(v).leftMap(_ => PosIntCoercionViolation))

  case object PosBigDecimalCoercionViolation extends ValueCoercionViolation("A positive decimal is expected")

  implicit val InputObjectPosBigDecimal: InputType[PosBigDecimal] =
    ScalarAlias[PosBigDecimal, BigDecimal](BigDecimalType, _.value, v => PosBigDecimal.from(v).leftMap(_ => PosBigDecimalCoercionViolation))

}

object RefinedSchema extends RefinedSchema
