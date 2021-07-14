// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.all._
import eu.timepit.refined.types.all.PosBigDecimal
import eu.timepit.refined.types.all.PosInt
import sangria.schema.{InputType, IntType, BigDecimalType, ScalarAlias, ScalarType}
import sangria.validation.ValueCoercionViolation

trait RefinedSchema {

  case object PosIntCoercionViolation extends ValueCoercionViolation("A positive integer is expected")

  implicit val InputObjectPosInt: InputType[PosInt] =
    ScalarAlias[PosInt, Int](IntType, _.value, v => PosInt.from(v).leftMap(_ => PosIntCoercionViolation))

  case object PosBigDecimalCoercionViolation extends ValueCoercionViolation("A positive decimal is expected")

  implicit val InputObjectPosBigDecimal: InputType[PosBigDecimal] =
    ScalarAlias[PosBigDecimal, BigDecimal](BigDecimalType, _.value, v => PosBigDecimal.from(v).leftMap(_ => PosBigDecimalCoercionViolation))

  final case object EmptyStringViolation extends ValueCoercionViolation("Expected a non-empty string")

  implicit val PosIntType: ScalarType[PosInt] =
    ScalarType[PosInt](
      name            =  "PosInt",
      description     = Some("An `Int` in the range from 1 to `Int.MaxValue`"),
      coerceUserInput = {
        case s: Int  => PosInt.from(s).leftMap(_ => PosIntCoercionViolation)
        case _       => Left(PosIntCoercionViolation)
      },
      coerceOutput    = (a, _) => a.value,
      coerceInput     = {
        case sangria.ast.IntValue(s, _, _) => PosInt.from(s).leftMap(_ => EmptyStringViolation)
        case _                             => Left(PosIntCoercionViolation)
      }
    )

  implicit val PosBigDecimalType: ScalarType[PosBigDecimal] =
    ScalarType[PosBigDecimal](
      name            =  "PosBigDecimal",
      description     = Some("A `BigDecimal` greater than 0"),
      coerceUserInput = {
        case s: Int  => PosBigDecimal.from(s).leftMap(_ => PosBigDecimalCoercionViolation)
        case _       => Left(PosBigDecimalCoercionViolation)
      },
      coerceOutput    = (a, _) => a.value,
      coerceInput     = {
        case sangria.ast.BigDecimalValue(s, _, _) => PosBigDecimal.from(s).leftMap(_ => EmptyStringViolation)
        case _                             => Left(PosBigDecimalCoercionViolation)
      }
    )
}

object RefinedSchema extends RefinedSchema
