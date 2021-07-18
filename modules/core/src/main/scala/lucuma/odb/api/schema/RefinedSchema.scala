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

  case class UnsupportedTypeCoercionViolation[A](value: A) extends ValueCoercionViolation(s"Unexpected value $value of type ${value.getClass}")
  implicit val PosIntType: ScalarType[PosInt] =
    ScalarType[PosInt](
      name            =  "PosInt",
      description     = Some("An `Int` in the range from 1 to `Int.MaxValue`"),
      coerceUserInput = {
        case s: BigInt  =>
          PosInt.from(s.intValue).leftMap(_ => PosIntCoercionViolation)
        case s: Int  =>
          PosInt.from(s).leftMap(_ => PosIntCoercionViolation)
        case x       =>
          Left(UnsupportedTypeCoercionViolation(x))
      },
      coerceOutput    = (a, _) => a.value,
      coerceInput     = {
        case sangria.ast.BigIntValue(s, _, _) =>
          PosInt.from(s.intValue).leftMap(_ => PosIntCoercionViolation)
        case sangria.ast.IntValue(s, _, _)    =>
          PosInt.from(s).leftMap(_ => PosIntCoercionViolation)
        case x                                =>
          Left(UnsupportedTypeCoercionViolation(x))
      }
    )

  implicit val PosBigDecimalType: ScalarType[PosBigDecimal] =
    ScalarType[PosBigDecimal](
      name            =  "PosBigDecimal",
      description     = Some("A `BigDecimal` greater than 0"),
      coerceUserInput = {
        case s: BigDecimal => PosBigDecimal.from(s).leftMap(_ => PosBigDecimalCoercionViolation)
        case x             =>
          Left(UnsupportedTypeCoercionViolation(x))
      },
      coerceOutput    = (a, _) => a.value,
      coerceInput     = {
        case sangria.ast.BigDecimalValue(s, _, _) => PosBigDecimal.from(s).leftMap(_ => PosBigDecimalCoercionViolation)
        case x                                =>
          Left(UnsupportedTypeCoercionViolation(x))
      }
    )
}

object RefinedSchema extends RefinedSchema
