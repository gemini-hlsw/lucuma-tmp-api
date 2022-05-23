// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.all._
import eu.timepit.refined.types.all.{NonEmptyString, NonNegBigDecimal, NonNegInt, NonNegLong, PosBigDecimal, PosInt, PosLong}
import lucuma.core.syntax.string._
import monocle.Prism
import sangria.schema.{BigDecimalType, InputType, IntType, LongType, ScalarAlias, ScalarType}
import sangria.validation.ValueCoercionViolation

trait RefinedSchema {

  case object EmptyStringViolation extends ValueCoercionViolation("Expected a non-empty string")

  implicit val NonEmptyStringType: ScalarType[NonEmptyString] =
    ScalarType[NonEmptyString](
      name            =  "NonEmptyString",
      description     = Some("A String value that cannot be empty"),
      coerceUserInput = {
        case s: String  => NonEmptyString.from(s).leftMap(_ => EmptyStringViolation)
        case _          => Left(EmptyStringViolation)
      },
      coerceOutput    = (a, _) => a.value,
      coerceInput     = {
        case sangria.ast.StringValue(s, _, _, _, _) => NonEmptyString.from(s).leftMap(_ => EmptyStringViolation)
        case _                                      => Left(EmptyStringViolation)
      }
    )


  case object NonNegIntCoercionViolation extends ValueCoercionViolation("A non-negative integer is expected")

  implicit val InputTypeNonNegInt: InputType[NonNegInt] =
    ScalarAlias[NonNegInt, Int](IntType, _.value, v => NonNegInt.from(v).leftMap(_ => NonNegIntCoercionViolation))

  case object PosIntCoercionViolation extends ValueCoercionViolation("A positive integer is expected")

  implicit val InputTypePosInt: InputType[PosInt] =
    ScalarAlias[PosInt, Int](IntType, _.value, v => PosInt.from(v).leftMap(_ => PosIntCoercionViolation))

  case object NonNegLongCoercionViolation extends ValueCoercionViolation("A non-negative long is expected")

  implicit val InputTypeNonNegLong: InputType[NonNegLong] =
    ScalarAlias[NonNegLong, Long](LongType, _.value, v => NonNegLong.from(v).leftMap(_ => NonNegLongCoercionViolation))

  case object PosLongCoercionViolation extends ValueCoercionViolation("A positive long is expected")

  implicit val InputTypePosLong: InputType[PosLong] =
    ScalarAlias[PosLong, Long](LongType, _.value, v => PosLong.from(v).leftMap(_ => PosLongCoercionViolation))

  case object NonNegBigDecimalCoercionViolation extends ValueCoercionViolation("A non-negative decimal is expected")

  implicit val InputTypeNonNegBigDecimal: InputType[NonNegBigDecimal] =
    ScalarAlias[NonNegBigDecimal, BigDecimal](BigDecimalType, _.value, v => NonNegBigDecimal.from(v).leftMap(_ => NonNegBigDecimalCoercionViolation))

  case object PosBigDecimalCoercionViolation extends ValueCoercionViolation("A positive decimal is expected")

  implicit val InputTypePosBigDecimal: InputType[PosBigDecimal] =
    ScalarAlias[PosBigDecimal, BigDecimal](BigDecimalType, _.value, v => PosBigDecimal.from(v).leftMap(_ => PosBigDecimalCoercionViolation))


  case class UnsupportedTypeCoercionViolation[A](value: A) extends ValueCoercionViolation(s"Unexpected value $value of type ${value.getClass}")

  private def intType[T](
    name:         String,
    description:  String,
    prism:        Prism[Int, T],
    violation: => ValueCoercionViolation
  ): ScalarType[T] = {

    def convert(n: Int): Either[ValueCoercionViolation, T] =
      prism.getOrModify(n).leftMap(_ => violation)

    ScalarType[T](
      name            = name,
      description     = description.some,
      coerceUserInput = {
        case s: BigInt  => convert(s.intValue)
        case s: Int     => convert(s)
        case s: String  => Either.fromOption(s.parseIntOption, PosIntCoercionViolation).flatMap(convert)
        case x          => Left(UnsupportedTypeCoercionViolation(x))
      },
      coerceOutput    = (a, _) => prism.reverseGet(a),
      coerceInput     = {
        case sangria.ast.BigIntValue(s, _, _)       => convert(s.intValue)
        case sangria.ast.IntValue(s, _, _)          => convert(s)
        case sangria.ast.StringValue(s, _, _, _, _) => Either.fromOption(s.parseIntOption, PosIntCoercionViolation).flatMap(convert)
        case x                                      => Left(UnsupportedTypeCoercionViolation(x))
      }
    )

  }

  implicit val NonNegIntType: ScalarType[NonNegInt] =
    intType(
      "NonNegInt",
      s"An `Int` in the range from 0 to ${Int.MaxValue}",
      Prism(NonNegInt.unapply)(_.value),
      NonNegIntCoercionViolation
    )

  implicit val PosIntType: ScalarType[PosInt] =
    intType(
      "PosInt",
      s"An `Int` in the range from 1 to ${Int.MaxValue}",
      Prism(PosInt.unapply)(_.value),
      PosIntCoercionViolation
    )


  private def longType[T](
    name:         String,
    description:  String,
    prism:        Prism[Long, T],
    violation: => ValueCoercionViolation
  ): ScalarType[T] = {

    def convert(g: Long): Either[ValueCoercionViolation, T] =
      prism.getOrModify(g).leftMap(_ => violation)

    ScalarType[T](
      name            = name,
      description     = description.some,
      coerceUserInput = {
        case s: BigInt  => convert(s.longValue)
        case s: Int     => convert(s.toLong)
        case s: Long    => convert(s)
        case s: String  => Either.fromOption(s.parseLongOption, PosLongCoercionViolation).flatMap(convert)
        case x          => Left(UnsupportedTypeCoercionViolation(x))
      },
      coerceOutput    = (a, _) => prism.reverseGet(a),
      coerceInput     = {
        case sangria.ast.BigIntValue(s, _, _)       => convert(s.longValue)
        case sangria.ast.IntValue(s, _, _)          => convert(s.toLong)
        case sangria.ast.StringValue(s, _, _, _, _) => Either.fromOption(s.parseLongOption, PosLongCoercionViolation).flatMap(convert)
        case x                                      => Left(UnsupportedTypeCoercionViolation(x))
      }
    )

  }

  implicit val NonNegLongType: ScalarType[NonNegLong] =
    longType(
      "NonNegLong",
      s"An `Long` in the range from 0 to ${Long.MaxValue}",
      Prism(NonNegLong.unapply)(_.value),
      NonNegLongCoercionViolation
    )

  implicit val PosLongType: ScalarType[PosLong] =
    longType(
      "PosLong",
      s"An `Long` in the range from 1 to ${Long.MaxValue}",
      Prism(PosLong.unapply)(_.value),
      PosLongCoercionViolation
    )


  private def bigDecimalType[T](
    name:         String,
    description:  String,
    prism:        Prism[BigDecimal, T],
    violation: => ValueCoercionViolation
  ): ScalarType[T] = {

    def convert(b: BigDecimal): Either[ValueCoercionViolation, T] =
      prism.getOrModify(b).leftMap(_ => violation)

    ScalarType[T](
      name            = name,
      description     = description.some,
      coerceUserInput = {
        case s: BigDecimal => convert(s)
        case s: BigInt     => convert(BigDecimal(s))
        case s: Int        => convert(BigDecimal(s))
        case s: Long       => convert(BigDecimal(s))
        case s: String     => Either.fromOption(s.parseBigDecimalOption, violation).flatMap(convert)
        case x             => Left(UnsupportedTypeCoercionViolation(x))
      },
      coerceOutput    = (a, _) => prism.reverseGet(a),
      coerceInput     = {
        case sangria.ast.BigDecimalValue(s, _, _)   => convert(s)
        case sangria.ast.BigIntValue(s, _, _)       => convert(BigDecimal(s))
        case sangria.ast.IntValue(s, _, _)          => convert(BigDecimal(s))
        case sangria.ast.StringValue(s, _, _, _, _) => Either.fromOption(s.parseBigDecimalOption, violation).flatMap(convert)
        case x                                      => Left(UnsupportedTypeCoercionViolation(x))
      }
    )
  }

  implicit val NonNegBigDecimalType: ScalarType[NonNegBigDecimal] =
    bigDecimalType[NonNegBigDecimal](
      "NonNegBigDecimal",
      "A `BigDecimal` greater than or equal to 0",
      Prism(NonNegBigDecimal.unapply)(_.value),
      PosBigDecimalCoercionViolation
    )

  implicit val PosBigDecimalType: ScalarType[PosBigDecimal] =
    bigDecimalType[PosBigDecimal](
      "PosBigDecimal",
      "A `BigDecimal` greater than 0",
      Prism(PosBigDecimal.unapply)(_.value),
      PosBigDecimalCoercionViolation
    )

}

object RefinedSchema extends RefinedSchema
