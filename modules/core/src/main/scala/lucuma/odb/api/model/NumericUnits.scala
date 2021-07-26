// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated

import cats.Eq
import io.circe.Decoder
import io.circe.generic.semiauto._

trait NumericUnits[A, U] {

  def readLong(value: Long, units: U): ValidatedInput[A]

  def readDecimal(value: BigDecimal, units: U): ValidatedInput[A]

}

object NumericUnits {

  def apply[A, U](implicit ev: NumericUnits[A, U]): ev.type = ev

  def fromRead[A, U](
    longToA:    (U, Long) => ValidatedInput[A],
    decimalToA: (U, BigDecimal) => ValidatedInput[A]
  ): NumericUnits[A, U] =
    new NumericUnits[A, U] {
      override def readLong(value: Long, units: U): ValidatedInput[A] =
        longToA(units, value)

      override def readDecimal(value: BigDecimal, units: U): ValidatedInput[A] =
        decimalToA(units, value)
    }

  final case class LongInput[U](
    value: Long,
    units: U
  ) {

    def read[A](implicit U: NumericUnits[A, U]): ValidatedInput[A] =
      U.readLong(value, units)

  }

  object LongInput {

    implicit def DecoderLongInput[U: Enumerated]: Decoder[LongInput[U]] =
      deriveDecoder[LongInput[U]]

    implicit def EqLongInput[U: Eq]: Eq[LongInput[U]] =
      Eq.by(li => (li.value, li.units))

  }

  final case class DecimalInput[U](
    value: BigDecimal,
    units: U
  ) {

    def read[A](implicit U: NumericUnits[A, U]): ValidatedInput[A] =
      U.readDecimal(value, units)

  }

  object DecimalInput {

    implicit def DecoderDecimalInput[U: Enumerated]: Decoder[DecimalInput[U]] =
      deriveDecoder[DecimalInput[U]]

    implicit def EqDecimalInput[U: Eq]: Eq[DecimalInput[U]] =
      Eq.by(li => (li.value, li.units))

  }

}
