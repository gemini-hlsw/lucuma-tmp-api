// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.util.Enumerated

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

  final case class LongInput[A, U](
    value: Long,
    units: U
  ) {

    def read(implicit U: NumericUnits[A, U]): ValidatedInput[A] =
      U.readLong(value, units)

  }

  object LongInput {

    implicit def DecoderLongInput[A, U: Enumerated]: Decoder[LongInput[A, U]] =
      deriveDecoder[LongInput[A, U]]

  }

  final case class DecimalInput[A, U](
    value: BigDecimal,
    units: U
  ) {

    def read(implicit U: NumericUnits[A, U]): ValidatedInput[A] =
      U.readDecimal(value, units)

  }

  object DecimalInput {

    implicit def DecoderDecimalInput[A, U: Enumerated]: Decoder[DecimalInput[A, U]] =
      deriveDecoder[DecimalInput[A, U]]

  }

}