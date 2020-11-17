// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck.{Cogen, Gen}

trait GenNumericUnitsInput {

  import NumericUnits.{DecimalInput, LongInput}

  /*
  def genLongInput[U: Arbitrary](
    min: Long = Long.MinValue,
    max: Long = Long.MaxValue
  ): Gen[LongInput[U]] =
    for {
      n <- Gen.choose(min, max)
      u <- arbitrary[U]
    } yield LongInput[U](n, u)

  def genLongInput[U: Arbitrary](
    g: Gen[Long]
  ): Gen[LongInput[U]] =
    for {
      n <- g
      u <- arbitrary[U]
    } yield LongInput[U](n, u)

  def genBigDecimal(
    genLong: Gen[Long],
    minMoveLeft: Int,
    maxMoveLeft: Int
  ): Gen[BigDecimal] =
    for {
      l <- genLong
      m <- Gen.choose(minMoveLeft, maxMoveLeft)
    } yield BigDecimal(BigDecimal(l).underlying.movePointLeft(m))

  def genDecimalInput[U: Arbitrary](
    g: Gen[BigDecimal]
  ): Gen[DecimalInput[U]] =
    for {
      n <- g
      u <- arbitrary[U]
    } yield DecimalInput[U](n, u)

  */

  def genLongInputFromLong[U](g: Gen[Long], u: U): Gen[LongInput[U]] =
    g.map(l => LongInput[U](l, u))

  def genLongInputFromDecimal[U](g: Gen[BigDecimal], u: U): Gen[LongInput[U]] =
    g.map(d => LongInput[U](d.toLong, u))

  def genDecimalInputFromLong[U](g: Gen[Long], u: U): Gen[DecimalInput[U]] =
    g.map(l => DecimalInput[U](l.toDouble, u))

  def genDecimalInputFromDecimal[U](g: Gen[BigDecimal], u: U): Gen[DecimalInput[U]] =
    g.map(d => DecimalInput[U](d, u))

  implicit def cogLongInput[U: Cogen]: Cogen[LongInput[U]] =
    Cogen[(Long, U)].contramap { li =>
      (li.value, li.units)
    }

  implicit def cogDecimalInput[U: Cogen]: Cogen[DecimalInput[U]] =
    Cogen[(BigDecimal, U)].contramap { di =>
      (di.value, di.units)
    }

}

object GenNumericUnitsInput extends GenNumericUnitsInput
