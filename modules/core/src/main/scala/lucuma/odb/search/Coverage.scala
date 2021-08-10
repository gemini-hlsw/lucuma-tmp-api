// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search

import cats.implicits._
import lucuma.core.math.Wavelength
import lucuma.odb.search.syntax.wavelength._

/** Wavelength coverage. */
sealed trait Coverage {
  import Coverage.{ Empty, Range }

  /** Intersect this `Coverage` with another. */
  def ⋂(other: Coverage): Coverage =
    (this, other) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Range(a, b), Range(aʹ, bʹ)) => Coverage(a max aʹ, b min bʹ)
    }

  /** Coverage width; i.e., difference between max and min (or zero). */
  def width: Wavelength =
    this match {
      case Empty       => Wavelength.Min
      case Range(a, b) => b - a
    }

  /** Range projection; defined when non-empty. */
  def range: Option[Coverage.Range] =
    this match {
      case Empty              => None
      case r @ Range(_, _) => Some(r)
    }

}

object Coverage {

  /** The empty `Coverage` with no bounds and a width of zero. */
  case object Empty extends Coverage

  /** Non-empty `Coverage` with upper and lower bounds. */
  sealed abstract case class Range(min: Wavelength, max: Wavelength) extends Coverage {
    require(min < max) // smart ctor should guarantee this
  }

  /** Construct a `Coverage`, empty if `min >= max`. */
  def apply(min: Wavelength, max: Wavelength): Coverage =
    if (min < max) new Range(min, max) {} else Empty

  /** Construct a `Coverage` centered at the given wavelength, with the specified width. */
  def centered(central: Wavelength, width: Wavelength): Coverage = {
    val half = Wavelength.fromPicometers.getOption(width.toPicometers.value.value / 2).get // always positive
    apply(central - half, central + half)
  }

}
