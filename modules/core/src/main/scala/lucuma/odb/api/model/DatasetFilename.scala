// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.Site
import lucuma.odb.api.model.time.FourDigitYearLocalDate
import lucuma.core.optics.Format
import atto._
import Atto._
import cats.Order
import io.chrisdavenport.cats.time.instances.localdate._
import eu.timepit.refined.types.all.PosInt
import io.circe.Decoder

/**
 * Describes the components of a valid dataset filename.
 */
final case class DatasetFilename(
  site:      Site,
  localDate: FourDigitYearLocalDate,
  index:     PosInt
) {

  /**
   * Formats the dataset filename.  For example, N20210521S0123.fits.
   */
  def format: String =
    f"${site.tag.last}${localDate.value.getYear}%04d${localDate.value.getMonth.getValue}%02d${localDate.value.getDayOfMonth}%02dS${index.value}%04d.fits"

}

object DatasetFilename {

  private def fixed(n: Int): Parser[Int] =
    count(n, digit).map(_.mkString).flatMap { s =>
      try ok(s.toInt) catch { case e: NumberFormatException => err(e.toString) }
    }

  val parser: Parser[DatasetFilename] =
    for {
      s  <- oneOf("NS").map(c => Site.unsafeFromTag(s"G$c"))
      y  <- fixed(4)
      m  <- fixed(2)
      d  <- fixed(2)
      ld <- FourDigitYearLocalDate.fromYMD(y, m, d).fold(err[FourDigitYearLocalDate](s"invalid date $y-$m-$d"))(ok)
      _  <- char('S')
      n  <- int.filter(_ > 0)
      _  <- string(".fits")
    } yield new DatasetFilename(s, ld, PosInt.unsafeFrom(n))

  val fromString: Format[String, DatasetFilename] =
    Format(s => parser.parse(s).option, _.format)

  implicit val OrderDatasetFilename: Order[DatasetFilename] =
    Order.by { a => (
      a.site,
      a.localDate.value,
      a.index.value
    )}

  implicit val DecoderDatasetFilename: Decoder[DatasetFilename] =
    Decoder[String].emap(s => fromString.getOption(s).toRight(s"Could not parse '$s' as a dataset filename"))

}

