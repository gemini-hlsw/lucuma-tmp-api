// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Angle
import lucuma.core.optics.SplitMono
import lucuma.core.util.{Display, Enumerated}

import cats.syntax.validated._

import scala.math.BigDecimal.RoundingMode.HALF_UP



object AngleModel {

  sealed abstract class Units(
    val symbol:       Option[String],
    val abbreviation: String,
    val name:         String,
    val µas:          Long
  ) extends Product with Serializable {

    private def longFromMicroarcseconds(m: SplitMono[Angle, Long]): SplitMono[Angle, Long] =
      m.imapB(
        c => c * µas,
        u => (u + math.signum(u) * µas/2) / µas
      )

    val signedLong: SplitMono[Angle, Long] =
      longFromMicroarcseconds(Angle.signedMicroarcseconds)

    val unsignedLong: SplitMono[Angle, Long] =
      longFromMicroarcseconds(Angle.microarcseconds)

    private def readLong(l: Long, m: SplitMono[Angle, Long]): ValidatedInput[Angle] =
      m.reverseGet(l).validNec[InputError]

    def readSignedLong(l: Long): ValidatedInput[Angle] =
      readLong(l, signedLong)

    def readUnsignedLong(l: Long): ValidatedInput[Angle] =
      readLong(l, unsignedLong)

    private def decimalFromMicroarcseconds(m: SplitMono[Angle, Long]): SplitMono[Angle, BigDecimal] =
      m.imapB(
        b => ((b * µas).setScale(0, HALF_UP).toBigInt % Units.µas360).longValue,
        n => BigDecimal(n) / µas
      )

    val signedDecimal: SplitMono[Angle, BigDecimal] =
      decimalFromMicroarcseconds(Angle.signedMicroarcseconds)

    val unsignedDecimal: SplitMono[Angle, BigDecimal] =
      decimalFromMicroarcseconds(Angle.microarcseconds)

    private def readDecimal(d: BigDecimal, m: SplitMono[Angle, BigDecimal]): ValidatedInput[Angle] =
      m.reverseGet(d).validNec[InputError]

    def readSignedDecimal(d: BigDecimal): ValidatedInput[Angle] =
      readDecimal(d, signedDecimal)

    def readUnsignedDecimal(d: BigDecimal): ValidatedInput[Angle] =
      readDecimal(d, unsignedDecimal)

  }

  object Units {

    private val µas360: Long =
      Angle.Angle180.toMicroarcseconds * 2L

    case object Microarcseconds extends Units(None,      "µas",  "microarcseconds",          1L)
    case object Microseconds    extends Units(None,      "µs",   "microseconds",            15L)
    case object Milliarcseconds extends Units(None,      "mas",  "milliarcseconds",       1000L)
    case object Milliseconds    extends Units(None,      "ms",   "milliseconds",         15000L)
    case object Arcseconds      extends Units(Some("″"), "asec", "arcseconds",         1000000L)
    case object Seconds         extends Units(None,      "sec",  "seconds",           15000000L)
    case object Arcminutes      extends Units(Some("′"), "amin", "arcminutes",        60000000L)
    case object Minutes         extends Units(None,      "min",  "minutes",          900000000L)
    case object Degrees         extends Units(Some("º"), "deg",  "degrees",         3600000000L)
    case object Hours           extends Units(None,      "hrs",  "hours",          54000000000L)

    implicit val EnumeratedAngleUnits: Enumerated[Units] =
      Enumerated.of(
        Microarcseconds,
        Microseconds,
        Milliarcseconds,
        Milliseconds,
        Arcseconds,
        Seconds,
        Arcminutes,
        Minutes,
        Degrees,
        Hours
      )

    implicit val DisplayAngleUnits: Display[Units] =
      Display.by(_.abbreviation, _.name)

  }

  /*
  def readHms(s: String): ValidatedInput[RightAscension] =
    RightAscension.fromStringHMS
      .getOption(s)
      .toValidNec(
        InputError.fromMessage(s"Could not parse $s as an HMS string.")
      )

  def readDms(s: String): ValidatedInput[Angle] =
    Angle.fromStringSignedDMS
      .getOption(s)
      .toValidNec(
        InputError.fromMessage(s"Could not parse $s as a signed DMS string.")
      )

  case class LongAngleInput(
    value: Long,
    units: Units
  ) {

    def read: ValidatedInput[Angle] =
      units.readSignedLong(value)

  }

  case class DecimalAngleInput(
    value: BigDecimal,
    units: Units
  ) {

    def read: ValidatedInput[Angle] =
      units.readSignedDecimal(value)

  }

  case class SignedAngleInput(
    microarcseconds: Option[Long],
    microseconds:    Option[BigDecimal],
    milliarcseconds: Option[BigDecimal],
    milliseconds:    Option[BigDecimal],
    arcseconds:      Option[BigDecimal],
    seconds:         Option[BigDecimal],
    arcminutes:      Option[BigDecimal],
    minutes:         Option[BigDecimal],
    degrees:         Option[BigDecimal],
    hours:           Option[BigDecimal],
    dms:             Option[String],
    hms:             Option[String],
    fromLong:        Option[LongAngleInput],
    fromDecimal:     Option[DecimalAngleInput]
  ) {

    import Units._

    private val decimalInputs: List[(Option[BigDecimal], Units)] = List(
      microseconds    -> Microseconds,
      milliarcseconds -> Milliarcseconds,
      milliseconds    -> Milliseconds,
      arcseconds      -> Arcseconds,
      seconds         -> Seconds,
      arcminutes      -> Arcminutes,
      minutes         -> Minutes,
      degrees         -> Degrees,
      hours           -> Hours
    )

    val toAngle: ValidatedInput[Angle] =
      ValidatedInput.requireOne("angle",
        dms            .map(readDms)                        ::
        hms            .map(s => readHms(s).map(_.toAngle)) ::
        fromLong       .map(_.read)                         ::
        fromDecimal    .map(_.read)                         ::
        microarcseconds.map(Microarcseconds.readSignedLong) ::
          decimalInputs.map { case (input, units) => input.map(units.readSignedDecimal)}
      )

  }
*/

}
