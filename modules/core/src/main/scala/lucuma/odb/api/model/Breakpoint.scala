// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

import monocle.Iso


sealed trait Breakpoint extends Product with Serializable {

  def enabled: Boolean =
    this match {
      case Breakpoint.Enabled  => true
      case Breakpoint.Disabled => false
    }

}

object Breakpoint {

  case object Enabled  extends Breakpoint
  case object Disabled extends Breakpoint

  val enabled: Breakpoint =
    Enabled

  val disabled: Breakpoint =
    Disabled

  val fromBoolean: Iso[Boolean, Breakpoint] =
    Iso[Boolean, Breakpoint](b => if (b) Enabled else Disabled)(_.enabled)

  implicit val EnumeratedBreakpoint: Enumerated[Breakpoint] =
    Enumerated.of(enabled, disabled)

  implicit val DecoderBreakpoint: Decoder[Breakpoint] =
    deriveDecoder[Breakpoint]

}
