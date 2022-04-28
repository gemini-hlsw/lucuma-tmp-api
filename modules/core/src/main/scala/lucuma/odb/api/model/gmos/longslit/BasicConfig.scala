// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import cats.Eq
import monocle.{Focus, Lens}

/**
 * BasicConfig options that match the science requirements are listed in a
 * table in Explore.  The user selects one, but may subsequently choose to
 * override (or refine details) in an AdvancedConfig without losing any
 * information in the original BasicConfig.
 */
final case class BasicConfig[G, F, U](
  grating: G,
  filter:  Option[F],
  fpu:     U
)

object BasicConfig extends BasicConfigOptics {

  implicit def EqBasicConfig[G: Eq, F: Eq, U: Eq]: Eq[BasicConfig[G, F, U]] =
    Eq.by { a => (
      a.grating,
      a.filter,
      a.fpu
    )}

}

sealed trait BasicConfigOptics { self: BasicConfig.type =>

  def grating[G, F, U]: Lens[BasicConfig[G, F, U], G] =
    Focus[BasicConfig[G, F, U]](_.grating)

  def filter[G, F, U]: Lens[BasicConfig[G, F, U], Option[F]] =
    Focus[BasicConfig[G, F, U]](_.filter)

  def fpu[G, F, U]: Lens[BasicConfig[G, F, U], U] =
    Focus[BasicConfig[G, F, U]](_.fpu)

}
