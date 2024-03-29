// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated

sealed abstract class ConfigurationMode(val name: String) extends Product with Serializable

object ConfigurationMode {
  case object GmosNorthLongSlit extends ConfigurationMode("GMOS North Long Slit")
  case object GmosSouthLongSlit extends ConfigurationMode("GMOS South Long Slit")

  implicit val ConfigurationModeEnumerated: Enumerated[ConfigurationMode] =
    Enumerated.of(GmosNorthLongSlit, GmosSouthLongSlit)
}

