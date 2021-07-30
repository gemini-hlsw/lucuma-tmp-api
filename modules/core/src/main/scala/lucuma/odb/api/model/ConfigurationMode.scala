// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated

sealed abstract class ConfigurationMode extends Product with Serializable

object ConfigurationMode {
  case object GmosNorthLongSlit extends ConfigurationMode
  case object GmosSouthLongSlit extends ConfigurationMode

  implicit val ConfigurationModeEnumerated: Enumerated[ConfigurationMode] =
    Enumerated.of(GmosNorthLongSlit, GmosSouthLongSlit)
}

