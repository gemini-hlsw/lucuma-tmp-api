// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`._

import sangria.schema._


object InstrumentSchema {

  import syntax.`enum`._

  implicit val EnumTypeMosPreImaging: EnumType[MosPreImaging] =
    EnumType.fromEnumerated(
      "MosPreImaging",
      "MOS pre-imaging observation"
    )

}
