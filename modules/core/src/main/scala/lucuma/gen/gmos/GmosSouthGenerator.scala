// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos

import cats.effect.Sync
import lucuma.core.`enum`._
import lucuma.odb.api.model.GmosModel.{SouthDynamic, SouthStatic}
import lucuma.odb.api.model.ExecutionModel


trait GmosSouthGenerator[F[_]] extends Generator[F, SouthStatic, SouthDynamic] {

  override def instrument: Instrument =
    Instrument.GmosSouth

}

object GmosSouthGenerator {

  def manual[F[_]: Sync](config: ExecutionModel.GmosSouth): Generator[F, SouthStatic, SouthDynamic] =
    Generator.manual(config.instrument, config.config)

}


