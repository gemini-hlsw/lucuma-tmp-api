// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos

import cats.effect.Sync
import lucuma.core.`enum`._
import lucuma.odb.api.model.GmosModel.{NorthDynamic, NorthStatic}
import lucuma.odb.api.model.ExecutionModel


trait GmosNorthGenerator[F[_]] extends Generator[F, NorthStatic, NorthDynamic] {

  override def instrument: Instrument =
    Instrument.GmosNorth

}

object GmosNorthGenerator {

  def manual[F[_]: Sync](config: ExecutionModel.GmosNorth): Generator[F, NorthStatic, NorthDynamic] =
    Generator.manual(config.instrument, config.config)

}
