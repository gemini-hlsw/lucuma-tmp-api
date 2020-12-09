// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.core.`enum`.Instrument
import monocle.Lens

sealed abstract class ConfigModel[I](val instrument: I) extends Product with Serializable


object ConfigModel {

  final case class GmosNorth(
    static:      GmosModel.NorthStatic,
    acquisition: List[StepModel[GmosModel.NorthDynamic]],
    science:     List[StepModel[GmosModel.NorthDynamic]]
  ) extends ConfigModel(Instrument.GmosN)

  object GmosNorth {

    implicit val EqGmosNorth: Eq[GmosNorth] =
      Eq.by { a => (
        a.static,
        a.acquisition,
        a.science
      )}

  }

  sealed trait GmosNorthOptics { this: GmosNorth.type =>

    val static: Lens[GmosNorth, GmosModel.NorthStatic] =
      Lens[GmosNorth, GmosModel.NorthStatic](_.static)(a => _.copy(static = a))

    val acquisition: Lens[GmosNorth, List[StepModel[GmosModel.NorthDynamic]]] =
      Lens[GmosNorth, List[StepModel[GmosModel.NorthDynamic]]](_.acquisition)(a => _.copy(acquisition = a))

  }

  final case class GmosSouth(
    static:      GmosModel.SouthStatic,
    acquisition: List[GmosModel.SouthDynamic],
    science:     List[GmosModel.SouthDynamic]
  ) extends ConfigModel(Instrument.GmosS)

}
