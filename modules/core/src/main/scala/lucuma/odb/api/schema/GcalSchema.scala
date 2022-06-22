// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.enums._
import lucuma.odb.api.model.GcalModel
import sangria.schema._

object GcalSchema {

  import syntax.`enum`._

  implicit val EnumTypeGcalContinuum: EnumType[GcalContinuum] =
    EnumType.fromEnumerated(
      "GcalContinuum",
      "GCAL Continuum"
    )

  implicit val EnumTypeGcalArc: EnumType[GcalArc] =
    EnumType.fromEnumerated(
      "GcalArc",
      "GCAL Arc"
    )

  implicit val EnumTypeGcalFilter: EnumType[GcalFilter] =
    EnumType.fromEnumerated(
      "GcalFilter",
      "GCAL Filter"
    )

  implicit val EnumTypeGcalDiffuser: EnumType[GcalDiffuser] =
    EnumType.fromEnumerated(
      "GcalDiffuser",
      "GCAL Diffuser"
    )

  implicit val EnumTypeGcalShutter: EnumType[GcalShutter] =
    EnumType.fromEnumerated(
      "GcalShutter",
      "GCAL Shutter"
    )

  val InputObjectTypeGcalModelCreate: InputObjectType[GcalModel.Create] =
    InputObjectType[GcalModel.Create](
      "GcalConfigurationInput",
      "GCAL configuration creation input",
      List(
        InputField("continuum", EnumTypeGcalContinuum.optional),
        InputField("arcs",      EnumTypeGcalArc.list),
        InputField("diffuser",  EnumTypeGcalDiffuser),
        InputField("shutter",   EnumTypeGcalShutter)
      )
    )

}
