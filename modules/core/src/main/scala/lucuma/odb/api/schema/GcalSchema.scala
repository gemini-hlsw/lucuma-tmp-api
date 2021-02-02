// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`._
import lucuma.odb.api.model.GcalModel
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._


object GcalSchema {

  import FiniteDurationSchema._
  import syntax.`enum`._

  implicit val EnumTypeGcalContinuum: EnumType[GcalContinuum] =
    EnumType.fromEnumerated(
      "GcalContinuum",
      "GCAL continuum"
    )

  implicit val EnumTypeGcalArc: EnumType[GcalArc] =
    EnumType.fromEnumerated(
      "GcalArc",
      "GCAL arc"
    )

  implicit val EnumTypeGcalFilter: EnumType[GcalFilter] =
    EnumType.fromEnumerated(
      "GcalFilter",
      "GCAL filter"
    )

  implicit val EnumTypeGcalDiffuser: EnumType[GcalDiffuser] =
    EnumType.fromEnumerated(
      "GcalDiffuser",
      "GCAL diffuser"
    )

  implicit val EnumTypeGcalShutter: EnumType[GcalShutter] =
    EnumType.fromEnumerated(
      "GcalShutter",
      "GCAL shutter"
    )

  def GcalType[F[_]: Effect]: ObjectType[OdbRepo[F], GcalModel] =
    ObjectType(
      name     = "Gcal",
      fieldsFn = () => fields(

        Field(
          name        = "continuum",
          fieldType   = OptionType(EnumTypeGcalContinuum),
          description = Some("GCAL continuum, present if no arcs are used"),
          resolve     = _.value.lamp.swap.toOption
        ),

        Field(
          name        = "arcs",
          fieldType   = ListType(EnumTypeGcalArc),
          description = Some("GCAL arcs, one or more present if no continuum is used"),
          resolve     = _.value.lamp.toOption.toList.flatMap(_.toList)
        ),

        Field(
          name        = "filter",
          fieldType   = EnumTypeGcalFilter,
          description = Some("GCAL filter"),
          resolve     = _.value.filter
        ),

        Field(
          name        = "diffuser",
          fieldType   = EnumTypeGcalDiffuser,
          description = Some("GCAL diffuser"),
          resolve     = _.value.diffuser
        ),

        Field(
          name        = "shutter",
          fieldType   = EnumTypeGcalShutter,
          description = Some("GCAL shutter"),
          resolve     = _.value.shutter
        ),

        Field(
          name        = "exposure",
          fieldType   = DurationType[F],
          description = Some("GCAL exposure time"),
          resolve     = _.value.exposureTime
        ),

        Field(
          name        = "coadds",
          fieldType   = IntType,
          description = Some("GCAL coadds"),
          resolve     = _.value.coadds.toPosShort.value.toInt
        )

      )
    )

}
