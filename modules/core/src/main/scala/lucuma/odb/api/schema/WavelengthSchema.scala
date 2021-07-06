// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.math.Wavelength
import lucuma.odb.api.model.WavelengthModel
import lucuma.odb.api.repo.OdbRepo
import sangria.macros.derive._
import sangria.schema._

import java.math.RoundingMode.HALF_UP

object WavelengthSchema {
  import NumericUnitsSchema._
  import syntax.enum._

  def WavelengthType[F[_]]: ObjectType[OdbRepo[F], Wavelength] =
    ObjectType(
      name     = "Wavelength",
      fieldsFn = () => fields(

        Field(
          name        = "picometers",
          fieldType   = IntType,
          description = Some("Wavelength in pm"),
          resolve     = _.value.toPicometers.value.value
        ),

        Field(
          name        = "angstroms",
          fieldType   = BigDecimalType,
          description = Some("Wavelength in Å"),
          resolve     = _.value.angstrom.value.toBigDecimal(2, HALF_UP)
        ),

        Field(
          name        = "nanometers",
          fieldType   = BigDecimalType,
          description = Some("Wavelength in nm"),
          resolve     = _.value.nanometer.value.toBigDecimal(3, HALF_UP)
        ),

        Field(
          name        = "micrometers",
          fieldType   = BigDecimalType,
          description = Some("Wavelength in µm"),
          resolve     = _.value.micrometer.value.toBigDecimal(6, HALF_UP)
        )
      )
    )

  implicit val EnumWavelengthModelUnits: EnumType[WavelengthModel.Units]=
    EnumType.fromEnumerated(
      "WavelengthUnits",
      "Wavelength units"
    )

  implicit val InputWavelengthModelInput: InputObjectType[WavelengthModel.Input] =
    deriveInputObjectType[WavelengthModel.Input](
      InputObjectTypeName("WavelengthModelInput"),
      InputObjectTypeDescription("Wavelength, choose one of the available units")
    )

}
