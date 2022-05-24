// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.Wavelength
import lucuma.odb.api.model.WavelengthModel
import sangria.macros.derive._
import sangria.schema._

import java.math.RoundingMode.HALF_UP

object WavelengthSchema {
  import RefinedSchema._
  import syntax.enum._

  val WavelengthType: ObjectType[Any, Wavelength] =
    ObjectType(
      name     = "Wavelength",
      fieldsFn = () => fields(

        Field(
          name        = "picometers",
          fieldType   = PosIntType,
          description = "Wavelength in pm".some,
          resolve     = _.value.toPicometers.value
        ),

        Field(
          name        = "angstroms",
          fieldType   = PosBigDecimalType,
          description = "Wavelength in Å".some,
          resolve     = c => PosBigDecimal.unsafeFrom(c.value.angstrom.value.toBigDecimal(2, HALF_UP))
        ),

        Field(
          name        = "nanometers",
          fieldType   = PosBigDecimalType,
          description = "Wavelength in nm".some,
          resolve     = c => PosBigDecimal.unsafeFrom(c.value.nanometer.value.toBigDecimal(3, HALF_UP))
        ),

        Field(
          name        = "micrometers",
          fieldType   = PosBigDecimalType,
          description = "Wavelength in µm".some,
          resolve     = c => PosBigDecimal.unsafeFrom(c.value.micrometer.value.toBigDecimal(6, HALF_UP))
        )
      )
    )

  implicit val EnumWavelengthUnits: EnumType[WavelengthModel.Units] =
    EnumType.fromEnumerated(
      "WavelengthUnits",
      "Wavelength units"
    )

  implicit val InputWavelength: InputObjectType[WavelengthModel.WavelengthInput] =
    deriveInputObjectType[WavelengthModel.WavelengthInput](
      InputObjectTypeName("WavelengthInput"),
      InputObjectTypeDescription("Wavelength, choose one of the available units")
    )

}
