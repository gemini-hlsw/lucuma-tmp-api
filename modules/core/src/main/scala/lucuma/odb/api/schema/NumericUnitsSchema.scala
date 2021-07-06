// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.NumericUnits
import sangria.schema._

object NumericUnitsSchema {
  private def inputNameFromEnum[U](E: EnumType[U]): String =
    E.name.replaceAll("Units", "")

  implicit def LongInput[U](implicit E: EnumType[U]): InputObjectType[NumericUnits.LongInput[U]] =
    InputObjectType[NumericUnits.LongInput[U]](
      name        = s"${inputNameFromEnum(E)}LongInput",
      description = s"Integral value in ${inputNameFromEnum(E)}",
      fields      = List[InputField[_]](
        InputField("value", LongType, "integral value in associated units"),
        InputField("units", E,        "units for associated value")
      )
    )

  implicit def DecimalInput[U](implicit E: EnumType[U]): InputObjectType[NumericUnits.DecimalInput[U]] =
    InputObjectType[NumericUnits.DecimalInput[U]](
      name        = s"${inputNameFromEnum(E)}DecimalInput",
      description = s"Decimal value in ${inputNameFromEnum(E)}",
      fields      = List[InputField[_]](
        InputField("value", BigDecimalType, "decimal value in associated units"),
        InputField("units", E,              "units for associated value")
      )
    )

}
