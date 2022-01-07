// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.AngleModel

import sangria.macros.derive._
import sangria.schema._

object AngleSchema {

  import syntax.enum._

  val unitFields: List[Field[Any, Angle]] =
    Field[Any, Angle, Long, Long](
      name        = "microarcseconds",
      description = "Angle in µas".some,
      fieldType   = LongType,
      resolve     = (c: Context[Any, Angle]) => Value[Any, Long](c.value.toMicroarcseconds)
    ) :: Enumerated[AngleModel.Units].all.tail.map { u =>
      Field.apply[Any, Angle, BigDecimal, BigDecimal](
        name        = u.name,
        description = s"Angle in ${u.abbreviation}".some,
        fieldType   = BigDecimalType,
        resolve     = (c: Context[Any, Angle]) => Value[Any, BigDecimal](u.signedDecimal.get(c.value))
      )
    }

  val AngleType: ObjectType[Any, Angle] =
    ObjectType[Any, Angle](
      name        = "Angle",
      fields      = unitFields
    )

  implicit val EnumAngleUnits: EnumType[AngleModel.Units] =
    EnumType.fromEnumerated(
      "AngleUnits",
      "Angle units"
    )

  implicit val InputObjectLongAngle: InputObjectType[AngleModel.LongAngleInput] =
    deriveInputObjectType(
      InputObjectTypeName("LongAngleInput"),
      InputObjectTypeDescription("Create an angle from a signed integral value and its units.")
    )

  implicit val InputObjectDecimalAngle: InputObjectType[AngleModel.DecimalAngleInput] =
    deriveInputObjectType(
      InputObjectTypeName("DecimalAngleInput"),
      InputObjectTypeDescription("Create an angle from a signed decimal value and its units.")
    )

  implicit val InputObjectAngle: InputObjectType[AngleModel.AngleInput] =
    deriveInputObjectType(
      InputObjectTypeName("AngleInput"),
      InputObjectTypeDescription("Create an angle from a signed value.  Choose exactly one of the available units.")
    )

}
