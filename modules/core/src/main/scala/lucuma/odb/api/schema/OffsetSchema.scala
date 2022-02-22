// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.math.{Angle, Offset}
import lucuma.core.math.Axis.{P, Q}
import lucuma.odb.api.model.OffsetModel
import sangria.schema._

object OffsetSchema {

  val InputObjectTypeOffsetComponentInput: InputObjectType[OffsetModel.ComponentInput] =
    InputObjectType[OffsetModel.ComponentInput](
      "OffsetComponentInput",
      "Offset component (p or q) input parameters. Choose one angle units definition.",
      List(
        InputField("microarcseconds", OptionInputType(LongType),       "Angle in µas"),
        InputField("milliarcseconds", OptionInputType(BigDecimalType), "Angle in mas"),
        InputField("arcseconds",      OptionInputType(BigDecimalType), "Angle in arcsec")
      )
    )

  val InputObjectTypeOffsetInput: InputObjectType[OffsetModel.Input] =
    InputObjectType[OffsetModel.Input](
      "OffsetInput",
      "Offset input.  Define offset in p and q.",
      List(
        InputField("p", InputObjectTypeOffsetComponentInput, "Offset in p"),
        InputField("q", InputObjectTypeOffsetComponentInput, "Offset in q")
      )
    )

  def OffsetComponentType[A](n: String): ObjectType[Any, Offset.Component[A]] =
    ObjectType(
      name     = n,
      fieldsFn = () => fields(

        Field(
          name        = "microarcseconds",
          fieldType   = LongType,
          description = Some(s"$n offset in µas"),
          resolve     = v => Angle.signedMicroarcseconds.get(v.value.toAngle)
        ),

        Field(
          name        = "milliarcseconds",
          fieldType   = BigDecimalType,
          description = Some(s"$n offset in mas"),
          resolve     = v => Angle.signedDecimalMilliarcseconds.get(v.value.toAngle)
        ),

        Field(
          name        = "arcseconds",
          fieldType   = BigDecimalType,
          description = Some(s"$n offset in arcsec"),
          resolve     = v => Angle.signedDecimalArcseconds.get(v.value.toAngle)
        )

      )
    )

  val OffsetType: ObjectType[Any, Offset] =
    ObjectType(
      name     = "Offset",
      fieldsFn = () => fields(

        Field(
          name        = "p",
          fieldType   = OffsetComponentType[P]("p"),
          description = Some("Offset in p"),
          resolve     = _.value.p
        ),

        Field(
          name        = "q",
          fieldType   = OffsetComponentType[Q]("q"),
          description = Some("Offset in q"),
          resolve     = _.value.q
        )

      )
    )
}
