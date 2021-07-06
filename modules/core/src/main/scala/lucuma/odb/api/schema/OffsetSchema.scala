// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.math.{Angle, Offset}
import lucuma.core.math.Axis.{P, Q}
import lucuma.odb.api.repo.OdbRepo
import sangria.schema._

object OffsetSchema {

  def OffsetComponentType[F[_], A](n: String): ObjectType[OdbRepo[F], Offset.Component[A]] =
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

  def OffsetType[F[_]]: ObjectType[OdbRepo[F], Offset] =
    ObjectType(
      name     = "Offset",
      fieldsFn = () => fields(

        Field(
          name        = "p",
          fieldType   = OffsetComponentType[F, P]("p"),
          description = Some("Offset in p"),
          resolve     = _.value.p
        ),

        Field(
          name        = "q",
          fieldType   = OffsetComponentType[F, Q]("q"),
          description = Some("Offset in q"),
          resolve     = _.value.q
        )

      )
    )
}
