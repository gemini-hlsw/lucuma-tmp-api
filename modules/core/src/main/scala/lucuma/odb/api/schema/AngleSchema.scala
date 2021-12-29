// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.AngleModel
import sangria.schema._

object AngleSchema {

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

  def AngleType: ObjectType[Any, Angle] =
    ObjectType[Any, Angle](
      name        = "Angle",
      fields      = unitFields
    )

}
