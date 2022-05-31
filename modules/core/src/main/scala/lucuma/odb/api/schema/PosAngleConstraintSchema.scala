// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import lucuma.odb.api.model.{PosAngleConstraint, PosAngleConstraintInput}
import lucuma.odb.api.schema.syntax.inputtype._
import sangria.schema._


object PosAngleConstraintSchema {

  import AngleSchema._
  import syntax.`enum`._

  val PosAngleConstraintEnum: EnumType[PosAngleConstraint.Type] =
    EnumType.fromEnumerated(
      "PosAngleConstraintType",
      "Position angle constraint type"
    )

  val PosAngleConstraintType: ObjectType[Any, PosAngleConstraint] =
    ObjectType[Any, PosAngleConstraint](
      name        = "PosAngleConstraint",
      description = "Constraints (if any) on the observation's position angle.",

      fields      = List[Field[Any, PosAngleConstraint]](

        Field(
          name        = "constraint",
          description = "The position angle constraint type in use".some,
          fieldType   = PosAngleConstraintEnum,
          resolve     = c => c.value.constraint
        ),

        Field(
          name        = "angle",
          description = "Describes the fixed position angle constraint, if set".some,
          fieldType   = OptionType(AngleType),
          resolve     = _.value.angle
        ),

      )
    )

  implicit val InputObjectPosAngleConstraint: InputObjectType[PosAngleConstraintInput] =
    InputObjectType[PosAngleConstraintInput](
      "PosAngleConstraintInput",
      """Create or edit position angle constraint.  If not specified, then the
        |position angle required to reach the best guide star option will be used.
        |""".stripMargin,
      List(
        PosAngleConstraintEnum.createRequiredEditOptional("constraint", "position angle constraint type"),
        InputObjectAngle.createRequiredEditOptional("angle", "fixed any, if any (not required for average parallactic")
      )
    )

}
