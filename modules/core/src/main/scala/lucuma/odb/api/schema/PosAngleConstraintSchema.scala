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

  val FixedConstraintType: ObjectType[Any, PosAngleConstraint.Fixed] =
    ObjectType[Any, PosAngleConstraint.Fixed](
      name        = "FixedPosAngleConstraint",
      description = "",

      fields      = List[Field[Any, PosAngleConstraint.Fixed]](
        Field(
          name        = "angle",
          description = "The fixed position angle value itself".some,
          fieldType   = AngleType,
          resolve     = _.value.angle
        ),

        Field(
          name        = "allowFlip",
          description = "Whether to allow a 180° flip to obtain a better guide star".some,
          fieldType   = BooleanType,
          resolve     = _.value.allowFlip
        )
      )
    )

  implicit val InputObjectFixedConstraint: InputObjectType[PosAngleConstraintInput.FixedInput] =
    InputObjectType[PosAngleConstraintInput.FixedInput](
      "FixedPosAngleConstraintInput",
      """Create or edit a fixed position angle constraint.  If `allowFlip` is true,
        |then either the specified angle or the angle + 180° may be used.
        |""".stripMargin,
      List(
        InputObjectAngle.createRequiredEditOptional("angle", "fixed position angle constraints"),
        BooleanType.optionField("allowFlip", "Set to allow a 180° flip to find the best guide star")
      )
    )

  val AverageParallacticConstraintType: ObjectType[Any, PosAngleConstraint.AverageParallactic] =
    ObjectType[Any, PosAngleConstraint.AverageParallactic](
      name        = "AverageParallacticPosAngleConstraint",
      description = "",

      fields      = List[Field[Any, PosAngleConstraint.AverageParallactic]](
        Field(
          name        = "overrideAngle",
          description = "When overridden, the angle to use instead of the parallactic angle".some,
          fieldType   = OptionType(AngleType),
          resolve     = _.value.overrideAngle
        )

      )
    )

  implicit val InputObjectAverageParallacticConstraint: InputObjectType[PosAngleConstraintInput.AverageParallacticInput] =
    InputObjectType[PosAngleConstraintInput.AverageParallacticInput](
      "AverageParallacticPosAngleConstraintInput",
      """Create or edit an average parallactic angle position angle constraint.
        |If `overrideAngle` is set, it will be used instead of the average parallactic
        |angle.
        |""".stripMargin,
      List(
        InputObjectAngle.createRequiredEditOptional("overrideAngle", "average parallactic angle constraints")
      )
    )

  val PosAngleConstraintType: ObjectType[Any, PosAngleConstraint] =
    ObjectType[Any, PosAngleConstraint](
      name        = "PosAngleConstraint",
      description = "Constraints (if any) on the observation's position angle.",

      fields      = List[Field[Any, PosAngleConstraint]](

        Field(
          name        = "constraintType",
          description = "The position angle constraint type in use".some,
          fieldType   = PosAngleConstraintEnum,
          resolve     = c => c.value.constraintType
        ),

        Field(
          name        = "fixed",
          description = "Describes the fixed position angle constraint, if set".some,
          fieldType   = OptionType(FixedConstraintType),
          resolve     = c => PosAngleConstraint.fixed.getOption(c.value)
        ),

        Field(
          name        = "averageParallactic",
          description = "Describes the average parallactic angle constraint, if set".some,
          fieldType   = OptionType(AverageParallacticConstraintType),
          resolve     = c => PosAngleConstraint.averageParallactic.getOption(c.value)
        )

      )
    )

  implicit val InputObjectPosAngleConstraint: InputObjectType[PosAngleConstraintInput] =
    InputObjectType[PosAngleConstraintInput](
      "PosAngleConstraintInput",
      """Create or edit position angle constraint.  If not specified, then the
        |position angle required to reach the best guide star option will be used.
        |Use `fixed` for a fixed (or fixed + 180°) constraint, or `averageParallactic`
        |for the average parallactic angle at the time of the observation.
        |""".stripMargin,
      List(
        InputObjectFixedConstraint.createRequiredEditOptional("fixed", "position angle constraint"),
        InputObjectAverageParallacticConstraint.createRequiredEditOptional("averageParallactic", "position angle constraint")
      )
    )

}
