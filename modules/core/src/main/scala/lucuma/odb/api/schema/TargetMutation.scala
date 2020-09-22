// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{DeclinationModel, RightAscensionModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._

import lucuma.core.math.{Coordinates, Offset, ProperVelocity}
import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait TargetMutation extends TargetScalars {

  import GeneralSchema.EnumTypeExistence
  import ProgramSchema.ProgramIdType
  import TargetSchema.{EphemerisKeyType, TargetIdArgument, TargetIdType, TargetType}

  import context._

  import syntax.inputobjecttype._

  val InputObjectTypeTargetCreateNonsidereal: InputObjectType[TargetModel.CreateNonsidereal] =
    deriveInputObjectType[TargetModel.CreateNonsidereal](
      InputObjectTypeName("CreateNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target parameters")
    )

  val ArgumentTargetCreateNonsidereal: Argument[TargetModel.CreateNonsidereal] =
    InputObjectTypeTargetCreateNonsidereal.argument(
      "input",
      "Nonsidereal target description"
    )

  implicit val InputObjectTypeOffset: InputObjectType[Offset] =
    deriveInputObjectType[Offset](
      InputObjectTypeName("OffsetInput"),
      InputObjectTypeDescription("Offset in p and q")
    )

  implicit val InputObjectTypeProperVelocity: InputObjectType[ProperVelocity] =
    deriveInputObjectType[ProperVelocity](
      InputObjectTypeName("ProperVelocityInput"),
      InputObjectTypeDescription("ProperVelocity in RA and dec")
    )

  implicit val InputObjectTypeCoordinates: InputObjectType[Coordinates] =
    deriveInputObjectType[Coordinates](
      InputObjectTypeName("CoordinatesInput"),
      InputObjectTypeDescription("RA/Dec Coordinates")
    )

  implicit val EnumTypeDeclinationUnits: EnumType[DeclinationModel.Units] =
    EnumType.fromEnumerated(
      "DeclinationUnits",
      "Unit options for Declination values"
    )

  implicit val EnumTypeRightAscensionUnits: EnumType[RightAscensionModel.Units] =
    EnumType.fromEnumerated(
      "RightAscensionUnits",
      "Unit options for RightAscension values"
    )

  implicit val InputObjectDeclinationLong: InputObjectType[DeclinationModel.LongInput] =
    deriveInputObjectType[DeclinationModel.LongInput](
      InputObjectTypeName("DeclinationFromLongInput"),
      InputObjectTypeDescription("Declination integral value in corresponding units")
    )

  implicit val InputObjectDeclinationDecimal: InputObjectType[DeclinationModel.DecimalInput] =
    deriveInputObjectType[DeclinationModel.DecimalInput](
      InputObjectTypeName("DeclinationFromDecimalInput"),
      InputObjectTypeDescription("Declination decimal value in corresponding units")
    )

  implicit val InputObjectDeclination: InputObjectType[DeclinationModel.Input] =
    deriveInputObjectType[DeclinationModel.Input](
      InputObjectTypeName("DeclinationInput"),
      InputObjectTypeDescription("Declination, choose one of the available units")
    )

  implicit val InputObjectRightAscensionLong: InputObjectType[RightAscensionModel.LongInput] =
    deriveInputObjectType[RightAscensionModel.LongInput](
      InputObjectTypeName("RightAscensionFromLongInput"),
      InputObjectTypeDescription("Right Ascension integral value in corresponding units")
    )

  implicit val InputObjectRightAscensionDecimal: InputObjectType[RightAscensionModel.DecimalInput] =
    deriveInputObjectType[RightAscensionModel.DecimalInput](
      InputObjectTypeName("RightAscensionFromDecimalInput"),
      InputObjectTypeDescription("Right Ascension decimal value in corresponding units")
    )

  implicit val InputObjectRightAscension: InputObjectType[RightAscensionModel.Input] =
    deriveInputObjectType[RightAscensionModel.Input](
      InputObjectTypeName("RightAscensionInput"),
      InputObjectTypeDescription("Right Ascension, choose one of the available units")
    )

  val InputObjectTypeCreateSidereal: InputObjectType[TargetModel.CreateSidereal] =
    deriveInputObjectType[TargetModel.CreateSidereal](
      InputObjectTypeName("CreateSiderealInput"),
      InputObjectTypeDescription("Sidereal target parameters")
    )

  val ArgumentTargetCreateSidereal: Argument[TargetModel.CreateSidereal] =
    InputObjectTypeCreateSidereal.argument(
      "input",
      "Sidereal target description"
    )

  val InputObjectTypeTargetEditSidereal: InputObjectType[TargetModel.EditSidereal] =
    deriveInputObjectType[TargetModel.EditSidereal](
      InputObjectTypeName("EditSiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters")
    )

  val ArgumentTargetEditSidereal: Argument[TargetModel.EditSidereal] =
    InputObjectTypeTargetEditSidereal.argument(
      "input",
      "Sidereal target edit"
    )

  def createNonsidereal[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createNonsiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetCreateNonsidereal),
      resolve   = c => c.target(_.insertNonsidereal(c.arg(ArgumentTargetCreateNonsidereal)))
    )

  def createSidereal[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createSiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetCreateSidereal),
      resolve   = c => c.target(_.insertSidereal(c.arg(ArgumentTargetCreateSidereal)))
    )

  //noinspection MutatorLikeMethodIsParameterless
  def updateSidereal[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateSiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetEditSidereal),
      resolve   = c => c.target(_.edit(c.arg(ArgumentTargetEditSidereal)))
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.delete(c.targetId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.undelete(c.targetId))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      createNonsidereal,
      createSidereal,
      updateSidereal,
      delete,
      undelete
    )

}

object TargetMutation extends TargetMutation
