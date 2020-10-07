// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{
  CoordinatesModel,
  DeclinationModel,
  ParallaxModel,
  ProperVelocityModel,
  RadialVelocityModel,
  RightAscensionModel,
  TargetModel
}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.core.math.VelocityAxis
import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait TargetMutation extends TargetScalars {

  import GeneralSchema.EnumTypeExistence
  import NumericUnitsSchema._
  import ProgramSchema.ProgramIdType
  import TargetSchema.{EphemerisKeyType, TargetIdArgument, TargetIdType, TargetType}

  import context._

  import syntax.inputobjecttype._

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

  implicit val EnumTypeProperVelocityUnits: EnumType[ProperVelocityModel.Units] =
    EnumType.fromEnumerated(
      "ProperVelocityComponentUnits",
      "Unit options for proper velocity components (RA and Dec)"
    )

  implicit val EnumTypeRadialVelocityUnits: EnumType[RadialVelocityModel.Units] =
    EnumType.fromEnumerated(
      "RadialVelocityUnits",
      "Unit options for radial velocity values"
    )

  implicit val EnumTypeParallaxUnits: EnumType[ParallaxModel.Units] =
    EnumType.fromEnumerated(
      "ParallaxUnits",
      "Unit options for parallax values"
    )

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

  implicit val InputObjectTypeCoordinates: InputObjectType[CoordinatesModel.Input] =
    deriveInputObjectType[CoordinatesModel.Input](
      InputObjectTypeName("CoordinatesInput"),
      InputObjectTypeDescription("Absolute coordinates relative base epoch")
    )

  implicit val InputObjectDeclination: InputObjectType[DeclinationModel.Input] =
    deriveInputObjectType[DeclinationModel.Input](
      InputObjectTypeName("DeclinationInput"),
      InputObjectTypeDescription("Declination, choose one of the available units")
    )

  implicit val InputObjectRightAscension: InputObjectType[RightAscensionModel.Input] =
    deriveInputObjectType[RightAscensionModel.Input](
      InputObjectTypeName("RightAscensionInput"),
      InputObjectTypeDescription("Right Ascension, choose one of the available units")
    )

  private def InputObjectProperVelocityComponent[A](
    name: String
  ): InputObjectType[ProperVelocityModel.ComponentInput[A]] =
    deriveInputObjectType[ProperVelocityModel.ComponentInput[A]](
      InputObjectTypeName(s"${name}Input"),
      InputObjectTypeDescription(s"$name, choose one of the available units")
    )

  implicit val InputObjectProperVelocityRa: InputObjectType[ProperVelocityModel.ComponentInput[VelocityAxis.RA]] =
    InputObjectProperVelocityComponent("ProperVelocityRa")

  implicit val InputObjectProperVelocityDec: InputObjectType[ProperVelocityModel.ComponentInput[VelocityAxis.Dec]] =
    InputObjectProperVelocityComponent("ProperVelocityDec")

  implicit val InputObjectProperVelocity: InputObjectType[ProperVelocityModel.Input] =
    deriveInputObjectType[ProperVelocityModel.Input](
      InputObjectTypeName("ProperVelocityInput"),
      InputObjectTypeDescription("Proper velocity, choose one of the available units")
    )

  implicit val InputObjectRadialVelocity: InputObjectType[RadialVelocityModel.Input] =
    deriveInputObjectType[RadialVelocityModel.Input](
      InputObjectTypeName("RadialVelocityInput"),
      InputObjectTypeDescription("Radial velocity, choose one of the available units")
    )

  implicit val InputObjectParallax: InputObjectType[ParallaxModel.Input] =
    deriveInputObjectType[ParallaxModel.Input](
      InputObjectTypeName("ParallaxModelInput"),
      InputObjectTypeDescription("Parallax, choose one of the available units")
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

  val InputObjectTargetProgramLinks: InputObjectType[TargetModel.TargetProgramLinks] =
    deriveInputObjectType[TargetModel.TargetProgramLinks](
      InputObjectTypeName("TargetProgramLinks"),
      InputObjectTypeDescription("Target and the programs with which they are associated")
    )

  val ArgumentTargetProgramLinks: Argument[TargetModel.TargetProgramLinks] =
    InputObjectTargetProgramLinks.argument(
      "input",
      "Target/program links"
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
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetEditSidereal),
      resolve   = c => c.target(_.edit(c.arg(ArgumentTargetEditSidereal)))
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteTarget",
      fieldType = TargetType[F],
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.delete(c.targetId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteTarget",
      fieldType = TargetType[F],
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.undelete(c.targetId))
    )

  def shareTargetWithPrograms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareTargetWithPrograms",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetProgramLinks),
      resolve   = c => c.target(_.shareWithPrograms(c.arg(ArgumentTargetProgramLinks)))
    )

  def unshareTargetWithPrograms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareTargetWithPrograms",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetProgramLinks),
      resolve   = c => c.target(_.unshareWithPrograms(c.arg(ArgumentTargetProgramLinks)))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      createNonsidereal,
      createSidereal,
      updateSidereal,
      delete,
      undelete,
      shareTargetWithPrograms,
      unshareTargetWithPrograms
    )

}

object TargetMutation extends TargetMutation
