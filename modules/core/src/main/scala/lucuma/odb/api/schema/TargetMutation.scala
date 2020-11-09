// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{CatalogIdModel, CoordinatesModel, DeclinationModel, MagnitudeModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._

import lucuma.core.`enum`.MagnitudeSystem
import lucuma.core.math.VelocityAxis

import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait TargetMutation extends TargetScalars {

  import GeneralSchema.EnumTypeExistence
  import NumericUnitsSchema._
  import ProgramSchema.ProgramIdType
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyType, EnumTypeMagnitudeBand, EnumTypeMagnitudeSystem, TargetIdArgument, TargetIdType, TargetType}

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

  implicit val EnumTypeProperVelocityUnits: EnumType[ProperMotionModel.Units] =
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

  implicit val InputObjectCatalogId: InputObjectType[CatalogIdModel.Input] =
    deriveInputObjectType[CatalogIdModel.Input](
      InputObjectTypeName("CatalogIdInput"),
      InputObjectTypeDescription("Catalog id consisting of catalog name and string identifier")
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

  private def InputObjectProperMotionComponent[A](
    name: String
  ): InputObjectType[ProperMotionModel.ComponentInput[A]] =
    deriveInputObjectType[ProperMotionModel.ComponentInput[A]](
      InputObjectTypeName(s"${name}Input"),
      InputObjectTypeDescription(s"$name, choose one of the available units")
    )

  implicit val InputObjectProperMotionRa: InputObjectType[ProperMotionModel.ComponentInput[VelocityAxis.RA]] =
    InputObjectProperMotionComponent("ProperMotionRa")

  implicit val InputObjectProperMotionDec: InputObjectType[ProperMotionModel.ComponentInput[VelocityAxis.Dec]] =
    InputObjectProperMotionComponent("ProperMotionDec")

  implicit val InputObjectProperMotion: InputObjectType[ProperMotionModel.Input] =
    deriveInputObjectType[ProperMotionModel.Input](
      InputObjectTypeName("ProperMotionInput"),
      InputObjectTypeDescription("Proper motion, choose one of the available units")
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

  implicit val InputObjectMagnitude: InputObjectType[MagnitudeModel.Input] =
    deriveInputObjectType[MagnitudeModel.Input](
      InputObjectTypeName("MagnitudeInput"),
      InputObjectTypeDescription("Magnitude description"),
      ReplaceInputField(
        "system",
        InputField(
          name         = "system",
          fieldType    = OptionInputType(EnumTypeMagnitudeSystem),
          defaultValue = Some(MagnitudeSystem.Vega: MagnitudeSystem)
        )
      )
    )

  val InputObjectTypeCreateSidereal: InputObjectType[TargetModel.CreateSidereal] =
    deriveInputObjectType[TargetModel.CreateSidereal](
      InputObjectTypeName("CreateSiderealInput"),
      InputObjectTypeDescription("Sidereal target parameters"),
      DocumentInputField("properVelocity", "Deprecated, use properMotion instead.")
    )

  val ArgumentTargetCreateSidereal: Argument[TargetModel.CreateSidereal] =
    InputObjectTypeCreateSidereal.argument(
      "input",
      "Sidereal target description"
    )

  val InputObjectTypeTargetEditSidereal: InputObjectType[TargetModel.EditSidereal] =
    deriveInputObjectType[TargetModel.EditSidereal](
      InputObjectTypeName("EditSiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters"),
      DocumentInputField("properVelocity", "Deprecated, use properMotion instead.")
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
