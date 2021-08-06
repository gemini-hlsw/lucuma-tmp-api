// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{CatalogIdModel, CoordinatesModel, DeclinationModel, MagnitudeModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.core.`enum`.MagnitudeSystem
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait TargetMutation extends TargetScalars {

  import GeneralSchema.NonEmptyStringType
  import NumericUnitsSchema._
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyTypeEnumType, EnumTypeMagnitudeBand, EnumTypeMagnitudeSystem}

  import syntax.inputtype._
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

  implicit val EnumTypeProperMotionUnits: EnumType[ProperMotionModel.Units] =
    EnumType.fromEnumerated(
      "ProperMotionComponentUnits",
      "Unit options for proper motion components (RA and Dec)"
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

  implicit val InputObjectProperMotionComponent: InputObjectType[ProperMotionModel.ComponentInput] =
    deriveInputObjectType[ProperMotionModel.ComponentInput](
      InputObjectTypeName("ProperMotionComponentInput"),
      InputObjectTypeDescription(s"Proper motion component, choose one of the available units")
    )

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

  implicit val InputObjectMagnitudeCreate: InputObjectType[MagnitudeModel.Create] =
    deriveInputObjectType[MagnitudeModel.Create](
      InputObjectTypeName("MagnitudeCreateInput"),
      InputObjectTypeDescription("Magnitude creation parameters"),
      ReplaceInputField(
        "system",
        InputField(
          name         = "system",
          fieldType    = OptionInputType(EnumTypeMagnitudeSystem),
          defaultValue = Some(MagnitudeSystem.Vega: MagnitudeSystem)
        )
      )
    )

  implicit val InputObjectMagnitudeEdit: InputObjectType[MagnitudeModel.Edit] =
    deriveInputObjectType[MagnitudeModel.Edit](
      InputObjectTypeName("MagnitudeEditInput"),
      InputObjectTypeDescription("Magnitude editing parameters"),
      ReplaceInputField("value",  BigDecimalType.notNullableField("value")          ),
      ReplaceInputField("system", EnumTypeMagnitudeSystem.notNullableField("system")),
      ReplaceInputField("error",  BigDecimalType.nullableField("error")             )
    )

  implicit val InputObjectMagnitudeEditAction: InputObjectType[MagnitudeModel.EditAction] =
    deriveInputObjectType[MagnitudeModel.EditAction](
      InputObjectTypeName("MagnitudeEditAction"),
      InputObjectTypeDescription("Magnitude edit action (choose one option only)")
    )

  implicit val InputObjectMagnitudeEditList: InputObjectType[MagnitudeModel.EditList] =
    deriveInputObjectType[MagnitudeModel.EditList](
      InputObjectTypeName("MagnitudeEditList"),
      InputObjectTypeDescription("Magnitude list editing (choose one option only)")
    )

  implicit val InputObjectTypeCreateNonsidereal: InputObjectType[TargetModel.CreateNonsidereal] =
    deriveInputObjectType[TargetModel.CreateNonsidereal](
      InputObjectTypeName("CreateNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target parameters")
    )

  implicit val InputObjectTypeCreateSidereal: InputObjectType[TargetModel.CreateSidereal] =
    deriveInputObjectType[TargetModel.CreateSidereal](
      InputObjectTypeName("CreateSiderealInput"),
      InputObjectTypeDescription("Sidereal target parameters")
    )

  val ArgumentTargetCreateSidereal: Argument[TargetModel.CreateSidereal] =
    InputObjectTypeCreateSidereal.argument(
      "input",
      "Sidereal target description"
    )

  implicit val InputObjectTypeTargetCreate: InputObjectType[TargetModel.Create] =
    deriveInputObjectType[TargetModel.Create](
      InputObjectTypeName("CreateTargetInput"),
      InputObjectTypeDescription("Target creation parameters")
    )

  val ArgumentTargetCreate: Argument[TargetModel.Create] =
    InputObjectTypeTargetCreate.argument(
      "input",
      "Target creation parameters.  Choose 'nonSidereal' or 'sidereal'."
    )


  implicit val InputObjectTypeEditNonsidereal: InputObjectType[TargetModel.EditNonsidereal] =
    deriveInputObjectType[TargetModel.EditNonsidereal](
      InputObjectTypeName("EditNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target edit parameters"),

      ReplaceInputField("name", NonEmptyStringType.notNullableField("name")),
      ReplaceInputField("key",  EphemerisKeyType  .notNullableField("key"))
    )


  implicit val InputObjectTypeTargetEditSidereal: InputObjectType[TargetModel.EditSidereal] =
    deriveInputObjectType[TargetModel.EditSidereal](
      InputObjectTypeName("EditSiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters"),

      DocumentInputField("magnitudes",    "Edit magnitudes"                                               ),

      ReplaceInputField("name",           NonEmptyStringType       .notNullableField("name"       )),
      ReplaceInputField("catalogId",      InputObjectCatalogId     .nullableField("catalogId"     )),
      ReplaceInputField("ra",             InputObjectRightAscension.notNullableField("ra"         )),
      ReplaceInputField("dec",            InputObjectDeclination   .notNullableField("dec"        )),
      ReplaceInputField("epoch",          EpochStringType          .notNullableField("epoch"      )),
      ReplaceInputField("properMotion",   InputObjectProperMotion  .nullableField("properMotion"  )),
      ReplaceInputField("radialVelocity", InputObjectRadialVelocity.nullableField("radialVelocity")),
      ReplaceInputField("parallax",       InputObjectParallax      .nullableField("parallax"      ))
    )

  val ArgumentTargetEditSidereal: Argument[TargetModel.EditSidereal] =
    InputObjectTypeTargetEditSidereal.argument(
      "input",
      "Sidereal target edit"
    )

  implicit val InputObjectTypeTargetEdit: InputObjectType[TargetModel.Edit] =
    deriveInputObjectType[TargetModel.Edit](
      InputObjectTypeName("EditTargetInput"),
      InputObjectTypeDescription("Target editing parameters")
    )

  val ArgumentTargetEdit: Argument[TargetModel.Edit] =
    InputObjectTypeTargetEdit.argument(
      "input",
      "Target edit"
    )

  implicit val InputObjectTypeTargetEditAction: InputObjectType[TargetModel.EditTargetAction] =
    deriveInputObjectType[TargetModel.EditTargetAction](
      InputObjectTypeName("EditTargetActionInput"),
      InputObjectTypeDescription("Target edit action (choose one of 'add', 'delete', or 'edit'.")
    )

  implicit val InputObjectTypeTargetEditList: InputObjectType[TargetModel.EditTargetList] =
    deriveInputObjectType[TargetModel.EditTargetList](
      InputObjectTypeName("EditTargetListInput"),
      InputObjectTypeDescription("Target list edit input (choose one of 'replaceList' or 'editList'.")
    )

  implicit val InputObjectTypeTargetEnvironmentCreate: InputObjectType[TargetEnvironmentModel.Create] =
    deriveInputObjectType[TargetEnvironmentModel.Create](
      InputObjectTypeName("CreateTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment creation input parameters")
    )

  implicit val InputObjectTypeTargetEnvironmentEdit: InputObjectType[TargetEnvironmentModel.Edit] =
    deriveInputObjectType[TargetEnvironmentModel.Edit](
      InputObjectTypeName("EditTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment edit input parameters"),

      ReplaceInputField("explicitBase", InputObjectTypeCoordinates.nullableField("explicitBase"))
    )

  def allFields[F[_]]: List[Field[OdbRepo[F], Unit]] =
    Nil

}

object TargetMutation extends TargetMutation
