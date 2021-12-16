// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.MonadError
import cats.effect.std.Dispatcher
import lucuma.odb.api.model.{CatalogIdModel, CoordinatesModel, DeclinationModel, MagnitudeModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{CreateNonsiderealInput, CreateSiderealInput, EditNonsiderealInput, EditSiderealInput, TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.core.`enum`.MagnitudeSystem
import sangria.macros.derive.{ReplaceInputField, _}
import sangria.marshalling.circe._
import sangria.schema._


trait TargetMutation extends TargetScalars {

  import context._
  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import NumericUnitsSchema._
  import ProgramSchema.ProgramIdType
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyTypeEnumType, EnumTypeMagnitudeBand, EnumTypeMagnitudeSystem, ArgumentTargetId, TargetIdType, TargetType}

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

  implicit val InputObjectTypeCreateNonsidereal: InputObjectType[CreateNonsiderealInput] =
    deriveInputObjectType[CreateNonsiderealInput](
      InputObjectTypeName("CreateNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target parameters")
    )

  implicit val InputObjectTypeCreateSidereal: InputObjectType[CreateSiderealInput] =
    deriveInputObjectType[CreateSiderealInput](
      InputObjectTypeName("CreateSiderealInput"),
      InputObjectTypeDescription("Sidereal target parameters")
    )

  implicit val InputObjectTypeCreateTarget: InputObjectType[TargetModel.Create] =
    deriveInputObjectType[TargetModel.Create](
      InputObjectTypeName("CreateTargetInput"),
      InputObjectTypeDescription("Target creation parameters")
    )

  val ArgumentTargetCreate: Argument[TargetModel.Create] =
    InputObjectTypeCreateTarget.argument(
      "input",
      "Target description"
    )

  implicit val InputObjectTypeEditNonsidereal: InputObjectType[EditNonsiderealInput] =
    deriveInputObjectType[EditNonsiderealInput](
      InputObjectTypeName("EditNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target edit parameters"),

      ReplaceInputField("key",  EphemerisKeyType  .notNullableField("key"))
    )


  implicit val InputObjectTypeEditSidereal: InputObjectType[EditSiderealInput] =
    deriveInputObjectType[EditSiderealInput](
      InputObjectTypeName("EditSiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters"),

      DocumentInputField("magnitudes",    "Edit magnitudes"                                               ),

      ReplaceInputField("catalogId",      InputObjectCatalogId     .nullableField("catalogId"     )),
      ReplaceInputField("ra",             InputObjectRightAscension.notNullableField("ra"         )),
      ReplaceInputField("dec",            InputObjectDeclination   .notNullableField("dec"        )),
      ReplaceInputField("epoch",          EpochStringType          .notNullableField("epoch"      )),
      ReplaceInputField("properMotion",   InputObjectProperMotion  .nullableField("properMotion"  )),
      ReplaceInputField("radialVelocity", InputObjectRadialVelocity.nullableField("radialVelocity")),
      ReplaceInputField("parallax",       InputObjectParallax      .nullableField("parallax"      ))
    )

  implicit val InputObjectEditTargetInput: InputObjectType[TargetModel.Edit] = {

      // Not able to derive this for some reason, TBD.
//    deriveInputObjectType[TargetModel.Edit](
//      InputObjectTypeName("EditTargetInput"),
//      InputObjectTypeDescription("Single target edit options"),
//      ReplaceInputField("existence",  EnumTypeExistence.notNullableField("existence"))
//    )

    InputObjectType[TargetModel.Edit](
      "EditTargetInput",
      "Single target edit options",
      List(
        InputField("targetId",    TargetIdType),
        InputField("existence",   OptionInputType(EnumTypeExistence)),
        InputField("name",        OptionInputType(NonEmptyStringType)),
        InputField("sidereal",    OptionInputType(InputObjectTypeEditSidereal)),
        InputField("nonSidereal", OptionInputType(InputObjectTypeEditNonsidereal))
      )
    )
  }

  val ArgumentEditTargetInput: Argument[TargetModel.Edit] =
    InputObjectEditTargetInput.argument(
      "input",
      "Parameters for editing an existing target"
    )

  implicit val InputObjectTypeCreateTargetEnvironmentInput: InputObjectType[TargetEnvironmentModel.Create] =
    deriveInputObjectType[TargetEnvironmentModel.Create](
      InputObjectTypeName("CreateTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment creation input parameters")
    )

  val ArgumentCreateTargetEnvironmentInput: Argument[TargetEnvironmentModel.Create] =
    InputObjectTypeCreateTargetEnvironmentInput.argument(
      "input",
      "Parameters for creating a new target environment"
    )

  implicit val InputObjectTypeTargetEnvironmentEdit: InputObjectType[TargetEnvironmentModel.Edit] = {

    // Not able to derive this for some reason, TBD.
//    deriveInputObjectType[TargetEnvironmentModel.Edit](
//      InputObjectTypeName("EditTargetEnvironmentInput"),
//      InputObjectTypeDescription("Target environment editing parameters"),
//      ReplaceInputField("explicitBase", InputObjectTypeCoordinates.nullableField("explicitBase"))
//    )

    InputObjectType[TargetEnvironmentModel.Edit](
      "EditTargetEnvironmentInput",
      "Target environment editing parameters",
      List(
        InputObjectTypeCoordinates.nullableField("explicitBase"),
        InputField("asterism", OptionInputType(ListInputType(TargetIdType)))
      )
    )
  }

  def createTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "createTarget",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetCreate),
      resolve   = c => c.target(_.insert(c.arg(ArgumentTargetCreate)))
    )

  def cloneTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] = {
    val existing  = Argument(
      name         = "existingTargetId",
      argumentType = TargetIdType,
      description  = "The existing target's id"
    )

    val suggested = Argument(
      name         = "suggestedCloneId",
      argumentType = OptionInputType(TargetIdType),
      description  = "The new target clone's id (will be generated if not supplied)"
    )

    Field(
      name      = "cloneTarget",
      fieldType = TargetType[F],
      arguments = List(existing, suggested),
      resolve   = c => c.target(_.clone(c.arg(existing), c.arg(suggested)))
    )
  }

  def updateTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateTarget",
      fieldType = TargetType[F],
      arguments = List(ArgumentEditTargetInput),
      resolve   = c => c.target(_.edit(c.arg(ArgumentEditTargetInput)))
    )

    def delete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteTarget",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetId),
      resolve   = c => c.target(_.delete(c.targetId))
    )

  def undelete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteTarget",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetId),
      resolve   = c => c.target(_.undelete(c.targetId))
    )


  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      createTarget,
      cloneTarget,
      updateTarget,
      delete,
      undelete
    )

}

object TargetMutation extends TargetMutation
