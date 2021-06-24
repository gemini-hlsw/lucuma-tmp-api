// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{CatalogIdModel, CoordinatesModel, DeclinationModel, MagnitudeModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.core.`enum`.MagnitudeSystem

import cats.MonadError
import cats.effect.std.Dispatcher
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait TargetMutation extends TargetScalars {

  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import NumericUnitsSchema._
  import ProgramSchema.ProgramIdType
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyType, EnumTypeMagnitudeBand, EnumTypeMagnitudeSystem, TargetIdArgument, TargetIdType, TargetType}

  import context._

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
      InputObjectTypeDescription("Sidereal target edit parameters"),

      DocumentInputField("magnitudes",       "Replace all magnitudes with the provided values"                  ),
      DocumentInputField("modifyMagnitudes", "Update any listed magnitudes leaving unmentioned values unchanged"),
      DocumentInputField("deleteMagnitudes", "Removes any listed magnitude values"                              ),

      ReplaceInputField("existence",      EnumTypeExistence        .notNullableField("existence"  )),
      ReplaceInputField("name",           StringType               .notNullableField("name"       )),
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

  def createNonsidereal[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "createNonsiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetCreateNonsidereal),
      resolve   = c => c.target(_.insertNonsidereal(c.arg(ArgumentTargetCreateNonsidereal)))
    )

  def createSidereal[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "createSiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetCreateSidereal),
      resolve   = c => c.target(_.insertSidereal(c.arg(ArgumentTargetCreateSidereal)))
    )

  //noinspection MutatorLikeMethodIsParameterless
  def updateSidereal[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateSiderealTarget",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetEditSidereal),
      resolve   = c =>
        c.target {
          val e = c.arg(ArgumentTargetEditSidereal)
          _.edit(e.id, e.editor)
        }
    )

  def delete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteTarget",
      fieldType = TargetType[F],
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.delete(c.targetId))
    )

  def undelete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteTarget",
      fieldType = TargetType[F],
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.undelete(c.targetId))
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      createNonsidereal,
      createSidereal,
      updateSidereal,
      delete,
      undelete
    )

}

object TargetMutation extends TargetMutation
