// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import lucuma.odb.api.model.{CoordinatesModel, DeclinationModel, ObservationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{CatalogInfoInput, EditAsterismInput, NonsiderealInput, SiderealInput, TargetEnvironmentInput, TargetModel}
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.core.model.Target
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.macros.derive.{ReplaceInputField, _}
import sangria.marshalling.circe._
import sangria.schema.{InputField, _}


trait TargetMutation extends TargetScalars {

  import context._
  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import NumericUnitsSchema._
  import ObservationSchema.ObservationIdType
  import ProgramSchema.ProgramIdType
  import SourceProfileSchema.InputObjectSourceProfile
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyTypeEnumType, ArgumentTargetId, TargetIdType, TargetType}

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

  implicit val InputObjectCatalogInfo: InputObjectType[CatalogInfoInput] =
    deriveInputObjectType[CatalogInfoInput](
      InputObjectTypeName("CatalogInfoInput"),
      InputObjectTypeDescription("Catalog id consisting of catalog name, string identifier and an optional object type"),
      ReplaceInputField("name",       EnumTypeCatalogName.notNullableField("name")),
      ReplaceInputField("id",         NonEmptyStringType.notNullableField("id")),
      ReplaceInputField("objectType", NonEmptyStringType.nullableField("objectType"))
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
      InputObjectTypeName("ParallaxInput"),
      InputObjectTypeDescription("Parallax, choose one of the available units")
    )

  implicit val InputObjectTypeNonsidereal: InputObjectType[NonsiderealInput] =
    deriveInputObjectType[NonsiderealInput](
      InputObjectTypeName("NonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target parameters.  Supply `keyType` and `des` or `key`"),

      ReplaceInputField("keyType",       EphemerisKeyTypeEnumType      .notNullableField("keyType"      )),
      ReplaceInputField("des",           NonEmptyStringType            .notNullableField("des"          )),
      ReplaceInputField("key",           NonEmptyStringType            .notNullableField("key"          )),
    )

  implicit val InputObjectTypeSidereal: InputObjectType[SiderealInput] =
    deriveInputObjectType[SiderealInput](
      InputObjectTypeName("SiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters"),

      ReplaceInputField("ra",             InputObjectRightAscension.notNullableField("ra"         )),
      ReplaceInputField("dec",            InputObjectDeclination   .notNullableField("dec"        )),
      ReplaceInputField("epoch",          EpochStringType          .notNullableField("epoch"      )),
      ReplaceInputField("properMotion",   InputObjectProperMotion  .nullableField("properMotion"  )),
      ReplaceInputField("radialVelocity", InputObjectRadialVelocity.nullableField("radialVelocity")),
      ReplaceInputField("parallax",       InputObjectParallax      .nullableField("parallax"      )),
      ReplaceInputField("catalogInfo",    InputObjectCatalogInfo   .nullableField("catalogInfo"   ))
    )

  implicit val InputObjectTypeTargetProperties: InputObjectType[TargetModel.PropertiesInput] =
    InputObjectType[TargetModel.PropertiesInput](
      "TargetPropertiesInput",
      "Target properties",
      List(
        InputField("name",          OptionInputType(NonEmptyStringType)),
        InputField("sidereal",      OptionInputType(InputObjectTypeSidereal)),
        InputField("nonsidereal",   OptionInputType(InputObjectTypeNonsidereal)),
        InputField("sourceProfile", OptionInputType(InputObjectSourceProfile))
      )
    )

  implicit val InputObjectTypeCreateTarget: InputObjectType[TargetModel.CreateInput] =
    deriveInputObjectType[TargetModel.CreateInput](
      InputObjectTypeName("CreateTargetInput"),
      InputObjectTypeDescription("Target creation parameters")
    )

  val ArgumentTargetCreate: Argument[TargetModel.CreateInput] =
    InputObjectTypeCreateTarget.argument(
      "input",
      "Target description.  One (and only one) of sidereal or nonsidereal must be specified."
    )

  implicit val InputObjectTypeTargetSelect: InputObjectType[TargetModel.SelectInput] =
    InputObjectType[TargetModel.SelectInput](
      "TargetSelectInput",
      """Choose programId to include all of its targets, observationId, or
       | observationIds to include their targets, or else individual targets
       | via targetId or targetIds.""".stripMargin,
      List(
        InputField("programId",      OptionInputType(ProgramIdType)),
        InputField("observationId",  OptionInputType(ObservationIdType)),
        InputField("observationIds", OptionInputType(ListInputType(ObservationIdType))),
        InputField("targetId",       OptionInputType(TargetIdType)),
        InputField("targetIds",      OptionInputType(ListInputType(TargetIdType)))
      )
    )

  implicit val InputObjectTypeTargetPatch: InputObjectType[TargetModel.PatchInput] =
    InputObjectType[TargetModel.PatchInput](
      "TargetPatchInput",
      "Description of updates to the target properties",
      List(
        InputField("target",    OptionInputType(InputObjectTypeTargetProperties)),
        InputField("existence", OptionInputType(EnumTypeExistence))
      )
    )

  implicit val InputObjectTypeEditTarget: InputObjectType[TargetModel.EditInput] =
    InputObjectType[TargetModel.EditInput](
      "EditTargetInput",
      "Target selection and update description.",
      List(
        InputField("select", InputObjectTypeTargetSelect),
        InputField("patch",  InputObjectTypeTargetPatch)
      )
    )

  val ArgumentEditTargetInput: Argument[TargetModel.EditInput] =
    InputObjectTypeEditTarget.argument(
      "input",
      "Parameters for editing an existing target. "
    )

  implicit val InputObjectTypeTargetEnvironment: InputObjectType[TargetEnvironmentInput] =
    InputObjectType[TargetEnvironmentInput](
      "TargetEnvironmentInput",
      "Target environment editing and creation parameters",
      List(
        InputObjectTypeCoordinates.nullableField("explicitBase"),
        InputField("asterism", OptionInputType(ListInputType(TargetIdType)))
      )
    )

  implicit val InputObjectTypeEditAsterism: InputObjectType[EditAsterismInput] =
    deriveInputObjectType[EditAsterismInput](
      InputObjectTypeName("EditAsterismInput"),
      InputObjectTypeDescription("Add or delete targets in an asterism")
    )

  def createTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "createTarget",
      fieldType   = TargetType[F],
      description = "Creates a new target according to the provided parameters.  Only one of sidereal or nonsidereal may be specified.".some,
      arguments   = List(ArgumentTargetCreate),
      resolve     = c => c.target(_.insert(c.arg(ArgumentTargetCreate)))
    )

  def cloneTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] = {
    import ObservationSchema.OptionalListObservationIdArgument

    val existing: Argument[Target.Id] =
      Argument(
        name         = "existingTargetId",
        argumentType = TargetIdType,
        description  = "The existing target's id"
      )

    val suggested: Argument[Option[Target.Id]] =
      Argument(
        name         = "suggestedCloneId",
        argumentType = OptionInputType(TargetIdType),
        description  = "The new target clone's id (will be generated if not supplied)"
      )

    Field(
      name        = "cloneTarget",
      fieldType   = TargetType[F],
      description = "Makes a copy of an existing target, setting it to unobserved and to PRESENT.  If observationIds is specified, the clone will replace the existing target in those observations".some,
      arguments   = List(existing, suggested, OptionalListObservationIdArgument),
      resolve     = c => {
        c.unsafeToFuture(
          for {
            t <- c.ctx.odbRepo.target.clone(c.arg(existing), c.arg(suggested))
            _ <- c.ctx.odbRepo.observation.bulkEditAsterism(
              ObservationModel.BulkEdit(
                ObservationModel.BulkEdit.Select(
                  None,
                  c.arg(OptionalListObservationIdArgument).map(_.toList)
                ),
                List(
                  EditAsterismInput.delete(c.arg(existing)),
                  EditAsterismInput.add(t.id)
                )
              )
            )
          } yield t
        )
      }
    )
  }

  def editTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "editTarget",
      fieldType   = ListType(TargetType[F]),
      description = "Edits existing targets".some,
      arguments   = List(ArgumentEditTargetInput),
      resolve     = c => c.target(_.bulkEdit(c.arg(ArgumentEditTargetInput)))
    )

    def delete[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "deleteTarget",
      fieldType   = TargetType[F],
      description = "Marks the target as DELETED.  Use undeleteTarget to retrieve it.".some,
      arguments   = List(ArgumentTargetId),
      resolve     = c => c.target(_.delete(c.targetId))
    )

  def undelete[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "undeleteTarget",
      fieldType   = TargetType[F],
      description = "Marks the target as PRESENT.".some,
      arguments   = List(ArgumentTargetId),
      resolve     = c => c.target(_.undelete(c.targetId))
    )


  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      createTarget,
      cloneTarget,
      editTarget,
      delete,
      undelete
    )

}

object TargetMutation extends TargetMutation
