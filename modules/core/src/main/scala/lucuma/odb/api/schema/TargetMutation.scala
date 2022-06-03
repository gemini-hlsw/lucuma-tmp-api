// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import clue.data.syntax._
import io.circe.{Decoder, HCursor}
import lucuma.odb.api.model.{CoordinatesModel, DeclinationModel, Existence, ObservationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{CatalogInfoInput, EditAsterismPatchInput, NonsiderealInput, SiderealInput, TargetEnvironmentInput, TargetModel}
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.macros.derive.{ReplaceInputField, _}
import sangria.marshalling.circe._
import sangria.schema.{InputField, _}


trait TargetMutation extends TargetScalars {

  import context._
  import GeneralSchema.EnumTypeExistence
  import ObservationSchema.ObservationIdType
  import ProgramSchema.ProgramIdType
  import RefinedSchema.NonEmptyStringType
  import SourceProfileSchema.InputObjectSourceProfile
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyTypeEnumType, TargetIdType, TargetType}

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
        InputField("sourceProfile", OptionInputType(InputObjectSourceProfile)),
        InputField("existence",     OptionInputType(EnumTypeExistence))
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
      """Choose programId to include all of its targets, observationIds to
       |include each listed observation's targets, or else individual targets
       |via targetIds.""".stripMargin,
      List(
        InputField("programId",      OptionInputType(ProgramIdType)),
        InputField("observationIds", OptionInputType(ListInputType(ObservationIdType))),
        InputField("targetIds",      OptionInputType(ListInputType(TargetIdType)))
      )
    )

  implicit val InputObjectTypeEditTarget: InputObjectType[TargetModel.EditInput] =
    InputObjectType[TargetModel.EditInput](
      "EditTargetInput",
      "Target selection and update description.",
      List(
        InputField("select", InputObjectTypeTargetSelect),
        InputField("patch",  InputObjectTypeTargetProperties)
      )
    )

  val ArgumentEditTargetInput: Argument[TargetModel.EditInput] =
    InputObjectTypeEditTarget.argument(
      "input",
      "Parameters for editing existing targets. "
    )

  def existenceEditInput(name: String): InputObjectType[TargetModel.EditInput] =
    InputObjectType[TargetModel.EditInput](
      s"${name.capitalize}TargetInput",
      s"Selects the targets for $name",
      List(
        InputField("select", InputObjectTypeTargetSelect)
        // leave out the "patch" since that is implied
      )
    )

  val InputObjectTypeTargetDelete: InputObjectType[TargetModel.EditInput] =
    existenceEditInput("delete")

  val InputObjectTypeTargetUndelete: InputObjectType[TargetModel.EditInput] =
    existenceEditInput("undelete")


  implicit val InputObjectTypeTargetEnvironment: InputObjectType[TargetEnvironmentInput] =
    InputObjectType[TargetEnvironmentInput](
      "TargetEnvironmentInput",
      "Target environment editing and creation parameters",
      List(
        InputObjectTypeCoordinates.nullableField("explicitBase"),
        InputField("asterism", OptionInputType(ListInputType(TargetIdType)))
      )
    )

  implicit val InputObjectTypeEditAsterism: InputObjectType[EditAsterismPatchInput] =
    deriveInputObjectType[EditAsterismPatchInput](
      InputObjectTypeName("EditAsterismPatchInput"),
      InputObjectTypeDescription("Add or delete targets in an asterism")
    )

  implicit val InputObjectCloneTarget: InputObjectType[TargetModel.CloneInput] =
    deriveInputObjectType[TargetModel.CloneInput](
      InputObjectTypeName("CloneTargetInput"),
      InputObjectTypeDescription("Describes a target clone operation, making any edits in the patch parameter and replacing the target in the selected observations")
    )

  val ArgumentCloneTarget: Argument[TargetModel.CloneInput] =
    InputObjectCloneTarget.argument(
      "input",
      "Parameters for cloning an existing target"
    )

   def CreateTargetResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], TargetModel.CreateResult] =
    ObjectType(
      name        = "CreateTargetResult",
      description = "The result of creating a new target.",
      fieldsFn    = () => fields(

        Field(
          name        = "newTarget",
          description = "The newly created target.".some,
          fieldType   = TargetType[F],
          resolve     = _.value.newTarget
        )

      )
    )

  def createTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "createTarget",
      fieldType   = CreateTargetResultType[F],
      description = "Creates a new target according to the provided parameters.  Only one of sidereal or nonsidereal may be specified.".some,
      arguments   = List(ArgumentTargetCreate),
      resolve     = c => c.target(_.insert(c.arg(ArgumentTargetCreate)))
    )

  def CloneTargetResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], TargetModel.CloneResult] =
    ObjectType(
      name        = "CloneTargetResult",
      description = "The result of cloning a target, containing the original and new targets.",
      fieldsFn    = () => fields(

        Field(
          name        = "originalTarget",
          description = "The original unmodified target which was cloned".some,
          fieldType   = TargetType[F],
          resolve     = _.value.originalTarget
        ),

        Field(
          name        = "newTarget",
          description = "The new cloned (but possibly modified) target".some,
          fieldType   = TargetType[F],
          resolve     = _.value.newTarget
        )

      )
    )

  def cloneTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "cloneTarget",
      fieldType   = CloneTargetResultType[F],
      description = "Makes a copy of an existing target, setting it to unobserved and to PRESENT.  If observationIds is specified, the clone will replace the existing target in those observations".some,
      arguments   = List(ArgumentCloneTarget),
      resolve     = c => {
        val cloneInput = c.arg(ArgumentCloneTarget)
        c.unsafeToFuture(
          for {
            r <- c.ctx.odbRepo.target.clone(cloneInput)
            _ <- c.ctx.odbRepo.observation.editAsterism(
              ObservationModel.BulkEdit(
                ObservationModel.SelectInput.observationIds(
                  cloneInput.replaceIn.toList.flatten
                ),
                List(
                  EditAsterismPatchInput.delete(cloneInput.targetId),
                  EditAsterismPatchInput.add(r.newTarget.id)
                )
              )
            )
          } yield r
        )
      }
    )

  def editTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "editTarget",
      fieldType   = ListType(TargetType[F]),
      description = "Edits existing targets".some,
      arguments   = List(ArgumentEditTargetInput),
      resolve     = c => c.target(_.edit(c.arg(ArgumentEditTargetInput)))
    )

  private def existenceEditField[F[_]: Dispatcher: Async: Logger](
    to: Existence
  ): Field[OdbCtx[F], Unit] = {
    val name  = to.fold("delete", "undelete")
    val patch = TargetModel.PropertiesInput.Empty.copy(existence = to.assign)

    // Need a custom decoder because we don't want to look for a "patch" field.
    implicit val decoder: Decoder[TargetModel.EditInput] =
      (c: HCursor) => for {
        sel <- c.downField("select").as[TargetModel.SelectInput]
      } yield TargetModel.EditInput(sel, patch)

    val arg   =
      to.fold(InputObjectTypeTargetDelete, InputObjectTypeTargetUndelete)
         .argument(
           "input",
           s"Parameters used to select observations for $name"
         )

    Field(
      name        = s"${name}Target",
      description = s"${name.capitalize}s all the targets identified by the `select` field".some,
      fieldType   = ListType(TargetType[F]),
      arguments   = List(arg),
      resolve     = c => c.target(_.edit(c.arg(arg)))
    )
  }

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      createTarget,
      cloneTarget,
      editTarget,
      existenceEditField(Existence.Deleted),
      existenceEditField(Existence.Present)
    )

}

object TargetMutation extends TargetMutation
