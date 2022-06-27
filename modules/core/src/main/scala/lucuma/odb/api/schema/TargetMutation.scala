// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import clue.data.syntax._
import eu.timepit.refined.types.all.NonNegInt
import io.circe.{Decoder, HCursor}
import io.circe.refined._
import lucuma.odb.api.model.{CoordinatesModel, DeclinationModel, Existence, ObservationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{CatalogInfoInput, EditAsterismPatchInput, NonsiderealInput, SiderealInput, TargetEnvironmentInput, TargetModel, WhereTargetInput}
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
  import QuerySchema.UpdateResultType
  import RefinedSchema.{NonEmptyStringType, NonNegIntType}
  import SourceProfileSchema.InputObjectSourceProfile
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyTypeEnumType, InputObjectWhereTarget, TargetIdType, TargetType}

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

  implicit val InputObjectTypeUpdateTargets: InputObjectType[TargetModel.UpdateInput] =
    InputObjectType[TargetModel.UpdateInput](
      "UpdateTargetsInput",
      "Target selection and update description. Use `SET` to specify the changes, `WHERE` to select the targets to update, and `LIMIT` to control the size of the return value.",
      List(
        InputField("SET",  InputObjectTypeTargetProperties, "Describes the target values to modify."),
        InputObjectWhereTarget.optionField("WHERE", "Filters the targets to be updated according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional targets match the WHERE clause they will be updated but not returned).")
      )
    )

  val ArgumentUpdateTargets: Argument[TargetModel.UpdateInput] =
    InputObjectTypeUpdateTargets.argument(
      "input",
      "Parameters for updating existing targets. "
    )

  def existenceEditInput(name: String): InputObjectType[TargetModel.UpdateInput] =
    InputObjectType[TargetModel.UpdateInput](
      s"${name.capitalize}TargetsInput",
      s"Selects the targets for $name",
      List(
        InputObjectWhereTarget.optionField("WHERE", s"Filters the targets for $name according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional targets match the WHERE clause they will be updated but not returned).")
        // leave out the "set" since that is implied
      )
    )

  val InputObjectTypeTargetDelete: InputObjectType[TargetModel.UpdateInput] =
    existenceEditInput("delete")

  val InputObjectTypeTargetUndelete: InputObjectType[TargetModel.UpdateInput] =
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

  implicit val InputObjectTypeEditAsterisms: InputObjectType[EditAsterismPatchInput] =
    deriveInputObjectType[EditAsterismPatchInput](
      InputObjectTypeName("EditAsterismsPatchInput"),
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
          name        = "target",
          description = "The newly created target.".some,
          fieldType   = TargetType[F],
          resolve     = _.value.target
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

  def updateTargets[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "updateTargets",
      fieldType   = UpdateResultType("targets", TargetType[F]),
      description = "Updates existing targets".some,
      arguments   = List(ArgumentUpdateTargets),
      resolve     = c => c.target(_.update(c.arg(ArgumentUpdateTargets)))
    )

  private def existenceEditField[F[_]: Dispatcher: Async: Logger](
    to: Existence
  ): Field[OdbCtx[F], Unit] = {
    val name = to.fold("delete", "undelete")
    val set  = TargetModel.PropertiesInput.Empty.copy(existence = to.assign)

    // Need a custom decoder because we don't want to look for a "patch" field.
    implicit val decoder: Decoder[TargetModel.UpdateInput] =
      (c: HCursor) => for {
        where <- c.downField("WHERE").as[Option[WhereTargetInput]]
        limit <- c.downField("LIMIT").as[Option[NonNegInt]]
      } yield TargetModel.UpdateInput(set, where, limit)

    val arg   =
      to.fold(InputObjectTypeTargetDelete, InputObjectTypeTargetUndelete)
         .argument(
           "input",
           s"Parameters used to select observations for $name"
         )

    Field(
      name        = s"${name}Targets",
      description = s"${name.capitalize}s all the targets identified by the `WHERE` field".some,
      fieldType   =  UpdateResultType("targets", TargetType[F]),
      arguments   = List(arg),
      resolve     = c => c.target(_.update(c.arg(arg)))
    )
  }

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      createTarget,
      cloneTarget,
      updateTargets,
      existenceEditField(Existence.Deleted),
      existenceEditField(Existence.Present)
    )

}

object TargetMutation extends TargetMutation
