// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.MonadError
import cats.effect.std.Dispatcher
import lucuma.odb.api.model.{CatalogIdModel, CoordinatesModel, DeclinationModel, MagnitudeModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{BulkEditTargetEnvironmentInput, BulkEditTargetInput, BulkEditTargetListInput, BulkReplaceTargetListInput, CreateNonsiderealInput, CreateSiderealInput, CreateTargetEnvironmentInput, CreateTargetInput, EditNonsiderealInput, EditSiderealInput, EditAsterismInput, SelectTargetEnvironmentInput, SelectTargetInput, TargetEditResult, TargetEnvironmentContext, TargetListEditResult}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.core.`enum`.MagnitudeSystem
import cats.syntax.option._
import sangria.macros.derive.{ReplaceInputField, _}
import sangria.marshalling.circe._
import sangria.schema._


trait TargetMutation extends TargetScalars {

  import context._
  import GeneralSchema.NonEmptyStringType
  import NumericUnitsSchema._
  import ObservationSchema.{ObservationIdType, ObservationType}
  import ProgramSchema.{ProgramIdArgument, ProgramIdType, ProgramType}
  import TargetSchema.{EnumTypeCatalogName, EphemerisKeyTypeEnumType, EnumTypeMagnitudeBand, EnumTypeMagnitudeSystem, TargetEnvironmentIdType, TargetEnvironmentModelType, TargetIdType, TargetModelType}

  import syntax.inputtype._
  import syntax.inputobjecttype._

  implicit val EnumTypeTargetEditDescOp: EnumType[TargetEditResult.Op] =
    EnumType.fromEnumerated(
      "TargetEditDescOp",
      "The target edit that was performed"
    )

  def TargetEditDescType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetEditResult] =
    ObjectType(
      name     = "TargetEditDesc",
      fieldsFn = () => fields(

        Field(
          name        = "op",
          fieldType   = EnumTypeTargetEditDescOp,
          description = "Which operation was performed".some,
          resolve     = _.value.op
        ),

        Field(
          name        = "target",
          fieldType   = TargetModelType[F],
          description = "Target that was edited".some,
          resolve     = _.value.target
        )

      )
    )

  private def targetEnvironmentContextFields[F[_]: Dispatcher, C <: TargetEnvironmentContext](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], C]] =
    List(
      Field(
        name        = "targetEnvironment",
        fieldType   = TargetEnvironmentModelType[F],
        description = "Target environment that was edited".some,
        resolve     = _.value.targetEnvironment
      ),

      Field(
        name        = "observation",
        fieldType   = OptionType(ObservationType[F]),
        description = "Observation that houses the target environment, if any".some,
        resolve     = _.value.observation
      ),

      Field(
        name        = "program",
        fieldType   = ProgramType[F],
        description = "Program that houses the target environment".some,
        resolve     = _.value.program
      )
    )

  def TargetListEditDescType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetListEditResult] =
    ObjectType(
      name     = "TargetListEditDesc",
      fieldsFn = () =>
        targetEnvironmentContextFields[F, TargetListEditResult] :+
          Field(
            name        = "edits",
            fieldType   = ListType(TargetEditDescType[F]),
            description = "Details any edits that were performed".some,
            resolve     = _.value.edits
          )
    )

  def TargetEnvironmentContextType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetEnvironmentContext] =
    ObjectType(
      name   = "TargetEnvironmentContext",
      fieldsFn = () => targetEnvironmentContextFields[F, TargetEnvironmentContext]
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

  implicit val InputObjectTypeCreateTarget: InputObjectType[CreateTargetInput] =
    deriveInputObjectType[CreateTargetInput](
      InputObjectTypeName("CreateTargetInput"),
      InputObjectTypeDescription("Target creation parameters")
    )

  implicit val InputObjectTypeSelectTarget: InputObjectType[SelectTargetInput] =
    deriveInputObjectType[SelectTargetInput](
      InputObjectTypeName("SelectTargetInput"),
      InputObjectTypeDescription("Target selection parameters.  Choose at least one of `names` or `targetIds`.")
    )

  implicit val InputObjectTypeEditNonsidereal: InputObjectType[EditNonsiderealInput] =
    deriveInputObjectType[EditNonsiderealInput](
      InputObjectTypeName("EditNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target edit parameters"),

      ReplaceInputField("name", NonEmptyStringType.notNullableField("name")),
      ReplaceInputField("key",  EphemerisKeyType  .notNullableField("key"))
    )


  implicit val InputObjectTypeTargetEditSidereal: InputObjectType[EditSiderealInput] =
    deriveInputObjectType[EditSiderealInput](
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

  implicit val InputObjectTypeSelectTargetEnvironmentInput: InputObjectType[SelectTargetEnvironmentInput] =
    deriveInputObjectType[SelectTargetEnvironmentInput](
      InputObjectTypeName("SelectTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment selection parameters. Choose at least one option.")
    )

  implicit val InputObjectTypeBulkEditTargetInput: InputObjectType[BulkEditTargetInput] =
    deriveInputObjectType[BulkEditTargetInput](
      InputObjectTypeName("BulkEditTargetInput"),
      InputObjectTypeDescription("Target editing parameters")
    )

  val ArgumentBulkEditTargetInput: Argument[BulkEditTargetInput] =
    InputObjectTypeBulkEditTargetInput.argument(
      "input",
      "Editing input for a single science target"
    )

  implicit val InputObjectEditTargetInput: InputObjectType[EditAsterismInput] =
    deriveInputObjectType[EditAsterismInput](
      InputObjectTypeName("EditTargetInput"),
      InputObjectTypeDescription("Single target edit options")
    )

  implicit val InputObjectTypeBulkEditTargetListInput: InputObjectType[BulkEditTargetListInput] =
    deriveInputObjectType[BulkEditTargetListInput](
      InputObjectTypeName("BulkEditTargetListInput"),
      InputObjectTypeDescription("Target editing parameters")
    )

  val ArgumentBulkEditTargetListInput: Argument[BulkEditTargetListInput] =
    InputObjectTypeBulkEditTargetListInput.argument(
      "input",
      "Editing input for multiple science targets"
    )

  implicit val InputObjectTypeBulkReplaceTargetListInput: InputObjectType[BulkReplaceTargetListInput] =
    deriveInputObjectType[BulkReplaceTargetListInput](
      InputObjectTypeName("BulkReplaceTargetListInput"),
      InputObjectTypeDescription("Target list set/replace parameters")
    )

  val ArgumentBulkReplaceTargetListInput: Argument[BulkReplaceTargetListInput] =
    InputObjectTypeBulkReplaceTargetListInput.argument(
      "input",
      "Editing input for replacing or setting science target lists"
    )

  implicit val InputObjectTypeCreateTargetEnvironmentInput: InputObjectType[CreateTargetEnvironmentInput] =
    deriveInputObjectType[CreateTargetEnvironmentInput](
      InputObjectTypeName("CreateTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment creation input parameters")
    )

  val ArgumentCreateTargetEnvironmentInput: Argument[CreateTargetEnvironmentInput] =
    InputObjectTypeCreateTargetEnvironmentInput.argument(
      "input",
      "Parameters for creating a new target environment"
    )

  implicit val InputObjectTypeBulkEditTargetEnvironmentInput: InputObjectType[BulkEditTargetEnvironmentInput] =
    deriveInputObjectType[BulkEditTargetEnvironmentInput](
      InputObjectTypeName("BulkEditTargetEnvironmentInput"),
      InputObjectTypeDescription("Target environment edit parameters"),
      ReplaceInputField("explicitBase",  InputObjectTypeCoordinates.nullableField("explicitBase"))
    )

  val ArgumentBulkEditTargetEnvironmentInput: Argument[BulkEditTargetEnvironmentInput] =
    InputObjectTypeBulkEditTargetEnvironmentInput.argument(
      "input",
      "Editing input for editing target environment properties"
    )

  def updateScienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateScienceTarget",
      fieldType = ListType(TargetListEditDescType[F]),
      arguments = List(ArgumentBulkEditTargetInput),
      resolve   = c => c.target(_.bulkEditScienceTarget(c.arg(ArgumentBulkEditTargetInput)))
    )

  def updateScienceTargetList[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateScienceTargetList",
      fieldType = ListType(TargetListEditDescType[F]),
      arguments = List(ArgumentBulkEditTargetListInput),
      resolve   = c => c.target(_.bulkEditScienceTargetList(c.arg(ArgumentBulkEditTargetListInput)))
    )

  def replaceScienceTargetList[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "replaceScienceTargetList",
      fieldType = ListType(TargetListEditDescType[F]),
      arguments = List(ArgumentBulkReplaceTargetListInput),
      resolve   = c => c.target(_.bulkReplaceScienceTargetList(c.arg(ArgumentBulkReplaceTargetListInput)))
    )

  def updateTargetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateTargetEnvironment",
      fieldType = ListType(TargetEnvironmentContextType[F]),
      arguments = List(ArgumentBulkEditTargetEnvironmentInput),
      resolve   = c => c.target(_.bulkEditTargetEnvironment(c.arg(ArgumentBulkEditTargetEnvironmentInput)))
    )

  def createUnaffiliatedTargetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "createTargetEnvironment",
      description = "Creates a new target environment unaffiliated with any observation".some,
      fieldType   = TargetEnvironmentModelType[F],
      arguments   = List(ProgramIdArgument, ArgumentCreateTargetEnvironmentInput),
      resolve     = c => c.target(_.createUnaffiliatedTargetEnvironment(c.programId, c.arg(ArgumentCreateTargetEnvironmentInput)))
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      createUnaffiliatedTargetEnvironment[F],
      updateScienceTarget[F],
      updateScienceTargetList[F],
      replaceScienceTargetList[F],
      updateTargetEnvironment[F]
    )

}

object TargetMutation extends TargetMutation
