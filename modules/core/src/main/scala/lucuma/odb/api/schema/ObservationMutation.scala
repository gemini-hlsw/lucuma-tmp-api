// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ConstraintSetModel, ObservationModel, ScienceRequirementsModel, TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.model.ObservationModel.{BulkEdit, ObservationSelector}
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.inputtype._

import cats.MonadError
import cats.effect.std.Dispatcher
import io.circe.Decoder
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationMutation {

  import ConstraintSetMutation.{InputObjectTypeConstraintSetCreate, InputObjectTypeConstraintSetEdit}
  import ScienceConfigurationMutation.{InputObjectTypeScienceConfigurationCreate, InputObjectTypeScienceConfigurationSetEdit}
  import ScienceRequirementsMutation.{InputObjectTypeScienceRequirementsCreate, InputObjectTypeScienceRequirementsEdit}
  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import ObservationSchema.{ObsActiveStatusType, ObservationIdType, ObservationIdArgument, ObsStatusType, ObservationType}
  import ProgramSchema.ProgramIdType
  import context._
  import syntax.inputobjecttype._
  import TargetMutation.{InputObjectTypeTargetEnvironmentCreate, InputObjectTypeTargetEditList, InputObjectTypeTargetEditName, InputObjectTypeTargetEnvironmentEdit, InputObjectTypeTargetEditSidereal}

  val InputObjectTypeObservationCreate: InputObjectType[ObservationModel.Create] =
    deriveInputObjectType[ObservationModel.Create](
      InputObjectTypeName("CreateObservationInput"),
      InputObjectTypeDescription("Observation creation parameters"),
      ExcludeInputFields("config")  // TODO
    )

  val ArgumentObservationCreate: Argument[ObservationModel.Create] =
    InputObjectTypeObservationCreate.argument(
      "input",
      "Observation description"
    )

  val InputObjectTypeObservationEdit: InputObjectType[ObservationModel.Edit] =
    deriveInputObjectType[ObservationModel.Edit](
      InputObjectTypeName("EditObservationInput"),
      InputObjectTypeDescription("Edit observation"),
      ReplaceInputField("existence",            EnumTypeExistence.notNullableField("existence")),
      ReplaceInputField("name",                 NonEmptyStringType.nullableField("name")),
      ReplaceInputField("status",               ObsStatusType.notNullableField("status")),
      ReplaceInputField("activeStatus",         ObsActiveStatusType.notNullableField("activeStatus")),
      ReplaceInputField("targets",              InputObjectTypeTargetEnvironmentEdit.notNullableField("targets")),
      ReplaceInputField("constraintSet",        InputObjectTypeConstraintSetEdit.notNullableField("constraintSet")),
      ReplaceInputField("scienceRequirements",  InputObjectTypeScienceRequirementsEdit.nullableField("scienceRequirements")),
      ReplaceInputField("scienceConfiguration", InputObjectTypeScienceConfigurationSetEdit.nullableField("scienceConfiguration"))
    )

  val ArgumentObservationEdit: Argument[ObservationModel.Edit] =
    InputObjectTypeObservationEdit.argument(
      "input",
      "Edit observation"
    )

  val InputObjectObservationSelector: InputObjectType[ObservationSelector] =
    deriveInputObjectType[ObservationSelector](
      InputObjectTypeName("SelectObservationsInput"),
      InputObjectTypeDescription("Select all observations with a 'programId', or only specific 'observationIds'")
    )

  private def bulkEditArgument[S: Decoder, E: Decoder](
    name:       String,
    selectType: InputType[S],
    editType:   InputType[E]
  ): Argument[BulkEdit[S, E]] = {

    val io: InputObjectType[BulkEdit[S, E]] =
      InputObjectType[BulkEdit[S, E]](
        s"BulkEdit${name.capitalize}Input",
        "Input for bulk editing multiple observations",
        List(
          InputField("select", selectType),
          InputField("edit",   editType)
        )
      )

    io.argument("input", s"Bulk edit $name")

  }

  val ArgumentTargetNameBulkEdit: Argument[BulkEdit[ObservationSelector, TargetModel.EditName]] =
    bulkEditArgument[ObservationSelector, TargetModel.EditName](
      "targetName",
      InputObjectObservationSelector,
      InputObjectTypeTargetEditName
    )

  val ArgumentSiderealScienceTargetBulkEdit: Argument[BulkEdit[ObservationSelector, TargetModel.EditSidereal]] =
    bulkEditArgument[ObservationSelector, TargetModel.EditSidereal](
      "sidereal",
      InputObjectObservationSelector,
      InputObjectTypeTargetEditSidereal
    )

  val ArgumentAllScienceTargetsBulkEdit: Argument[BulkEdit[ObservationSelector, TargetModel.EditTargetList]] =
    bulkEditArgument[ObservationSelector, TargetModel.EditTargetList](
      "science",
      InputObjectObservationSelector,
      InputObjectTypeTargetEditList
    )

  val ArgumentTargetEnvironmentBulkEdit: Argument[BulkEdit[ObservationSelector, TargetEnvironmentModel.Edit]] =
    bulkEditArgument[ObservationSelector, TargetEnvironmentModel.Edit](
      "targetEnvironment",
      InputObjectObservationSelector,
      InputObjectTypeTargetEnvironmentEdit
    )

  val ArgumentConstraintSetBulkEdit: Argument[BulkEdit[ObservationSelector, ConstraintSetModel.Edit]] =
    bulkEditArgument[ObservationSelector, ConstraintSetModel.Edit](
      "constraintSet",
      InputObjectObservationSelector,
      InputObjectTypeConstraintSetEdit
    )

  val ArgumentScienceRequirementsBulkEdit: Argument[BulkEdit[ObservationSelector, ScienceRequirementsModel.Edit]] =
    bulkEditArgument[ObservationSelector, ScienceRequirementsModel.Edit](
      "scienceRequirements",
      InputObjectObservationSelector,
      InputObjectTypeScienceRequirementsEdit
    )

  def create[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "createObservation",
      fieldType = OptionType(ObservationType[F]),
      arguments = List(ArgumentObservationCreate),
      resolve   = c => c.observation(_.insert(c.arg(ArgumentObservationCreate)))
    )

  def update[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateObservation",
      fieldType = ObservationType[F],
      arguments = List(ArgumentObservationEdit),
      resolve   = c => c.observation(_.edit(c.arg(ArgumentObservationEdit)))
    )

  def updateScienceTargetName[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateScienceTargetName",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentTargetNameBulkEdit),
      resolve   = c => c.observation(_.bulkEditScienceTargetName(c.arg(ArgumentTargetNameBulkEdit)))
    )

  def updateSiderealScienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateSiderealScienceTarget",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentSiderealScienceTargetBulkEdit),
      resolve   = c => c.observation(_.bulkEditSiderealScienceTarget(c.arg(ArgumentSiderealScienceTargetBulkEdit)))
    )

  def updateScienceTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateAllScienceTargets",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentAllScienceTargetsBulkEdit),
      resolve   = c => c.observation(_.bulkEditAllScienceTargets(c.arg(ArgumentAllScienceTargetsBulkEdit)))
    )

  def updateTargetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateTargetEnvironment",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentTargetEnvironmentBulkEdit),
      resolve   = c => c.observation(_.bulkEditTargetEnvironment(c.arg(ArgumentTargetEnvironmentBulkEdit)))
    )

  def updateConstraintSet[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateConstraintSet",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentConstraintSetBulkEdit),
      resolve   = c => c.observation(_.bulkEditConstraintSet(c.arg(ArgumentConstraintSetBulkEdit)))
    )

  def updateScienceRequirements[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateScienceRequirements",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentScienceRequirementsBulkEdit),
      resolve   = c => c.observation(_.bulkEditScienceRequirements(c.arg(ArgumentScienceRequirementsBulkEdit)))
    )

  def delete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteObservation",
      fieldType = ObservationType[F],
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.delete(c.observationId))
    )

  def undelete[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteObservation",
      fieldType = ObservationType[F],
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.undelete(c.observationId))
    )

  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      create,
      update,
      updateScienceTargetName,
      updateSiderealScienceTarget,
      updateScienceTargets,
      updateTargetEnvironment,
      updateConstraintSet,
      updateScienceRequirements,
      delete,
      undelete,
    )

}

object ObservationMutation extends ObservationMutation
