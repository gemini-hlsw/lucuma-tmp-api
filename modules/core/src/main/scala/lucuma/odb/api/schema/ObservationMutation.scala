// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ConstraintSetInput, ObservationModel, ScienceRequirementsInput}
import lucuma.odb.api.model.ObservationModel.BulkEdit
import lucuma.odb.api.schema.syntax.inputtype._
import cats.effect.Async
import cats.effect.std.Dispatcher
import io.circe.Decoder
import lucuma.odb.api.model.targetModel.{EditAsterismInput, TargetEnvironmentInput}
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationMutation {

  import ConstraintSetMutation.InputObjectTypeConstraintSet
  import context._
  import ScienceModeMutation.InputObjectTypeScienceMode
  import ScienceRequirementsMutation.InputObjectTypeScienceRequirements
  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import ObservationSchema.{ObsActiveStatusType, ObservationIdType, ObservationIdArgument, ObsStatusType, ObservationType}
  import ProgramSchema.ProgramIdType
  import TargetMutation.{InputObjectTypeEditAsterism, InputObjectTypeTargetEnvironment}
  import syntax.inputobjecttype._

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

  val InputObjectTypeObservationCloneInput: InputObjectType[ObservationModel.CloneInput] =
    deriveInputObjectType[ObservationModel.CloneInput](
      InputObjectTypeName("CloneObservationInput"),
      InputObjectTypeDescription("Parameters for cloning an existing observation.  The existingObservationId is required, all else is optional. Unset values will be copied from the existing observation, except that status will default to NEW."),
      ExcludeInputFields("config")
    )

  val ArgumentObservationCloneInput: Argument[ObservationModel.CloneInput] =
    InputObjectTypeObservationCloneInput.argument(
      "input",
      "Observation clone parameters"
    )

  val InputObjectTypeObservationEdit: InputObjectType[ObservationModel.Edit] =
    deriveInputObjectType[ObservationModel.Edit](
      InputObjectTypeName("EditObservationInput"),
      InputObjectTypeDescription("Edit observation"),
      ReplaceInputField("existence",            EnumTypeExistence.notNullableField("existence")),
      ReplaceInputField("subtitle",             NonEmptyStringType.nullableField("subtitle")),
      ReplaceInputField("status",               ObsStatusType.notNullableField("status")),
      ReplaceInputField("activeStatus",         ObsActiveStatusType.notNullableField("activeStatus")),
      ReplaceInputField("targetEnvironment",    InputObjectTypeTargetEnvironment.notNullableField("targetEnvironment")),
      ReplaceInputField("constraintSet",        InputObjectTypeConstraintSet.notNullableField("constraintSet")),
      ReplaceInputField("scienceRequirements",  InputObjectTypeScienceRequirements.notNullableField("scienceRequirements")),
      ReplaceInputField("scienceMode",          InputObjectTypeScienceMode.nullableField("scienceMode"))
    )

  val ArgumentObservationEdit: Argument[ObservationModel.Edit] =
    InputObjectTypeObservationEdit.argument(
      "input",
      "Edit observation"
    )

  private def bulkEditArgument[A: Decoder](
    name:       String,
    editType:   InputType[A]
  ): Argument[BulkEdit[A]] = {

    val io: InputObjectType[BulkEdit[A]] =
      InputObjectType[BulkEdit[A]](
        s"BulkEdit${name.capitalize}Input",
        "Input for bulk editing multiple observations",
        List(
          InputField("selectProgram",      OptionInputType(ProgramIdType)),
          InputField("selectObservations", OptionInputType(ListInputType(ObservationIdType))),
          InputField("edit",               editType)
        )
      )

    io.argument("input", s"Bulk edit $name")

  }

  val ArgumentAsterismBulkEdit: Argument[BulkEdit[Seq[EditAsterismInput]]] =
    bulkEditArgument[Seq[EditAsterismInput]](
      "asterism",
      ListInputType(InputObjectTypeEditAsterism)
    )

  val ArgumentTargetEnvironmentBulkEdit: Argument[BulkEdit[TargetEnvironmentInput]] =
    bulkEditArgument[TargetEnvironmentInput](
      "targetEnvironment",
      InputObjectTypeTargetEnvironment
    )

  val ArgumentConstraintSetBulkEdit: Argument[BulkEdit[ConstraintSetInput]] =
    bulkEditArgument[ConstraintSetInput](
      "constraintSet",
      InputObjectTypeConstraintSet
    )

  val ArgumentScienceRequirementsBulkEdit: Argument[BulkEdit[ScienceRequirementsInput]] =
    bulkEditArgument[ScienceRequirementsInput](
      "scienceRequirements",
      InputObjectTypeScienceRequirements
    )

  def create[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "createObservation",
      fieldType = OptionType(ObservationType[F]),
      arguments = List(ArgumentObservationCreate),
      resolve   = c => c.observation(_.insert(c.arg(ArgumentObservationCreate)))
    )

  def update[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "updateObservation",
      fieldType = ObservationType[F],
      arguments = List(ArgumentObservationEdit),
      resolve   = c => c.observation(_.edit(c.arg(ArgumentObservationEdit)))
    )

  def clone[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "cloneObservation",
      fieldType = ObservationType[F],
      arguments = List(ArgumentObservationCloneInput),
      resolve   = c => c.observation(_.clone(c.arg(ArgumentObservationCloneInput)))
    )

  def bulkEditAsterism[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "bulkEditAsterism",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentAsterismBulkEdit),
      resolve   = c => c.observation(_.bulkEditAsterism(c.arg(ArgumentAsterismBulkEdit)))
    )

  def bulkEditTargetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "bulkEditTargetEnvironment",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentTargetEnvironmentBulkEdit),
      resolve   = c => c.observation(_.bulkEditTargetEnvironment(c.arg(ArgumentTargetEnvironmentBulkEdit)))
    )

  def bulkEditConstraintSet[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "bulkEditConstraintSet",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentConstraintSetBulkEdit),
      resolve   = c => c.observation(_.bulkEditConstraintSet(c.arg(ArgumentConstraintSetBulkEdit)))
    )

  def bulkEditScienceRequirements[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "bulkEditScienceRequirements",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentScienceRequirementsBulkEdit),
      resolve   = c => c.observation(_.bulkEditScienceRequirements(c.arg(ArgumentScienceRequirementsBulkEdit)))
    )

  def delete[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "deleteObservation",
      fieldType = ObservationType[F],
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.delete(c.observationId))
    )

  def undelete[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "undeleteObservation",
      fieldType = ObservationType[F],
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.undelete(c.observationId))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      update,
      clone,
      bulkEditAsterism,
      bulkEditTargetEnvironment,
      bulkEditConstraintSet,
      bulkEditScienceRequirements,
      delete,
      undelete,
    )

}

object ObservationMutation extends ObservationMutation
