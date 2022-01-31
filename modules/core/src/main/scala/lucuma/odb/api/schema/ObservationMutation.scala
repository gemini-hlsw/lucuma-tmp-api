// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ConstraintSetInput, ObservationModel, ScienceRequirementsModel}
import lucuma.odb.api.model.ObservationModel.BulkEdit
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.inputtype._
import cats.MonadError
import cats.effect.std.Dispatcher
import io.circe.Decoder
import lucuma.odb.api.model.targetModel.{EditAsterismInput, TargetEnvironmentModel}
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationMutation {

  import ConstraintSetMutation.InputObjectTypeConstraintSet
  import context._
  import ScienceConfigurationMutation.InputObjectTypeScienceConfig
  import ScienceRequirementsMutation.{InputObjectTypeScienceRequirementsCreate, InputObjectTypeScienceRequirementsEdit}
  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import ObservationSchema.{ObsActiveStatusType, ObservationIdType, ObservationIdArgument, ObsStatusType, ObservationType}
  import ProgramSchema.ProgramIdType
  import TargetMutation.{InputObjectTypeCreateTargetEnvironmentInput, InputObjectTypeEditAsterism, InputObjectTypeTargetEnvironmentEdit}
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

  val InputObjectTypeObservationEdit: InputObjectType[ObservationModel.Edit] =
    deriveInputObjectType[ObservationModel.Edit](
      InputObjectTypeName("EditObservationInput"),
      InputObjectTypeDescription("Edit observation"),
      ReplaceInputField("existence",            EnumTypeExistence.notNullableField("existence")),
      ReplaceInputField("name",                 NonEmptyStringType.nullableField("name")),
      ReplaceInputField("status",               ObsStatusType.notNullableField("status")),
      ReplaceInputField("activeStatus",         ObsActiveStatusType.notNullableField("activeStatus")),
      ReplaceInputField("targets",              InputObjectTypeTargetEnvironmentEdit.notNullableField("targetEnvironment")),
      ReplaceInputField("constraintSet",        InputObjectTypeConstraintSet.notNullableField("constraintSet")),
      ReplaceInputField("scienceRequirements",  InputObjectTypeScienceRequirementsEdit.nullableField("scienceRequirements")),
      ReplaceInputField("scienceConfiguration", InputObjectTypeScienceConfig.nullableField("scienceConfiguration"))
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

  val ArgumentTargetEnvironmentBulkEdit: Argument[BulkEdit[TargetEnvironmentModel.Edit]] =
    bulkEditArgument[TargetEnvironmentModel.Edit](
      "targetEnvironment",
      InputObjectTypeTargetEnvironmentEdit
    )

  val ArgumentConstraintSetBulkEdit: Argument[BulkEdit[ConstraintSetInput]] =
    bulkEditArgument[ConstraintSetInput](
      "constraintSet",
      InputObjectTypeConstraintSet
    )

  val ArgumentScienceRequirementsBulkEdit: Argument[BulkEdit[ScienceRequirementsModel.Edit]] =
    bulkEditArgument[ScienceRequirementsModel.Edit](
      "scienceRequirements",
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

  def updateAsterism[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateAsterism",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentAsterismBulkEdit),
      resolve   = c => c.observation(_.bulkEditAsterism(c.arg(ArgumentAsterismBulkEdit)))
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
      updateAsterism,
      updateTargetEnvironment,
      updateConstraintSet,
      updateScienceRequirements,
      delete,
      undelete,
    )

}

object ObservationMutation extends ObservationMutation
