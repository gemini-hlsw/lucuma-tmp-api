// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ObservationModel
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.inputtype._
import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationMutation {

  import AsterismSchema.AsterismIdType
  import ConstraintSetMutation.{ArgumentConstraintSetBulkEdit, InputObjectTypeConstraintSetCreate, InputObjectTypeConstraintSetEdit}
  import GeneralSchema.{EnumTypeExistence, NonEmptyStringType}
  import ObservationSchema.{ObsActiveStatusType, ObservationIdType, ObservationIdArgument, ObsStatusType, ObservationType}
  import ProgramSchema.ProgramIdType
  import TargetSchema.TargetIdType
  import context._
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
      ReplaceInputField("existence",       EnumTypeExistence.notNullableField("existence")),
      ReplaceInputField("name",            NonEmptyStringType.nullableField("name")),
      ReplaceInputField("status",          ObsStatusType.notNullableField("status")),
      ReplaceInputField("active",          ObsActiveStatusType.notNullableField("active")),
      ReplaceInputField("asterismId",      AsterismIdType.nullableField("asterismId")),
      ReplaceInputField("targetId",        TargetIdType.nullableField("targetId")),
      ReplaceInputField("constraintSet",   InputObjectTypeConstraintSetEdit.nullableField("constraintSet"))
    )

  val ArgumentObservationEdit: Argument[ObservationModel.Edit] =
    InputObjectTypeObservationEdit.argument(
      "input",
      "Edit observation"
    )

  val InputObjectObservationEditPointing: InputObjectType[ObservationModel.EditPointing] =
    deriveInputObjectType[ObservationModel.EditPointing](
      InputObjectTypeName("EditObservationPointingInput"),
      InputObjectTypeDescription("Edit the target or asterism for a set of observations")
    )

  val ArgumentObservationEditPointing: Argument[ObservationModel.EditPointing] =
    InputObjectObservationEditPointing.argument(
      "input",
      "Edit observation asterism / target"
    )

  def create[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createObservation",
      fieldType = OptionType(ObservationType[F]),
      arguments = List(ArgumentObservationCreate),
      resolve   = c => c.observation(_.insert(c.arg(ArgumentObservationCreate)))
    )

  def update[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateObservation",
      fieldType = ObservationType[F],
      arguments = List(ArgumentObservationEdit),
      resolve   = c => c.observation(_.edit(c.arg(ArgumentObservationEdit)))
    )

  def updatePointing[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updatePointing",
      fieldType = ListType(ObservationType[F]), // Should change to a Payload where the observations and asterisms and targets, etc. can be included
      arguments = List(ArgumentObservationEditPointing),
      resolve   = c => c.observation(_.editPointing(c.arg(ArgumentObservationEditPointing)))
    )

  def updateConstraintSet[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateConstraintSet",
      fieldType = ListType(ObservationType[F]),
      arguments = List(ArgumentConstraintSetBulkEdit),
      resolve   = c => c.observation(_.bulkEditConstraintSet(c.arg(ArgumentConstraintSetBulkEdit)))
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteObservation",
      fieldType = ObservationType[F],
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.delete(c.observationId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteObservation",
      fieldType = ObservationType[F],
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.undelete(c.observationId))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      create,
      update,
      updatePointing,
      updateConstraintSet,
      delete,
      undelete,
    )

}

object ObservationMutation extends ObservationMutation
