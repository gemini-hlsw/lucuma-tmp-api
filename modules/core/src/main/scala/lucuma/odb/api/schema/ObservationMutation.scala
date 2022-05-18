// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ObservationModel
import lucuma.odb.api.model.ObservationModel.BulkEdit
import lucuma.odb.api.schema.syntax.inputtype._
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import io.circe.Decoder
import lucuma.odb.api.model.targetModel.EditAsterismPatchInput
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

  implicit val InputObjectTypeObservationProperties: InputObjectType[ObservationModel.PropertiesInput] =
    InputObjectType[ObservationModel.PropertiesInput](
      "ObservationPropertiesInput",
      "Observation properties",
      List(
        NonEmptyStringType.optionField("subtitle", "Subtitle adds additional detail to the target-based observation title, and is both optional and nullable"),
        ObsStatusType.optionField("status", "The observation status will default to New if not specified when an observation is created and may be edited but not deleted"),
        ObsActiveStatusType.optionField("activeStatus", "The observation active status will default to Active if not specified when an observation is created and may be edited but not deleted"),
        InputObjectTypeTargetEnvironment.optionField("targetEnvironment", "The targetEnvironment defaults to empty if not specified on creation, and may be edited but not deleted"),
        InputObjectTypeConstraintSet.optionField("constraintSet", "The constraintSet defaults to standard values if not specified on creation, and may be edited but not deleted"),
        InputObjectTypeScienceRequirements.optionField("scienceRequirements", "The scienceRequirements defaults to spectroscopy if not specified on creation, and may be edited but not deleted"),
        InputObjectTypeScienceMode.optionField("scienceMode", "The scienceMode describes the chosen observing mode and instrument, is optional and may be deleted"),
        EnumTypeExistence.optionField("existence", "Whether the observation is considered deleted (defaults to PRESENT) but may be edited")
      )
    )

  val InputObjectTypeObservationCreate: InputObjectType[ObservationModel.CreateInput] =
    deriveInputObjectType[ObservationModel.CreateInput](
      InputObjectTypeName("CreateObservationInput"),
      InputObjectTypeDescription("Observation creation parameters")
    )

  val ArgumentObservationCreate: Argument[ObservationModel.CreateInput] =
    InputObjectTypeObservationCreate.argument(
      "input",
      "Observation description"
    )

  implicit val InputObjectTypeObservationSelect: InputObjectType[ObservationModel.SelectInput] =
    InputObjectType[ObservationModel.SelectInput](
      "ObservationSelectInput",
      """Choose programId to include all of its observations, or else a single
        |observationId, or a collection of observationIds to include
        |particular observations.""".stripMargin,
      List(
        InputField("programId",      OptionInputType(ProgramIdType)),
        InputField("observationId",  OptionInputType(ObservationIdType)),
        InputField("observationIds", OptionInputType(ListInputType(ObservationIdType)))
      )
    )

  val InputObjectTypeObservationEdit: InputObjectType[ObservationModel.EditInput] =
    InputObjectType[ObservationModel.EditInput](
      "EditObservationInput",
      "Observation selection and update description",
      List(
        InputField("select", InputObjectTypeObservationSelect),
        InputField("patch",  InputObjectTypeObservationProperties)
      )
    )

  val ArgumentObservationEdit: Argument[ObservationModel.EditInput] =
    InputObjectTypeObservationEdit.argument(
      "input",
      "Parameters for editing existing observations"
    )

  val InputObjectTypeObservationCloneInput: InputObjectType[ObservationModel.CloneInput] =
    deriveInputObjectType[ObservationModel.CloneInput](
      InputObjectTypeName("CloneObservationInput"),
      InputObjectTypeDescription("Describes an observation clone operation, making any edits in the patch parameter.  The observation status in the cloned observation defaults to NEW."),
    )

  val ArgumentObservationCloneInput: Argument[ObservationModel.CloneInput] =
    InputObjectTypeObservationCloneInput.argument(
      "input",
      "Parameters for cloning an existing observation"
    )


  private def bulkEditArgument[A: Decoder](
    name:       String,
    editType:   InputType[A]
  ): Argument[BulkEdit[A]] = {

    val io: InputObjectType[BulkEdit[A]] =
      InputObjectType[BulkEdit[A]](
        name        = s"Edit${name.capitalize}Input",
        description =
          """Input for bulk editing multiple observations.  Select observations
            |with the 'select' input and specify the changes in 'edit'.
            |""".stripMargin,
        List(
          InputField("select", InputObjectTypeObservationSelect),
          InputField("patch",  editType)
        )
      )

    io.argument("input", s"Bulk edit $name")

  }

  val ArgumentEditAsterism: Argument[BulkEdit[Seq[EditAsterismPatchInput]]] =
    bulkEditArgument[Seq[EditAsterismPatchInput]](
      "asterism",
      ListInputType(InputObjectTypeEditAsterism)
    )

  def create[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "createObservation",
      fieldType   = ObservationType[F],
      description = "Creates a new observation according to provided parameters".some,
      arguments   = List(ArgumentObservationCreate),
      resolve     = c => c.observation(_.insert(c.arg(ArgumentObservationCreate)))
    )

  def editObservation[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "editObservation",
      fieldType = ListType(ObservationType[F]),
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

  def editAsterism[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "editAsterism",
      description =
        """Edit asterisms, adding or deleting targets, in (potentially) multiple
          |observations at once.
        """.stripMargin.some,
      fieldType   = ListType(ObservationType[F]),
      arguments   = List(ArgumentEditAsterism),
      resolve     = c => c.observation(_.editAsterism(c.arg(ArgumentEditAsterism)))
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
      editObservation,
      clone,
      editAsterism,
      delete,
      undelete,
    )

}

object ObservationMutation extends ObservationMutation
