// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{Existence, ObservationModel}
import lucuma.odb.api.model.ObservationModel.BulkEdit
import lucuma.odb.api.schema.syntax.inputtype._
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.functor._
import cats.syntax.option._
import clue.data.syntax._
import io.circe.{Decoder, HCursor}
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
  import GeneralSchema.EnumTypeExistence
  import ObservationSchema.{ObsActiveStatusType, ObservationIdType, ObsStatusType, ObservationType}
  import PosAngleConstraintSchema._
  import ProgramSchema.ProgramIdType
  import RefinedSchema.NonEmptyStringType
  import TargetMutation.{InputObjectTypeEditAsterism, InputObjectTypeTargetEnvironment}
  import TimeSchema.InstantScalar
  import syntax.inputobjecttype._

  implicit val InputObjectTypeObservationProperties: InputObjectType[ObservationModel.PropertiesInput] =
    InputObjectType[ObservationModel.PropertiesInput](
      "ObservationPropertiesInput",
      "Observation properties",
      List(
        NonEmptyStringType.optionField("subtitle", "Subtitle adds additional detail to the target-based observation title, and is both optional and nullable"),
        ObsStatusType.optionField("status", "The observation status will default to New if not specified when an observation is created and may be edited but not deleted"),
        ObsActiveStatusType.optionField("activeStatus", "The observation active status will default to Active if not specified when an observation is created and may be edited but not deleted"),
        InstantScalar.optionField("visualizationTime", "Reference time used for time-dependent calculations such as average parallactic angle"),
        InputObjectPosAngleConstraint.optionField("posAngleConstraint", "Position angle constraint, if any. Set to null to remove all position angle constraints"),
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
      """Choose programId to include all of its observations, or else a
        |collection of observationIds to include particular observations.""".stripMargin,
      List(
        InputField("programId",      OptionInputType(ProgramIdType)),
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

  def existenceEditInput(name: String): InputObjectType[ObservationModel.EditInput] =
    InputObjectType[ObservationModel.EditInput](
      s"${name.capitalize}ObservationInput",
      s"Selects the observations for $name",
      List(
        InputField("select", InputObjectTypeObservationSelect)
        // leave out the "patch" since that is implied
      )
    )

  val InputObjectTypeObservationDelete: InputObjectType[ObservationModel.EditInput] =
    existenceEditInput("delete")

  val InputObjectTypeObservationUndelete: InputObjectType[ObservationModel.EditInput] =
    existenceEditInput("undelete")

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

  def CreateObservationResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ObservationModel.CreateResult] =
    ObjectType(
      name        = "CreateObservationResult",
      description = "The result of creating a new observation.",
      fieldsFn    = () => fields(

        Field(
          name        = "observation",
          description = "The newly created observation.".some,
          fieldType   = ObservationType[F],
          resolve     = _.value.observation
        )

      )
    )

  def create[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "createObservation",
      fieldType   = CreateObservationResultType[F],
      description = "Creates a new observation according to provided parameters".some,
      arguments   = List(ArgumentObservationCreate),
      resolve     = c => c.observation(_.insert(c.arg(ArgumentObservationCreate)))
    )

  def EditObservationResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ObservationModel.EditResult] =
    ObjectType(
      name        = "EditObservationResult",
      description = "The result of editing select observations.",
      fieldsFn    = () => fields(

        Field(
          name        = "observations",
          description = "The edited observations.".some,
          fieldType   = ListType(ObservationType[F]),
          resolve     = _.value.observations
        )

      )
    )

  def editObservation[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "editObservation",
      fieldType = EditObservationResultType[F],
      arguments = List(ArgumentObservationEdit),
      resolve   = c => c.observation(_.edit(c.arg(ArgumentObservationEdit)))
    )

  def CloneObservationResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ObservationModel.CloneResult] =
    ObjectType(
      name        = "CloneObservationResult",
      description = "The result of cloning an observation, containing the original and new observations.",
      fieldsFn    = () => fields(

        Field(
          name        = "originalObservation",
          description = "The original unmodified observation which was cloned.".some,
          fieldType   = ObservationType[F],
          resolve     = _.value.originalObservation
        ),

        Field(
          name        = "newObservation",
          description = "The new cloned (but possibly modified) observation.".some,
          fieldType   = ObservationType[F],
          resolve     = _.value.newObservation
        )

      )
    )


  def clone[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "cloneObservation",
      fieldType = CloneObservationResultType[F],
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

  private def existenceEditField[F[_]: Dispatcher: Async: Logger](
    to: Existence
  ): Field[OdbCtx[F], Unit] = {
    val name  = to.fold("delete", "undelete")
    val patch = ObservationModel.PropertiesInput.Empty.copy(existence = to.assign)

    // Need a custom decoder because we don't want to look for a "patch" field.
    implicit val decoder: Decoder[ObservationModel.EditInput] =
      (c: HCursor) => for {
        sel <- c.downField("select").as[ObservationModel.SelectInput]
      } yield ObservationModel.EditInput(sel, patch)

    val arg   =
      to.fold(InputObjectTypeObservationDelete, InputObjectTypeObservationUndelete)
         .argument(
           "input",
           s"Parameters used to select observations for $name"
         )

    Field(
      name        = s"${name}Observation",
      description = s"${name.capitalize}s all the observations identified by the `select` field".some,
      fieldType   = ListType(ObservationType[F]),
      arguments   = List(arg),
      resolve     = c => c.observation(_.edit(c.arg(arg)).map(_.observations))
    )
  }

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      editObservation,
      clone,
      editAsterism,
      existenceEditField(Existence.Deleted),
      existenceEditField(Existence.Present),
    )

}

object ObservationMutation extends ObservationMutation
