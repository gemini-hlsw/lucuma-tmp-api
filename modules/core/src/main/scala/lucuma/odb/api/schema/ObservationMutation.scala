// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{Existence, ObservationModel, WhereObservationInput}
import lucuma.odb.api.model.ObservationModel.BulkEdit
import lucuma.odb.api.schema.syntax.inputtype._
import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import clue.data.syntax._
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.{Decoder, HCursor}
import io.circe.refined._
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
  import ObservationSchema.{InputObjectWhereObservation, ObsActiveStatusType, ObservationIdType, ObsStatusType, ObservationType}
  import PosAngleConstraintSchema._
  import ProgramSchema.ProgramIdType
  import QuerySchema.UpdateResultType
  import RefinedSchema.{NonEmptyStringType, NonNegIntType}
  import TargetMutation.{InputObjectTypeEditAsterisms, InputObjectTypeTargetEnvironment}
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

  val InputObjectTypeUpdateObservations: InputObjectType[ObservationModel.UpdateInput] =
    InputObjectType[ObservationModel.UpdateInput](
      "UpdateObservationsInput",
      "Observation selection and update description.  Use `SET` to specify the changes, `WHERE` to select the observations to update, and `LIMIT` to control the size of the return value.",
      List(
        InputField("SET",  InputObjectTypeObservationProperties, "Describes the observation values to modify."),
        InputObjectWhereObservation.optionField("WHERE", "Filters the observations to be updated according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional observations match the WHERE clause they will be updated but not returned).")
      )
    )

  val ArgumentUpdateObservations: Argument[ObservationModel.UpdateInput] =
    InputObjectTypeUpdateObservations.argument(
      "input",
      "Parameters for editing existing observations."
    )

  def existenceEditInput(name: String): InputObjectType[ObservationModel.UpdateInput] =
    InputObjectType[ObservationModel.UpdateInput](
      s"${name.capitalize}ObservationsInput",
      s"Selects the observations for $name",
      List(
        InputObjectWhereObservation.optionField("WHERE", s"Filters the observations for $name according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional observations match the WHERE clause they will be updated but not returned).")
        // leave out the "set" since that is implied
      )
    )

  val InputObjectTypeObservationDelete: InputObjectType[ObservationModel.UpdateInput] =
    existenceEditInput("delete")

  val InputObjectTypeObservationUndelete: InputObjectType[ObservationModel.UpdateInput] =
    existenceEditInput("undelete")

  val InputObjectTypeObservationCloneInput: InputObjectType[ObservationModel.CloneInput] =
    deriveInputObjectType[ObservationModel.CloneInput](
      InputObjectTypeName("CloneObservationInput"),
      InputObjectTypeDescription("Describes an observation clone operation, making any edits in the `SET` parameter.  The observation status in the cloned observation defaults to NEW."),
    )

  val ArgumentObservationCloneInput: Argument[ObservationModel.CloneInput] =
    InputObjectTypeObservationCloneInput.argument(
      "input",
      "Parameters for cloning an existing observation"
    )


  private def bulkUpdateArgument[A: Decoder](
    name:       String,
    editType:   InputType[A]
  ): Argument[BulkEdit[A]] = {

    val io: InputObjectType[BulkEdit[A]] =
      InputObjectType[BulkEdit[A]](
        name        = s"Update${name.capitalize}sInput",
        description =
          """Input for bulk updating multiple observations.  Select observations
            |with the 'WHERE' input and specify the changes in 'SET'.
            |""".stripMargin,
        List(
          InputField("SET",  editType, "Describes the values to modify."),
          InputObjectWhereObservation.optionField("WHERE", "Filters the observations to be updated according to those that match the given constraints."),
          NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional observations match the WHERE clause they will be updated but not returned).")
        )
      )

    io.argument("input", s"Bulk update $name")

  }

  val ArgumentUpdateAsterisms: Argument[BulkEdit[Seq[EditAsterismPatchInput]]] =
    bulkUpdateArgument[Seq[EditAsterismPatchInput]](
      "asterism",
      ListInputType(InputObjectTypeEditAsterisms)
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

  def updateObservations[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "updateObservations",
      fieldType   = UpdateResultType("UpdateObservationsResult", "observations", ObservationType[F]),
      description = "Updates existing observations".some,
      arguments   = List(ArgumentUpdateObservations),
      resolve     = c => c.observation(_.update(c.arg(ArgumentUpdateObservations)))
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

  def updateAsterisms[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "updateAsterisms",
      description =
        """Update asterisms, adding or deleting targets, in (potentially) multiple
          |observations at once.
        """.stripMargin.some,
      fieldType   = UpdateResultType("UpdateAsterismsResult", "observations", ObservationType[F]),
      arguments   = List(ArgumentUpdateAsterisms),
      resolve     = c => c.observation(_.updateAsterism(c.arg(ArgumentUpdateAsterisms)))
    )

  private def existenceEditField[F[_]: Dispatcher: Async: Logger](
    to: Existence
  ): Field[OdbCtx[F], Unit] = {
    val name = to.fold("delete", "undelete")
    val set  = ObservationModel.PropertiesInput.Empty.copy(existence = to.assign)

    // Need a custom decoder because we don't want to look for a "patch" field.
    implicit val decoder: Decoder[ObservationModel.UpdateInput] =
      (c: HCursor) => for {
        where <- c.downField("WHERE").as[Option[WhereObservationInput]]
        limit <- c.downField("LIMIT").as[Option[NonNegInt]]
      } yield ObservationModel.UpdateInput(set, where, limit)

    val arg   =
      to.fold(InputObjectTypeObservationDelete, InputObjectTypeObservationUndelete)
         .argument(
           "input",
           s"Parameters used to select observations for $name"
         )

    Field(
      name        = s"${name}Observations",
      description = s"${name.capitalize}s all the observations identified by the `WHERE` field".some,
      fieldType   = UpdateResultType(s"${name.capitalize}ObservationsResult", "observations", ObservationType[F]),
      arguments   = List(arg),
      resolve     = c => c.observation(_.update(c.arg(arg)))
    )
  }

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      updateObservations,
      clone,
      updateAsterisms,
      existenceEditField(Existence.Deleted),
      existenceEditField(Existence.Present),
    )

}

object ObservationMutation extends ObservationMutation
