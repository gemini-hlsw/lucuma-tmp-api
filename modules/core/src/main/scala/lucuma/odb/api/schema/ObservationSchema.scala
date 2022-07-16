// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import clue.data.syntax._
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.{Decoder, HCursor}
import io.circe.refined._
import lucuma.core.enums.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{ConstraintSet, Observation}
import lucuma.odb.api.model.{Existence, ObservationModel, PlannedTimeSummaryModel, ScienceMode, ScienceRequirements, WhereObservationInput}
import lucuma.odb.api.model.ObservationModel.BulkEdit
import lucuma.odb.api.model.targetModel.{EditAsterismPatchInput, TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.model.query.{SizeLimitedResult, WhereOrderInput}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.TargetSchema.TargetEnvironmentType
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


object ObservationSchema {

  import ConstraintSetSchema.{ConstraintSetType, InputObjectTypeConstraintSet}
  import ScienceModeSchema._
  import ExecutionSchema.ExecutionType
  import ItcSchema.ItcSuccessType
  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, PlannedTimeSummaryType}
  import PosAngleConstraintSchema._
  import ProgramSchema.{ProgramIdType, ProgramType, InputObjectWhereOrderProgramId}
  import RefinedSchema.{NonEmptyStringType, NonNegIntType}
  import QuerySchema._
  import ScienceModeSchema.InputObjectTypeScienceMode
  import ScienceRequirementsSchema.{InputObjectTypeScienceRequirements, ScienceRequirementsType}
  import TargetSchema.{InputObjectTypeEditAsterisms, InputObjectTypeTargetEnvironment, TargetType}
  import TimeSchema.InstantScalar

  import context._
  import syntax.`enum`._
  import syntax.inputtype._
  import syntax.inputobjecttype._

  implicit val ObservationIdType: ScalarType[Observation.Id] =
    ObjectIdSchema.gidType[Observation.Id](name = "ObservationId")

  implicit val ObsStatusType: EnumType[ObsStatus] =
    EnumType.fromEnumerated(
      "ObsStatus",
      "Observation status options"
    )

  implicit val ObsActiveStatusType: EnumType[ObsActiveStatus] =
    EnumType.fromEnumerated(
      "ObsActiveStatus",
      "Observation operational/active status options"
    )

  implicit val InputObjectWhereOrderObservationId: InputObjectType[WhereOrderInput[Observation.Id]] =
    inputObjectWhereOrder[Observation.Id]("ObservationId", ObservationIdType)

  implicit val InputObjectWhereOrderObsStatus: InputObjectType[WhereOrderInput[ObsStatus]] =
    inputObjectWhereOrder[ObsStatus]("ObsStatus", ObsStatusType)

  implicit val InputObjectWhereOrderObsActiveStatus: InputObjectType[WhereOrderInput[ObsActiveStatus]] =
    inputObjectWhereOrder[ObsActiveStatus]("ObsActiveStatus", ObsActiveStatusType)

  implicit val InputObjectWhereObservation: InputObjectType[WhereObservationInput] =
    InputObjectType[WhereObservationInput](
      "WhereObservation",
      "Observation filter options.  All specified items must match.",
      () =>
        combinatorFields(InputObjectWhereObservation, "observation") :::
          List(
            InputObjectWhereOrderObservationId.optionField("id", "Matches the observation id."),
            InputObjectWhereOrderProgramId.optionField("programId", "Matches the id of the program associated with this observation."),
            InputObjectWhereOptionString.optionField("subtitle", "Matches the subtitle of the observation."),
            InputObjectWhereOrderObsStatus.optionField("status", "Matches the observation status."),
            InputObjectWhereOrderObsActiveStatus.optionField("activeStatus", "Matches the observation active status.")
          )
    )

  implicit val ArgumentOptionWhereObservation: Argument[Option[WhereObservationInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereObservation),
      description  = "Filters the selection of observations."
    )

  implicit val ArgumentOptionOffsetObservation: Argument[Option[Observation.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(ObservationIdType),
      description  = "Starts the result set at (or after if not existent) the given observation id."
    )

  implicit def ObservationSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SizeLimitedResult[ObservationModel]] =
    SelectResultType[ObservationModel]("observation", ObservationType[F])

  val ArgumentObservationId: Argument[Observation.Id] =
    Argument(
      name         = "observationId",
      argumentType = ObservationIdType,
      description  = "Observation ID"
    )

  val ArgumentOptionObservationId: Argument[Option[Observation.Id]] =
    Argument(
      name         = "observationId",
      argumentType = OptionInputType(ObservationIdType),
      description  = "Observation ID"
    )

  val ArgumentUseItcCache: Argument[Boolean] =
    Argument(
      name         = "useCache",
      argumentType = BooleanType,
      description  = "Whether to use cached results (true) or ignore the cache and make a remote ITC call (false).",
      defaultValue = true
    )

  def ObservationType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ObservationModel] =
    ObjectType(
      name     = "Observation",
      fieldsFn = () => fields(

        Field(
          name        = "id",
          fieldType   = ObservationIdType,
          description = Some("Observation ID"),
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = Some("DELETED or PRESENT"),
          resolve     = _.value.existence
        ),

        Field(
          name        = "title",
          fieldType   = StringType,
          description = Some("Observation title generated from id and targets"),
          resolve     = c => {
            val targets = c.value.targetEnvironment.asterism.toList.flatTraverse { tid =>
              c.ctx.odbRepo.target.selectTarget(tid).map(_.map(_.target.name.value).toList)
            }
            c.unsafeToFuture(targets.map(_.mkString(", ")))
          }
        ),

        Field(
          name        = "subtitle",
          fieldType   = OptionType(NonEmptyStringType),
          description = Some("User-supplied observation-identifying detail information"),
          resolve     = _.value.subtitle
        ),

        Field(
          name        = "status",
          fieldType   = ObsStatusType,
          description = Some("Observation status"),
          resolve     = _.value.status
        ),

        Field(
          name        = "activeStatus",
          fieldType   = ObsActiveStatusType,
          description = "Observation operational status".some,
          resolve     = _.value.activeStatus
        ),

        Field(
          name        = "visualizationTime",
          fieldType   = OptionType(InstantScalar),
          description = "Reference time used by default for visualization and time-dependent calculations (e.g., average parallactic angle)".some,
          resolve     = _.value.visualizationTime
        ),

        Field(
          name        = "posAngleConstraint",
          fieldType   = OptionType(PosAngleConstraintType),
          description = "Position angle constraint, if any.".some,
          resolve     = _.value.posAngleConstraint
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeSummaryType,
          description = Some("Observation planned time calculation."),
          resolve     = c => PlannedTimeSummaryModel.forObservation(c.value)
        ),

        Field(
          name        = "program",
          fieldType   = ProgramType[F],
          description = Some("The program that contains this observation"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.program(_.unsafeSelect(c.value.programId, c.includeDeleted))
        ),

        Field(
          name        = "targetEnvironment",
          fieldType   = TargetEnvironmentType[F],
          description = "The observation's target(s)".some,
          resolve     = c => c.target(_.unsafeSelectObservationTargetEnvironment(c.value.id))
        ),

        Field(
          name        = "constraintSet",
          fieldType   = ConstraintSetType,
          description = Some("The constraint set for the observation"),
          resolve     = c => c.value.constraintSet
        ),

        Field(
          name        = "scienceRequirements",
          fieldType   = ScienceRequirementsType[F],
          description = Some("The top level science requirements"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.scienceRequirements
        ),

        Field(
          name        = "scienceMode",
          fieldType   = OptionType(ScienceModeType),
          description = Some("The science configuration"),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.value.scienceMode
        ),

        Field(
          name        = "itc",
          fieldType   = OptionType(ItcSuccessType),
          description = "ITC execution results".some,
          arguments   = List(ArgumentUseItcCache),
          resolve     = c => c.unsafeToFuture {
            c.ctx
             .itcClient
             .query(c.value.id, c.ctx.odbRepo, useCache = c.args.arg(ArgumentUseItcCache))
             .map(_.toOption.map(_._2))
          }
        ),

        Field(
          name        = "manualConfig",
          fieldType   = OptionType(ManualConfigSchema.ManualConfigType[F]),
          description = Some("Manual instrument configuration"),
          resolve     = _.value.manualConfig
        ),

        Field(
          name        = "execution",
          fieldType   = ExecutionType[F],
          description = Some("Execution sequence and runtime artifacts"),
          resolve     = _.value.id
        )

      )
    )

  def observations[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "observations",
      fieldType   = ObservationSelectResult[F],
      description = "Selects the first `LIMIT` matching observations based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(
        ArgumentOptionWhereObservation,
        ArgumentOptionOffsetObservation,
        ArgumentOptionLimit,
        ArgumentIncludeDeleted
      ),
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereObservation).getOrElse(WhereObservationInput.MatchAll)
        val off   = c.arg(ArgumentOptionOffsetObservation)
        val limit = c.resultSetLimit
        c.observation(_.selectWhere(where, off, limit, c.includeDeleted))
      }
    )

  def observation[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "observation",
      fieldType   = OptionType(ObservationType[F]),
      description = "Returns the observation with the given id, if any.".some,
      arguments   = List(ArgumentObservationId),
      resolve     = c => c.observation(_.select(c.observationId, includeDeleted = true))
    )

  def groupByTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetModel](
      "target",
      "Observations grouped by commonly held targets",
      TargetType[F],
      (repo, pid, where, inc) => repo.groupByTargetInstantiated(pid, where, inc)
    )

  def groupByAsterism[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, Seq[TargetModel]](
      "asterism",
      "Observations grouped by commonly held science asterisms",
      ListType(TargetType[F]),
      (repo, pid, where, inc) => repo.groupByAsterismInstantiated(pid, where, inc)
    )

  def groupByTargetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, TargetEnvironmentModel](
      "targetEnvironment",
      "Observations grouped by common target environment",
      TargetEnvironmentType[F],
      (repo, pid, where, inc) => repo.groupByTargetEnvironment(pid, where, inc)
    )

  def groupByConstraintSet[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, ConstraintSet](
      "constraintSet",
      "Observations grouped by commonly held constraints",
      ConstraintSetType,
      (repo, pid, where, inc) => repo.groupByConstraintSet(pid, where, inc)
    )

  def groupByScienceMode[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, Option[ScienceMode]](
      "scienceMode",
      "Observations grouped by commonly held science mode",
      OptionType(ScienceModeType),
      (repo, pid, where, inc) => repo.groupByScienceMode(pid, where, inc)
    )

  def groupByScienceRequirements[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =

    ObservationGroupSchema.groupingField[F, ScienceRequirements](
      "scienceRequirements",
      "Observations grouped by commonly held science requirements",
      ScienceRequirementsType[F],
      (repo, pid, where, inc) => repo.groupByScienceRequirements(pid, where, inc)
    )

  def queryFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      observations,
      observation,
      groupByTarget,
      groupByAsterism,
      groupByTargetEnvironment,
      groupByConstraintSet,
      groupByScienceMode,
      groupByScienceRequirements
    )

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
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional observations match the WHERE clause they will be updated but not returned)."),
        InputField("includeDeleted", OptionInputType(BooleanType), "Set to `true` to include deleted observations.", false.some)
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

  def mutationFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      updateObservations,
      clone,
      updateAsterisms,
      existenceEditField(Existence.Deleted),
      existenceEditField(Existence.Present),
    )

}
