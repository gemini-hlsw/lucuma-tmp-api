// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.functor._
import lucuma.core.enums.{TacCategory, ToOActivation}
import lucuma.core.model.{Partner, Program}
import lucuma.odb.api.model.{Existence, PlannedTimeSummaryModel, ProgramModel, ProposalClassInput, ProposalInput, WhereProgramInput}
import lucuma.odb.api.model.ProposalClassInput._
import lucuma.odb.api.model.query.{SizeLimitedResult, WhereEqInput, WhereOrderInput}
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

object ProgramSchema {

  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, InputObjectTypeWhereEqExistence, PlannedTimeSummaryType}
  import ObservationSchema.{ArgumentOptionOffsetObservation, ObservationSelectResult}
  import ProposalSchema.{ProposalType, InputObjectWhereProposal}
  import RefinedSchema.{IntPercentType, NonEmptyStringType, NonNegIntType}
  import QuerySchema._
  import TimeSchema.InputObjectTypeNonNegDuration
  import context._
  import syntax.`enum`._
  import syntax.inputobjecttype._
  import syntax.inputtype._

  implicit val ProgramIdType: ScalarType[Program.Id] =
    ObjectIdSchema.gidType[Program.Id]("ProgramId")

  val ArgumentProgramId: Argument[Program.Id] =
    Argument(
      name         = "programId",
      argumentType = ProgramIdType,
      description  = "Program ID"
    )

  val ArgumentOptionProgramId: Argument[Option[Program.Id]] =
    Argument(
      name         = "programId",
      argumentType = OptionInputType(ProgramIdType),
      description  = "Program ID"
    )

  implicit val InputObjectWhereOrderProgramId: InputObjectType[WhereOrderInput[Program.Id]] =
    inputObjectWhereOrder[Program.Id]("ProgramId", ProgramIdType)

  implicit val InputObjectWhereProgram: InputObjectType[WhereProgramInput] =
    InputObjectType[WhereProgramInput](
      "WhereProgram",
      "Program filter options.  All specified items must match.",
      () =>
        combinatorFields(InputObjectWhereProgram, "program") :::
          List(
            InputObjectWhereOrderProgramId.optionField("id", "Matches the program ID."),
            InputObjectWhereOptionString.optionField("name", "Matches the program name."),
            InputObjectWhereProposal.optionField("proposal", "Matches the proposal."),
            InputField("existence", OptionInputType(InputObjectTypeWhereEqExistence), "By default matching is limited to PRESENT programs.  Use this filter to included DELETED programs for example.", WhereEqInput.EQ(Existence.Present: Existence).some)
          )
    )

  implicit val ArgumentOptionWhereProgram: Argument[Option[WhereProgramInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereProgram),
      description  = "Filters the selection of programs."
    )

  implicit val ArgumentOptionOffsetProgram: Argument[Option[Program.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(ProgramIdType),
      description  = "Starts the result set at (or after if not existent) the given program id."
    )

  implicit def ProgramSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SizeLimitedResult[ProgramModel]] =
    SelectResultType[ProgramModel]("program", ProgramType[F])

  def ProgramType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ProgramModel] =
    ObjectType(
      name     = "Program",
      fieldsFn = () => fields(

        Field(
          name        = "id",
          fieldType   = ProgramIdType,
          description = Some("Program ID"),
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = Some("DELETED or PRESENT"),
          resolve     = _.value.existence
        ),

        Field(
          name        = "name",
          fieldType   = OptionType(NonEmptyStringType),
          description = Some("Program name"),
          resolve     = _.value.name
        ),

        Field(
          name        = "proposal",
          fieldType   = OptionType(ProposalType),
          description = Some("Program proposal"),
          resolve     = _.value.proposal
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationSelectResult[F],
          description = Some("All observations associated with the program."),
          arguments   = List(
            ArgumentIncludeDeleted,
            ArgumentOptionOffsetObservation,
            ArgumentOptionLimit
          ),
          resolve     = c =>
            c.observation(_.selectForProgram(c.value.id, c.includeDeleted, c.arg(ArgumentOptionOffsetObservation), c.resultSetLimit))
        ),

        Field(
          name        = "plannedTime",
          fieldType   = PlannedTimeSummaryType,
          description = Some("Program planned time calculation."),
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.observation {
            _.selectForProgram(c.value.id, c.includeDeleted, None)
             .map(_.limitedValues.foldMap(PlannedTimeSummaryModel.forObservation))
          }
        )

      )
    )

  def programs[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "programs",
      fieldType   = ProgramSelectResult[F],
      description = "Selects the first `LIMIT` matching programs based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(ArgumentOptionWhereProgram, ArgumentOptionOffsetProgram, ArgumentOptionLimit),
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereProgram).getOrElse(WhereProgramInput.MatchPresent)
        val off   = c.arg(ArgumentOptionOffsetProgram)
        val limit = c.resultSetLimit
        c.program(_.selectWhere(where, off, limit))
      }
    )

  def program[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "program",
      fieldType   = OptionType(ProgramType[F]),
      description = "Returns the program with the given id, if any.".some,
      arguments   = List(ArgumentProgramId, ArgumentIncludeDeleted),
      resolve     = c => c.program(_.select(c.programId, c.includeDeleted))
    )

  def queryFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      programs,
      program
    )

  implicit val InputObjectTypeClassicalInput: InputObjectType[ClassicalInput] =
    deriveInputObjectType[ClassicalInput](
      InputObjectTypeName("ClassicalInput"),
      InputObjectTypeDescription("Classical observing at Gemini"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "classical"))
    )

  implicit val InputObjectTypeDemoScienceInput: InputObjectType[DemoScienceInput] =
    deriveInputObjectType[DemoScienceInput](
      InputObjectTypeName("DemoScienceInput"),
      InputObjectTypeDescription("Demo science"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "demoScience"))
    )

  implicit val InputObjectTypeDirectorsTimeInput: InputObjectType[DirectorsTimeInput] =
    deriveInputObjectType[DirectorsTimeInput](
      InputObjectTypeName("DirectorsTimeInput"),
      InputObjectTypeDescription("Director's time"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "directorsTime"))
    )

  implicit val InputObjectTypeExchangeInput: InputObjectType[ExchangeInput] =
    deriveInputObjectType[ExchangeInput](
      InputObjectTypeName("ExchangeInput"),
      InputObjectTypeDescription("Exchange observing at Keck/Subaru"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "exchange"))
    )

  implicit val InputObjectTypeFastTurnaroundInput: InputObjectType[FastTurnaroundInput] =
    deriveInputObjectType[FastTurnaroundInput](
      InputObjectTypeName("FastTurnaroundInput"),
      InputObjectTypeDescription("Fast turnaround observing at Gemini"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "fastTurnaround"))
    )

  implicit val InputObjectTypePoorWeatherInput: InputObjectType[PoorWeatherInput] =
    deriveInputObjectType[PoorWeatherInput](
      InputObjectTypeName("PoorWeatherInput"),
      InputObjectTypeDescription("Poor weather"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "poorWeather"))
    )

  implicit val InputObjectTypeQueueInput: InputObjectType[QueueInput] =
    deriveInputObjectType[QueueInput](
      InputObjectTypeName("QueueInput"),
      InputObjectTypeDescription("Queue observing at Gemini"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "queue"))
    )

  implicit val InputObjectTypeSystemVerificationInput: InputObjectType[SystemVerificationInput] =
    deriveInputObjectType[SystemVerificationInput](
      InputObjectTypeName("SystemVerificationInput"),
      InputObjectTypeDescription("System verification"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "systemVerification"))
    )

  implicit val InputObjectTypeLargeProgramInput: InputObjectType[LargeProgramInput] =
    deriveInputObjectType[LargeProgramInput](
      InputObjectTypeName("LargeProgramInput"),
      InputObjectTypeDescription("Large program observing at Gemini"),
      ReplaceInputField("minPercentTime",      IntPercentType.createRequiredEditOptional("minPercentTime", "largeProgram")),
      ReplaceInputField("minPercentTotalTime", IntPercentType.createRequiredEditOptional("minPercentTotalTime", "largeProgram")),
      ReplaceInputField("totalTime",           InputObjectTypeNonNegDuration.createRequiredEditOptional("totalTime", "largeProgram"))
    )

  implicit val InputObjectTypeIntensiveInput: InputObjectType[IntensiveInput] =
    deriveInputObjectType[IntensiveInput](
      InputObjectTypeName("IntensiveInput"),
      InputObjectTypeDescription("Intensive program observing at Subaru"),
      ReplaceInputField("minPercentTime",      IntPercentType.createRequiredEditOptional("minPercentTime", "intensive")),
      ReplaceInputField("minPercentTotalTime", IntPercentType.createRequiredEditOptional("minPercentTotalTime", "intensive")),
      ReplaceInputField("totalTime",           InputObjectTypeNonNegDuration.createRequiredEditOptional("totalTime", "intensive"))
    )

  implicit val InputObjectTypeProposalClassInput: InputObjectType[ProposalClassInput] =
    InputObjectType[ProposalClassInput](
      "ProposalClassInput",
      "Proposal class. Choose exactly one class type",
      List(
        InputObjectTypeClassicalInput.optionField("classical", "Classical observing at Gemini"),
        InputObjectTypeDemoScienceInput.optionField("demoScience", "Demo science"),
        InputObjectTypeDirectorsTimeInput.optionField("directorsTime", "Director's time"),
        InputObjectTypeExchangeInput.optionField("exchange", "Exchange observing at Keck/Subaru"),
        InputObjectTypeFastTurnaroundInput.optionField("fastTurnaround", "Fast turnaround observing at Gemini"),
        InputObjectTypePoorWeatherInput.optionField("poorWeather", "Poor weather"),
        InputObjectTypeQueueInput.optionField("queue", "Queue observing at Gemini"),
        InputObjectTypeSystemVerificationInput.optionField("systemVerification", "System verification"),
        InputObjectTypeLargeProgramInput.optionField("largeProgram", "Large program observing at Gemini"),
        InputObjectTypeIntensiveInput.optionField("intensive", "Intensive program observing at Subaru")
      )
    )

  implicit val EnumTypeTacCategory: EnumType[TacCategory] =
    EnumType.fromEnumerated(
      "tacCategory",
      "TAC category"
    )

  implicit val EnumTypeToOActivation: EnumType[ToOActivation] =
    EnumType.fromEnumerated(
      "toOActivation",
      "Target of opportunity activation"
    )

  implicit val EnumTypePartner: EnumType[Partner] =
    EnumType.fromEnumerated(
      "partner",
      "Partner"
    )

  implicit val InputObjectTypePartnerSplitInput: InputObjectType[ProposalInput.PartnerSplitInput] =
    deriveInputObjectType[ProposalInput.PartnerSplitInput](
      InputObjectTypeName("PartnerSplitsInput"),
      InputObjectTypeDescription("Partner time allocation: must be empty or sum to 100%"),
      ReplaceInputField("partner", EnumTypePartner.notNullableField("partner")),
      ReplaceInputField("percent", IntPercentType.notNullableField("percent"))
    )

  implicit val InputObjectProposalInput: InputObjectType[ProposalInput] =
    deriveInputObjectType[ProposalInput](
      InputObjectTypeName("ProposalInput"),
      InputObjectTypeDescription("Program proposal"),
      ReplaceInputField("title",         NonEmptyStringType.nullableField("title")),
      ReplaceInputField("proposalClass", InputObjectTypeProposalClassInput.createRequiredEditOptional("proposalClass", "proposal")),
      ReplaceInputField("category",      EnumTypeTacCategory.nullableField("category")),
      ReplaceInputField("toOActivation", EnumTypeToOActivation.createRequiredEditOptional("toOActivation", "proposal")),
      ReplaceInputField("abstrakt",      NonEmptyStringType.nullableField("abstract")),
      ReplaceInputField("partnerSplits", ListInputType(InputObjectTypePartnerSplitInput).createRequiredEditOptional("partnerSplits", "proposal"))
    )

  implicit val InputObjectTypeProgramProperties: InputObjectType[ProgramModel.PropertiesInput] =
    InputObjectType[ProgramModel.PropertiesInput](
      "ProgramPropertiesInput",
      "Program properties",
      List(
        NonEmptyStringType.optionField("name", "The program name, which is both optional and nullable"),
        InputObjectProposalInput.optionField("proposal", "The program proposal is both optional and nullable"),
        EnumTypeExistence.optionField("existence", "Whether the program is considered deleted (defaults to PRESENT) but may be edited")
      )
    )

  val InputObjectTypeProgramCreate: InputObjectType[ProgramModel.CreateInput] =
    deriveInputObjectType[ProgramModel.CreateInput](
      InputObjectTypeName("CreateProgramInput"),
      InputObjectTypeDescription("Program creation parameters")
    )

  val ArgumentProgramCreate: Argument[ProgramModel.CreateInput] =
    InputObjectTypeProgramCreate.argument(
      "input",
      "Program description"
    )

  val InputObjectTypeUpdatePrograms: InputObjectType[ProgramModel.UpdateInput] =
    InputObjectType[ProgramModel.UpdateInput](
      "UpdateProgramsInput",
      "Program selection and update description.  Use `SET` to specify the changes, `WHERE` to select the programs to update, and `LIMIT` to control the size of the return value.",
      List(
        InputField("SET", InputObjectTypeProgramProperties, "Describes the program values to modify."),
        InputObjectWhereProgram.optionField("WHERE", "Filters the programs to be updated according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional programs match the WHERE clause they will be updated but not returned).")
      )
    )

  val ArgumentUpdatePrograms: Argument[ProgramModel.UpdateInput] =
    InputObjectTypeUpdatePrograms.argument(
      "input",
      "Parameters for updating existing programs."
    )

   def CreateProgramResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ProgramModel.CreateResult] =
    ObjectType(
      name        = "CreateProgramResult",
      description = "The result of creating a new program.",
      fieldsFn    = () => fields(

        Field(
          name        = "program",
          description = "The newly created program.".some,
          fieldType   = ProgramType[F],
          resolve     = _.value.program
        )

      )
    )

  def create[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "createProgram",
      fieldType   = CreateProgramResultType[F],
      description = "Creates a new program according to provided properties".some,
      arguments   = List(ArgumentProgramCreate),
      resolve     = c => c.program(_.insert(c.arg(ArgumentProgramCreate)))
    )

  def update[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "updatePrograms",
      fieldType = UpdateResultType("UpdateProgramsResult", "programs", ProgramType[F]),
      arguments = List(ArgumentUpdatePrograms),
      resolve   = c => c.program(_.update(c.arg(ArgumentUpdatePrograms)))
    )

  def mutationFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      update
    )

}
