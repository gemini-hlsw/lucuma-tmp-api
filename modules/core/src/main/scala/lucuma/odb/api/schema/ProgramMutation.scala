// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.option._
import lucuma.core.enums.{TacCategory, ToOActivation}
import lucuma.core.model.Partner
import lucuma.odb.api.model.{ProgramModel, ProposalInput, ProposalClassInput}
import lucuma.odb.api.schema.syntax.inputtype._
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.ProgramSchema.ProgramType
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ProgramMutation {

  import GeneralSchema.EnumTypeExistence
  import ProgramSchema.ProgramIdType
  import RefinedSchema.IntPercentType
  import TimeSchema.InputObjectTypeNonNegDuration
  import context._
  import RefinedSchema.NonEmptyStringType
  import syntax.`enum`._
  import syntax.inputobjecttype._

  import ProposalClassInput._

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

  implicit val InputObjectTypeProgramSelect: InputObjectType[ProgramModel.SelectInput] =
    InputObjectType[ProgramModel.SelectInput](
      "ProgramSelectInput",
      "Choose programId to select the program for editing",
      List(
        ProgramIdType.optionField("programId")
      )
    )

  val InputObjectTypeProgramEdit: InputObjectType[ProgramModel.EditInput] =
    InputObjectType[ProgramModel.EditInput](
      "EditProgramInput",
      "Program selection and update description",
      List(
        InputField("select", InputObjectTypeProgramSelect),
        InputField("patch",  InputObjectTypeProgramProperties)
      )
    )

  val ArgumentProgramEdit: Argument[ProgramModel.EditInput] =
    InputObjectTypeProgramEdit.argument(
      "input",
      "Parameters for editing an existing program"
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

  def EditProgramResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], ProgramModel.EditResult] =
    ObjectType(
      name        = "EditProgramResult",
      description = "The result of editing the selected program.",
      fieldsFn    = () => fields(

        Field(
          name        = "program",
          description = "The edited program.".some,
          fieldType   = OptionType(ProgramType[F]),
          resolve     = _.value.program
        )
      )
    )

  def edit[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "editProgram",
      fieldType = EditProgramResultType[F],
      arguments = List(ArgumentProgramEdit),
      resolve   = c => c.program(_.edit(c.arg(ArgumentProgramEdit)))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      edit
    )

}

object ProgramMutation extends ProgramMutation
