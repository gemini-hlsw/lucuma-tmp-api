// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import lucuma.core.enum.{TacCategory, ToOActivation}
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
      InputObjectTypeDescription("Classical proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "classical"))
    )

  implicit val InputObjectTypeDemoScienceInput: InputObjectType[DemoScienceInput] =
    deriveInputObjectType[DemoScienceInput](
      InputObjectTypeName("DemoScienceInput"),
      InputObjectTypeDescription("Demo science proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "demoScience"))
    )

  implicit val InputObjectTypeDirectorsTimeInput: InputObjectType[DirectorsTimeInput] =
    deriveInputObjectType[DirectorsTimeInput](
      InputObjectTypeName("DirectorsTimeInput"),
      InputObjectTypeDescription("Directors time proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "directorsTime"))
    )

  implicit val InputObjectTypeExchangeInput: InputObjectType[ExchangeInput] =
    deriveInputObjectType[ExchangeInput](
      InputObjectTypeName("ExchangeInput"),
      InputObjectTypeDescription("Exchange proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "exchange"))
    )

  implicit val InputObjectTypeFastTurnaroundInput: InputObjectType[FastTurnaroundInput] =
    deriveInputObjectType[FastTurnaroundInput](
      InputObjectTypeName("FastTurnaroundInput"),
      InputObjectTypeDescription("Fast turnaround proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "fastTurnaround"))
    )

  implicit val InputObjectTypePoorWeatherInput: InputObjectType[PoorWeatherInput] =
    deriveInputObjectType[PoorWeatherInput](
      InputObjectTypeName("PoorWeatherInput"),
      InputObjectTypeDescription("Poor weather proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "poorWeather"))
    )

  implicit val InputObjectTypeQueueInput: InputObjectType[QueueInput] =
    deriveInputObjectType[QueueInput](
      InputObjectTypeName("QueueInput"),
      InputObjectTypeDescription("Queue proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "queue"))
    )

  implicit val InputObjectTypeSystemVerificationInput: InputObjectType[SystemVerificationInput] =
    deriveInputObjectType[SystemVerificationInput](
      InputObjectTypeName("SystemVerificationInput"),
      InputObjectTypeDescription("System verification proposal"),
      ReplaceInputField("minPercentTime", IntPercentType.createRequiredEditOptional("minPercentTime", "systemVerification"))
    )

  implicit val InputObjectTypeLargeProgramInput: InputObjectType[LargeProgramInput] =
    deriveInputObjectType[LargeProgramInput](
      InputObjectTypeName("LargeProgramInput"),
      InputObjectTypeDescription("Large program proposal"),
      ReplaceInputField("minPercentTime",      IntPercentType.createRequiredEditOptional("minPercentTime", "largeProgram")),
      ReplaceInputField("minPercentTotalTime", IntPercentType.createRequiredEditOptional("minPercentTotalTime", "largeProgram")),
      ReplaceInputField("totalTime",           InputObjectTypeNonNegDuration.createRequiredEditOptional("totalTime", "largeProgram"))
    )

  implicit val InputObjectTypeIntensiveInput: InputObjectType[IntensiveInput] =
    deriveInputObjectType[IntensiveInput](
      InputObjectTypeName("IntensiveInput"),
      InputObjectTypeDescription("Intensive proposal"),
      ReplaceInputField("minPercentTime",      IntPercentType.createRequiredEditOptional("minPercentTime", "intensive")),
      ReplaceInputField("minPercentTotalTime", IntPercentType.createRequiredEditOptional("minPercentTotalTime", "intensive")),
      ReplaceInputField("totalTime",           InputObjectTypeNonNegDuration.createRequiredEditOptional("totalTime", "intensive"))
    )

  implicit val InputObjectTypeProposalClassInput: InputObjectType[ProposalClassInput] =
    InputObjectType[ProposalClassInput](
      "ProposalClassInput",
      "Proposal class. Choose exactly one class type",
      List(
        InputField("classical",          OptionInputType(InputObjectTypeClassicalInput),          "classical"),
        InputField("demoScience",        OptionInputType(InputObjectTypeDemoScienceInput),        "demoScience"),
        InputField("directorsTime",      OptionInputType(InputObjectTypeDirectorsTimeInput),      "directorsTime"),
        InputField("exchange",           OptionInputType(InputObjectTypeExchangeInput),           "exchange"),
        InputField("fastTurnaround",     OptionInputType(InputObjectTypeFastTurnaroundInput),     "fastTurnaround"),
        InputField("poorWeather",        OptionInputType(InputObjectTypePoorWeatherInput),        "poorWeather"),
        InputField("queue",              OptionInputType(InputObjectTypeQueueInput),              "queue"),
        InputField("systemVerification", OptionInputType(InputObjectTypeSystemVerificationInput), "systemVerification"),
        InputField("largeProgram",       OptionInputType(InputObjectTypeLargeProgramInput),       "largeProgram"),
        InputField("intensive",          OptionInputType(InputObjectTypeIntensiveInput),          "intensive")
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

  val InputObjectTypeProgramCreate: InputObjectType[ProgramModel.Create] =
    deriveInputObjectType[ProgramModel.Create](
      InputObjectTypeName("CreateProgramInput"),
      InputObjectTypeDescription("Program creation parameters"),
      ReplaceInputField("proposal", InputObjectProposalInput.nullableField("proposal"))
    )

  val ArgumentProgramCreate: Argument[ProgramModel.Create] =
    InputObjectTypeProgramCreate.argument(
      "input",
      "Program description"
    )

  val InputObjectTypeProgramEdit: InputObjectType[ProgramModel.Edit] =
    deriveInputObjectType[ProgramModel.Edit](
      InputObjectTypeName("EditProgramInput"),
      InputObjectTypeDescription("Edit program"),
      ReplaceInputField("existence",     EnumTypeExistence.notNullableField("existence")),
      ReplaceInputField("name",          NonEmptyStringType.nullableField("name")),
      ReplaceInputField("proposal",      InputObjectProposalInput.nullableField("proposal"))
    )

  val ArgumentProgramEdit: Argument[ProgramModel.Edit] =
    InputObjectTypeProgramEdit.argument(
      "input",
      "Edit program"
    )

  def create[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "createProgram",
      fieldType = OptionType(ProgramType[F]),
      arguments = List(ArgumentProgramCreate),
      resolve   = c => c.program(_.insert(c.arg(ArgumentProgramCreate)))
    )

  def update[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name      = "updateProgram",
      fieldType = ProgramType[F],
      arguments = List(ArgumentProgramEdit),
      resolve   = c => c.program(_.edit(c.arg(ArgumentProgramEdit)))
    )

  def allFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      create,
      update
    )

}

object ProgramMutation extends ProgramMutation
