// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.functor._
import lucuma.core.model.Program
import lucuma.odb.api.model.{Existence, PlannedTimeSummaryModel, ProgramModel, WhereProgramInput}
import lucuma.odb.api.model.query.{WhereEqInput, WhereOrderInput}
import lucuma.odb.api.repo.OdbCtx
import org.typelevel.log4cats.Logger
import sangria.marshalling.circe._
import sangria.schema._

import scala.collection.immutable.Seq

object ProgramSchema {

  import GeneralSchema.{ArgumentIncludeDeleted, EnumTypeExistence, InputObjectTypeWhereEqExistence, PlannedTimeSummaryType}
  import ObservationSchema.{ArgumentOptionOffsetObservation, ObservationSelectResult}
  import ProposalSchema.{ProposalType, InputObjectWhereProposal}
  import RefinedSchema.NonEmptyStringType
  import QuerySchema._
  import context._
  import syntax.inputtype._

  implicit val ProgramIdType: ScalarType[Program.Id] =
    ObjectIdSchema.gidType[Program.Id]("ProgramId")

  val ProgramIdArgument: Argument[Program.Id] =
    Argument(
      name         = "programId",
      argumentType = ProgramIdType,
      description  = "Program ID"
    )

  val OptionalProgramIdArgument: Argument[Option[Program.Id]] =
    Argument(
      name         = "programId",
      argumentType = OptionInputType(ProgramIdType),
      description  = "Program ID"
    )

  implicit val InputObjectWhereOrderProgramId: InputObjectType[WhereOrderInput[Program.Id]] =
    inputObjectWhereOrder[Program.Id]("ProgramId", ProgramIdType)

  val OptionalListProgramIdArgument: Argument[Option[Seq[Program.Id]]] =
    Argument(
      name         = "programIds",
      argumentType = OptionInputType(ListInputType(ProgramIdType)),
      description  = "Program Ids"
    )

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

}
