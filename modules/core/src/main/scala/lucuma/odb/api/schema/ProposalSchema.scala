// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.enums.{TacCategory, ToOActivation}
import lucuma.core.model.{Partner, Proposal}
import lucuma.odb.api.model.{PartnerSplit, ProposalClassEnum, WhereProposalInput, WhereProposalClassInput, WhereProposalPartnerEntryInput, WhereProposalPartnersInput}
import lucuma.odb.api.model.query.{WhereEqInput, WhereOptionEqInput}
import sangria.schema._

object ProposalSchema {

  import ProposalClassSchema.ProposalClassType
  import RefinedSchema.{IntPercentType, NonEmptyStringType}
  import QuerySchema._
  import syntax.all._

  implicit val EnumTypePartner: EnumType[Partner] =
    EnumType.fromEnumerated("Partner", "Partner")

  implicit val EnumTypeTacCategory: EnumType[TacCategory] =
    EnumType.fromEnumerated("TacCategory", "TAC Category")

  implicit val EnumTypeToOActivation: EnumType[ToOActivation] =
    EnumType.fromEnumerated("ToOActivation", "ToO Activation")

  implicit val EnumTypeProposalClassEnum: EnumType[ProposalClassEnum] =
    EnumType.fromEnumerated("ProposalClassEnum", "Proposal class type")

  implicit val InputObjectWhereEqProposalClassEnum: InputObjectType[WhereEqInput[ProposalClassEnum]] =
    inputObjectWhereEq("ProposalClassType", EnumTypeProposalClassEnum)

  implicit val InputObjectWhereEqPartner: InputObjectType[WhereEqInput[Partner]] =
    inputObjectWhereEq("Partner", EnumTypePartner)

  implicit val InputObjectWhereProposalPartnerEntry: InputObjectType[WhereProposalPartnerEntryInput] =
    InputObjectType[WhereProposalPartnerEntryInput](
      "WhereProposalPartnerEntry",
      "Proposal partner entry filter options. The set of partners is scanned for a matching partner and percentage entry.",
      () =>
        combinatorFields(InputObjectWhereProposalPartnerEntry, "partner entry") :::
          List(
            InputObjectWhereEqPartner.optionField("partner", "Matches on partner equality"),
            InputObjectWhereOrderInt.optionField("percent", "Matches on partner percentage")
          )
    )

  implicit val InputObjectWhereProposalPartners: InputObjectType[WhereProposalPartnersInput] =
    InputObjectType[WhereProposalPartnersInput](
      "WhereProposalPartners",
      "Proposal partners matching.  Use `MATCH` for detailed matching options, `EQ` to just match against a partners list, and/or `isJoint` for checking joint vs individual proposals",
      List(
        InputObjectWhereProposalPartnerEntry.optionField("MATCH", "Detailed partner matching.  Use EQ instead of a simple exact match."),
        ListInputType(EnumTypePartner).optionField("EQ", "A simple exact match for the supplied partners. Use `MATCH` instead for more advanced options."),
        BooleanType.optionField("isJoint", "Matching based on whether the proposal is a joint (i.e., multi-partner) proposal.")
      )
    )

  implicit val InputObjectWhereEqTacCategory: InputObjectType[WhereOptionEqInput[TacCategory]] =
    inputObjectWhereOptionEq("TacCategory", EnumTypeTacCategory)

  implicit val InputObjectWhereEqToOActivation: InputObjectType[WhereEqInput[ToOActivation]] =
    inputObjectWhereEq("ToOActivation", EnumTypeToOActivation)

  implicit val InputObjectWhereProposalClass: InputObjectType[WhereProposalClassInput] =
    InputObjectType[WhereProposalClassInput](
      "WhereProposalClass",
      "Proposal class filter options.",
      List(
        InputObjectWhereEqProposalClassEnum.optionField("type", "Proposal class type match."), // classType
        InputObjectWhereOrderInt.optionField("minPercent", "Minimum acceptable percentage match.")
      )
    )

  implicit val InputObjectWhereProposal: InputObjectType[WhereProposalInput] =
    InputObjectType[WhereProposalInput](
      "WhereProposal",
      "Proposal filter options.  All specified items must match.",
      () =>
        isNullField("proposal")                                  ::
          combinatorFields(InputObjectWhereProposal, "proposal") :::
          List(
            InputObjectWhereOptionString.optionField("title", "Matches the proposal title."),
            InputObjectWhereProposalClass.optionField("class", "Matches the proposal class."),  // clazz,
            InputObjectWhereEqTacCategory.optionField("category", "Matches the proposal TAC category."),
            InputObjectWhereEqToOActivation.optionField("toOActivation", "Matches the Target of Opportunity setting."),
            InputObjectWhereOptionString.optionField("abstract", "Matches the proposal abstract."), // abstrakt
            InputObjectWhereProposalPartners.optionField("partners", "Matches proposal partners.")
          )
    )

  implicit val PartnerSplitType: ObjectType[Any, PartnerSplit] =
    ObjectType(
      name   = "PartnerSplit",
      fieldsFn = () =>
        fields(
          Field(
            name        = "partner",
            fieldType   = EnumTypePartner,
            description = Some("Partner"),
            resolve     = _.value.partner
          ),
          Field(
            name        = "percent",
            fieldType   = IntPercentType,
            description = Some("Percentage of observation time"),
            resolve     = _.value.percent
          ),
        )
    )

  implicit val ProposalType: ObjectType[Any, Proposal] =
    ObjectType(
      name  = "Proposal",
      fieldsFn = () =>
        fields(
          Field(
            name        = "title",
            fieldType   = OptionType(NonEmptyStringType),
            description = Some("Proposal title"),
            resolve     = _.value.title
          ),
          Field(
            name        = "proposalClass",
            fieldType   = ProposalClassType,
            description = Some("Proposal class"),
            resolve     = _.value.proposalClass
          ),
          Field(
            name        = "category",
            fieldType   = OptionType(EnumTypeTacCategory),
            description = Some("Proposal TAC category"),
            resolve     = _.value.category
          ),
          Field(
            name        = "toOActivation",
            fieldType   = OptionType(EnumTypeToOActivation),
            description = Some("Target of Opportunity activation"),
            resolve     = _.value.toOActivation
          ),
          Field(
            name        = "abstract",
            fieldType   = OptionType(NonEmptyStringType),
            description = Some("Abstract"),
            resolve     = _.value.abstrakt
          ),
          Field(
            name        = "partnerSplits",
            fieldType   = ListType(PartnerSplitType),
            description = Some("Partner time allocations"),
            resolve     = _.value.partnerSplits.toList.map{ case (partner, percent) => PartnerSplit(partner, percent)}
          )
        )
    )
}
