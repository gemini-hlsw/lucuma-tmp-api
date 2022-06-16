// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.enums.{TacCategory, ToOActivation}
import lucuma.core.model.{Partner, Proposal}
import lucuma.odb.api.model.{PartnerSplit, ProposalClassEnum, WhereProposal, WhereProposalClass, WhereProposalPartnerEntry, WhereProposalPartners}
import lucuma.odb.api.model.query.{WhereEq, WhereOptionEq}
import sangria.macros.derive.{DocumentInputField, InputObjectTypeDescription, InputObjectTypeName, ReplaceInputField, deriveInputObjectType}
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

  implicit val InputObjectWhereEqProposalClassEnum: InputObjectType[WhereEq[ProposalClassEnum]] =
    deriveInputObjectType(
      InputObjectTypeName("WhereProposalClassType")
    )

  implicit val InputObjectWhereEqPartner: InputObjectType[WhereEq[Partner]] =
    deriveInputObjectType(
      InputObjectTypeName("WhereEqPartner")
    )

  implicit val InputObjectWhereProposalPartnerEntry: InputObjectType[WhereProposalPartnerEntry] =
    deriveInputObjectType(
      InputObjectTypeName("WhereProposalPartnerEntry")
    )

  implicit val InputObjectWhereProposalPartners: InputObjectType[WhereProposalPartners] =
    deriveInputObjectType(
      InputObjectTypeName("WhereProposalPartners")
    )

  implicit val InputObjectWhereEqTacCategory: InputObjectType[WhereOptionEq[TacCategory]] =
    deriveInputObjectType(
      InputObjectTypeName("WhereTacCategory")
    )

  implicit val InputObjectWhereEqToOActivation: InputObjectType[WhereEq[ToOActivation]] =
    deriveInputObjectType(
      InputObjectTypeName("WhereToOActivation")
    )

  implicit val InputObjectWhereProposalClass: InputObjectType[WhereProposalClass] =
    deriveInputObjectType[WhereProposalClass](
      InputObjectTypeName("WhereProposalClass"),
      InputObjectTypeDescription("Proposal class filter options."),
      ReplaceInputField("classType", OptionInputType(InputObjectWhereEqProposalClassEnum).optionField("type"))
    )

  implicit val InputObjectWhereProposal: InputObjectType[WhereProposal] =
    deriveInputObjectType[WhereProposal](
      InputObjectTypeName("WhereProposal"),
      InputObjectTypeDescription("Proposal filter options.  All specified items must match."),
      ReplaceInputField("abstrakt", InputObjectWhereOptionString.optionField("abstract")),
      ReplaceInputField("clazz",    OptionInputType(InputObjectWhereProposalClass).optionField("class")),
      DocumentInputField("AND",     document.andField("proposal")),
      DocumentInputField("OR",      document.orField("proposal")),
      DocumentInputField("NOT",     document.notField("proposal")),
      DocumentInputField("IS_NULL", document.isNullField("proposal"))
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
