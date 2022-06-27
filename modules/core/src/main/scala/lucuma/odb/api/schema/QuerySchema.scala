// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegInt
import io.circe.refined._
import lucuma.odb.api.model.query.{SizeLimitedResult, WhereEqInput, WhereOptionEqInput, WhereOptionStringInput, WhereOrderInput, WhereStringInput}
import sangria.marshalling.circe._
import sangria.macros.derive.{DocumentInputField, InputObjectTypeDescription, InputObjectTypeName, deriveInputObjectType}
import sangria.schema._

object QuerySchema {

  import RefinedSchema._
  import syntax.inputtype._

  val DefaultLimit: NonNegInt =
    1000

  val ArgumentOptionLimit: Argument[Option[NonNegInt]] =
    Argument(
      name         = "LIMIT",
      argumentType =  OptionInputType(NonNegIntType.copy(description = "foo".some)),
      description  = s"Limits the result to at most this number of matches (but never more than ${SizeLimitedResult.MaxSize})."
    )

  implicit val InputObjectWhereString: InputObjectType[WhereStringInput] =
    deriveInputObjectType[WhereStringInput](
      InputObjectTypeName("WhereString"),
      InputObjectTypeDescription("String matching options."),
      DocumentInputField("LIKE",       document.like),
      DocumentInputField("NLIKE",      document.nlike),
      DocumentInputField("MATCH_CASE", document.matchCase)
    )

  implicit val InputObjectWhereOptionString: InputObjectType[WhereOptionStringInput] =
    deriveInputObjectType[WhereOptionStringInput](
      InputObjectTypeName("WhereOptionString"),
      InputObjectTypeDescription("String matching options."),
      DocumentInputField("IS_NULL",    document.isNullField("string")),
      DocumentInputField("LIKE",       document.like),
      DocumentInputField("NLIKE",      document.nlike),
      DocumentInputField("MATCH_CASE", document.matchCase)
    )

  def isNullField(name: String): InputField[_] =
    BooleanType.optionField("IS_NULL", s"When `true`, matches if the $name is not defined. When `false` matches if the $name is defined.")

  def eqFields[A](inputType: InputType[A]): List[InputField[_]] =
    List(
      inputType.optionField("EQ",  "Matches if the property is exactly the supplied value."),
      inputType.optionField("NEQ", "Matches if the property is not the supplied value."),
      ListInputType(inputType).optionField("IN",  "Matches if the property value is any of the supplied options."),
      ListInputType(inputType).optionField("NIN", "Matches if the property value is none of the supplied values.")
    )

  def orderFields[A](inputType: InputType[A]): List[InputField[_]] =
    eqFields(inputType) ::: List(
      inputType.optionField("GT",  "Matches if the property is ordered after (>) the supplied value."),
      inputType.optionField("LT",  "Matches if the property is ordered before (<) the supplied value."),
      inputType.optionField("GTE", "Matches if the property is ordered after or equal (>=) the supplied value."),
      inputType.optionField("LTE", "Matches if the property is ordered before or equal (<=) the supplied value."),
    )

  def combinatorFields[A](inputType: InputType[A], name: String): List[InputField[_]] =
    List(
      ListInputType(inputType).optionField("AND", s"A list of nested $name filters that all must match in order for the AND group as a whole to match."),
      ListInputType(inputType).optionField("OR",  s"A list of nested $name filters where any one match causes the entire OR group as a whole to match."),
      inputType.optionField("NOT", s"A nested $name filter that must not match in order for the NOT itself to match.")
    )

  def inputObjectWhereEq[A](
    name:      String,
    inputType: InputType[A]
  ): InputObjectType[WhereEqInput[A]] =
    InputObjectType[WhereEqInput[A]](
      s"WhereEq${name.capitalize}",
      """Filters on equality (or not) of the property value and the supplied criteria.
        |All supplied criteria must match, but usually only one is selected.  E.g.
        |'EQ = "Foo"' will match when the property value is "FOO".
      """.stripMargin,
      eqFields(inputType)
    )

  def inputObjectWhereOptionEq[A](
    name:      String,
    inputType: InputType[A]
  ): InputObjectType[WhereOptionEqInput[A]] =
    InputObjectType[WhereOptionEqInput[A]](
      s"WhereOptionEq${name.capitalize}",
      """Filters on equality (or not) of the property value and the supplied criteria.
        |All supplied criteria must match, but usually only one is selected.  E.g.
        |'EQ = "Foo"' will match when the property value is "FOO".  Defining, `EQ`,
        |`NEQ` etc. implies `IS_NULL` is `false`.
      """.stripMargin,
      isNullField(name) :: eqFields(inputType)
    )

  def inputObjectWhereOrder[A](
    name:      String,
    inputType: InputType[A]
  ): InputObjectType[WhereOrderInput[A]] =
    InputObjectType[WhereOrderInput[A]](
      s"WhereOrder${name.capitalize}",
      """Filters on equality or order comparisons of the property.  All supplied
        |criteria must match, but usually only one is selected.  E.g., 'GT = 2'
        |for an integer property will match when the value is 3 or more.
      """.stripMargin,
      orderFields(inputType)
    )

  implicit val InputObjectWhereOrderInt: InputObjectType[WhereOrderInput[Int]] =
    inputObjectWhereOrder[Int]("Int", IntType)

  object document {

    def isNullField(n: String): String =
      s"When `true` the $n must not be defined.  When `false` the $n must be defined."

    val like: String =
      "Performs string matching with wildcard patterns.  The entire string must be matched.  Use % to match a sequence of any characters and _ to match any single character."

    val nlike: String =
      "Performs string matching with wildcard patterns.  The entire string must not match.  Use % to match a sequence of any characters and _ to match any single character."

    val matchCase: String =
      "Set to `true` (the default) for case sensitive matches, `false` to ignore case."
  }

  def SelectResultType[A](
    prefix: String,
    aType:  OutputType[A]
  ): ObjectType[Any, SizeLimitedResult[A]] =
    ObjectType(
      name        = s"${prefix.capitalize}SelectResult",
      description = s"The matching $prefix results, limited to a maximum of ${SizeLimitedResult.MaxSize} entries.",

      fieldsFn    = () => List(

        Field(
          name        = "matches",
          description = s"Matching ${prefix}s up to the return size limit of ${SizeLimitedResult.MaxSize}".some,
          fieldType   = ListType(aType),
          resolve     = _.value.limitedValues
        ),

        Field(
          name        = "hasMore",
          description = "`true` when there were additional matches that were not returned.".some,
          fieldType   = BooleanType,
          resolve     = _.value.hasMore
        )
      )
    )

  def UpdateResultType[A](
    name:     String,
    aType:    OutputType[A],
    typeName: Option[String] = None
  ): ObjectType[Any, SizeLimitedResult[A]] = {
    val tn = typeName.getOrElse(name)

    ObjectType(
      name        = s"Update${name.capitalize}Result",
      description = s"The result of updating the selected $tn, up to `LIMIT` or the maximum of (${SizeLimitedResult.MaxSize}).  If `hasMore` is true, additional $tn were modified and not included here.",

      fieldsFn    = () => List(

        Field(
          name        = s"$tn",
          description = s"The edited $tn, up to the specified LIMIT or the default maximum of ${SizeLimitedResult.MaxSize}.".some,
          fieldType   = ListType(aType),
          resolve     = _.value.limitedValues
        ),

        Field(
          name        = "hasMore",
          description = "`true` when there were additional edits that were not returned.".some,
          fieldType   = BooleanType,
          resolve     = _.value.hasMore
        )
      )
    )
  }

}
