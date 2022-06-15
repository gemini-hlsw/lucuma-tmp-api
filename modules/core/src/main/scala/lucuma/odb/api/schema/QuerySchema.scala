// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegInt
import io.circe.refined._
import lucuma.odb.api.model.query.{SelectResult, WhereOptionString, WhereOrder, WhereString}
import sangria.marshalling.circe._
import sangria.macros.derive.{DocumentInputField, InputObjectTypeDescription, InputObjectTypeName, deriveInputObjectType}
import sangria.schema._

object QuerySchema {

  import RefinedSchema._

  val DefaultLimit: NonNegInt =
    1000

  val ArgumentOptionLimit: Argument[Option[NonNegInt]] =
    Argument(
      name         = "LIMIT",
      argumentType =  OptionInputType(NonNegIntType.copy(description = "foo".some)),
      description  = s"Limits the result to at most this number of matches (but never more than $DefaultLimit)."
    )

  implicit val InputObjectWhereString: InputObjectType[WhereString] =
    deriveInputObjectType[WhereString](
      InputObjectTypeName("WhereString"),
      InputObjectTypeDescription("String matching options."),
      DocumentInputField("LIKE",       document.like),
      DocumentInputField("NLIKE",      document.nlike),
      DocumentInputField("MATCH_CASE", document.matchCase)
    )

  implicit val InputObjectWhereOptionString: InputObjectType[WhereOptionString] =
    deriveInputObjectType[WhereOptionString](
      InputObjectTypeName("WhereOptionString"),
      InputObjectTypeDescription("String matching options."),
      DocumentInputField("IS_NULL",    document.isNullField("string")),
      DocumentInputField("LIKE",       document.like),
      DocumentInputField("NLIKE",      document.nlike),
      DocumentInputField("MATCH_CASE", document.matchCase)
    )

  implicit val InputObjectWhereOrderInt: InputObjectType[WhereOrder[Int]] =
    deriveInputObjectType[WhereOrder[Int]](
      InputObjectTypeName("WhereOrderInt"),
      InputObjectTypeDescription("Integer matching options")
    )

  object document {
    def andField(n: String): String =
      s"A list of nested $n filters that all must match in order for the AND group as a whole to match."

    def orField(n: String): String =
      s"A list of nested $n filters where any one match causes the entire OR group as a whole to match."

    def notField(n: String): String =
      s"A nested $n filter that must not match in order for the NOT itself to match."

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
  ): ObjectType[Any, SelectResult[A]] =
    ObjectType(
      name        = s"${prefix.capitalize}SelectResult",
      description = s"The matching $prefix results, limited to a maximum of $DefaultLimit entries.",

      fieldsFn    = () => List(

        Field(
          name        = "matches",
          description = s"Matching ${prefix}s up to the return size limit of $DefaultLimit".some,
          fieldType   = ListType(aType),
          resolve     = _.value.matches
        ),

        Field(
          name        = "hasMore",
          description = "`true` when there were additional matches that were not returned.".some,
          fieldType   = BooleanType,
          resolve     = _.value.hasMore
        )
      )
    )
}
