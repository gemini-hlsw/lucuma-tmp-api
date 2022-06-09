// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.NonNegInt
import io.circe.refined._
import lucuma.odb.api.model.query.{SelectResult, WhereOptionString, WhereString}
import sangria.marshalling.circe._
import sangria.macros.derive.{InputObjectTypeName, deriveInputObjectType}
import sangria.schema._

object QuerySchema {

  import RefinedSchema._

  val DefaultLimit: NonNegInt =
    1000

  val ArgumentLimit: Argument[Option[NonNegInt]] =
    Argument(
      name         = "limit",
      argumentType =  OptionInputType(NonNegIntType.copy(description = "foo".some)),
      description  = s"Limits the result to at most this number of matches (but never more than $DefaultLimit)."
    )

  implicit val InputObjectWhereString: InputObjectType[WhereString] =
    deriveInputObjectType[WhereString](
      InputObjectTypeName("WhereString")
    )

  implicit val InputObjectWhereOptionString: InputObjectType[WhereOptionString] =
    deriveInputObjectType[WhereOptionString](
      InputObjectTypeName("WhereOptionString")
    )

  def SelectResultType[A](
    name:  String,
    aType: OutputType[A]
  ): ObjectType[Any, SelectResult[A]] =
    ObjectType(
      name        = name,
      description = "Selection results",

      fieldsFn    = () => List(

        Field(
          name        = "matches",
          description = s"Matching objects up to the return size limit of $DefaultLimit".some,
          fieldType   = ListType(aType),
          resolve     = _.value.matches
        ),

        Field(
          name        = "totalCount",
          description = "Total match count, including any those that are past the return size cutoff limit.".some,
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
