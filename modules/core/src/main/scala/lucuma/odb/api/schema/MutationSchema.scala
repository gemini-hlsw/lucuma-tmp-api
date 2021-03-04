// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{InputError, ValidatedInput}
import lucuma.odb.api.repo.OdbRepo
import cats.data.Chain
import cats.effect.Effect
import sangria.schema._

import scala.reflect.ClassTag


object MutationSchema {

  // We could create typed errors like "MissingReference" for each specific
  // case which would allow a script writer to handle the issues more easily.
  // As is, InputErrors just has a list of String messages

  /**
   * A generic input error type with a String "messages" list.
   */
  def InputErrorType[F[_]]: ObjectType[OdbRepo[F], Chain[InputError]] =
    ObjectType(
      name = "InputErrors",
      fieldsFn = () => fields(

        Field(
          name        = "messages",
          fieldType   = ListType(StringType),
          description = Some("Errors in the input to a mutation"),
          resolve     = _.value.map(_.message).toList
        )
      )

    )

  // If we want to add more information to a success payload, it would need
  // to become specific to the individual case. The type created by this method
  // simply has an appropriately named field (e.g., "observation") that
  // contains the object that was created.

  /**
   * A success payload type that simply wraps the created object.
   */
  def SimpleCreateSuccessType[F[_]: Effect, T: ClassTag](
    name:       String,
    resultType: ObjectType[OdbRepo[F], T]
  ): ObjectType[OdbRepo[F], T] =

    ObjectType(
      name = s"Create${name.capitalize}Success",
      fieldsFn = () => fields(
        Field(
          name        = name,
          fieldType   = resultType,
          description = Some(s"The new $name that was successfully created."),
          resolve     = _.value
        )
      )
    )

  /**
   * Create payload type that is either an input error or else a success.
   */
  def SimpleCreatePayloadUnionType[F[_]: Effect, T: ClassTag](
    name:       String,
    resultType: ObjectType[OdbRepo[F], T]
  ): OutputType[ValidatedInput[T]] =

    UnionType(
      name        = s"Create${name.capitalize}Payload",
      description = Some(s"Result of calling create${name.capitalize}"),
      types       = List(SimpleCreateSuccessType[F, T](name, resultType), InputErrorType[F])
    ).mapValue[ValidatedInput[T]](
      _.fold(
        nec => nec: Any,
        suc => suc: Any
      )
    )

}
