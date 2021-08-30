// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad}
import cats.mtl.Stateful
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}

import scala.collection.immutable.SortedSet

/**
 * Input required to create either a non-sidereal or sidereal target.
 */
final case class CreateTargetInput(
  nonsidereal: Option[CreateNonsiderealInput],
  sidereal:    Option[CreateSiderealInput]
) {

  def create[F[_]: Monad, T](
    db:  DatabaseState[T],
    vid: TargetEnvironment.Id
  )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =
    ValidatedInput.requireOneF(
      "create",
      nonsidereal.map(_.create(db, vid)),
      sidereal.map(_.create(db, vid))
    )

  def createAll[F[_]: Monad, T](
    db: DatabaseState[T],
    vs: SortedSet[TargetEnvironment.Id]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEditResult]]] =
    ValidatedInput.requireOneF(
      "create",
      nonsidereal.map(_.createAll(db, vs)),
      sidereal.map(_.createAll(db, vs))
    )

}

object CreateTargetInput {

  implicit val DecoderCreateTargetInput: Decoder[CreateTargetInput] =
    deriveDecoder[CreateTargetInput]

  implicit val EqCreateTargetInput: Eq[CreateTargetInput] =
    Eq.by { a => (
      a.nonsidereal,
      a.sidereal
    )}

  def nonsidereal(n: CreateNonsiderealInput): CreateTargetInput =
    CreateTargetInput(n.some, None)

  def sidereal(s: CreateSiderealInput): CreateTargetInput =
    CreateTargetInput(None, s.some)

}

