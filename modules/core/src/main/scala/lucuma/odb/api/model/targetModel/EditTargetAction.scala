// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Monad
import cats.data.Nested
import cats.mtl.Stateful
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}

import scala.collection.immutable.SortedSet


/**
 * Trait shared by inputs that offer target edit options.
 */
trait EditTargetAction {

  def addSidereal:     Option[CreateSiderealInput]

  def addNonsidereal:  Option[CreateNonsiderealInput]

  def editSidereal:    Option[EditSiderealInput]

  def editNonsidereal: Option[EditNonsiderealInput]

  def delete:          Option[SelectTargetInput]

  protected def doDeletion[F[_]: Monad, T](
    db:  DatabaseState[T],
    vs:  SortedSet[TargetEnvironment.Id],
    del: SelectTargetInput
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEditResult]]] =
    for {
      v   <- vs.toList.traverse(v => db.targetEnvironment.lookupValidated[F](v)).map(_.sequence)
      tms <- db.target.findAll { case (_, tm) => del.matches(vs, tm) }
      res <- v.as(tms.traverse(tm => db.target.delete(tm.id)).as(tms)).sequence
    } yield Nested(res).map(TargetEditResult.delete).value

  def editEnv[F[_]: Monad, T](
    db: DatabaseState[T],
    vs: SortedSet[TargetEnvironment.Id]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEditResult]]] =
    ValidatedInput.requireOneF("edit",
      addSidereal.map(_.createAll[F, T](db, vs)),
      addNonsidereal.map(_.createAll[F, T](db, vs)),
      editSidereal.map(_.edit[F, T](db, vs)),
      delete.map(doDeletion[F, T](db, vs, _))
    )
}

