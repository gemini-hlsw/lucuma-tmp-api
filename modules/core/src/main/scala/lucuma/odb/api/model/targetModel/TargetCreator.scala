// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Monad
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Target
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}
import lucuma.odb.api.model.syntax.validatedinput._

import scala.collection.immutable.SortedSet

trait TargetCreator {
    def name: NonEmptyString

    def toGemTarget: ValidatedInput[Target]

    def create[F[_]: Monad, T](
      db:  DatabaseState[T],
      vid: TargetEnvironment.Id
    )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =
      for {
        i <- db.target.cycleNextUnused
        v <- db.targetEnvironment.lookupValidated(vid)
        tm = (v, toGemTarget).mapN { (_, g) => TargetModel(i, vid, g) }
        _ <- db.target.saveNewIfValid(tm)(_.id)
      } yield tm

    def createAll[F[_]: Monad, T](
      db: DatabaseState[T],
      vs: SortedSet[TargetEnvironment.Id]
    )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEditResult]]] =
      SelectTargetEnvironmentInput
        .validateNonEmpty(vs, "cannot create targets without specifying a target environment")
        .flatTraverse {
          _.toNonEmptyList
           .traverse(create(db, _))
           .map(_.sequence.map(_.map(TargetEditResult.create).toList))
        }
  }
