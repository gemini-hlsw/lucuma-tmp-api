// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.syntax

import cats.data.{State, StateT}
import cats.syntax.either._
import cats.syntax.functor._
import lucuma.odb.api.model.{Database, EitherInput, InputError, ValidatedInput}

final class DatabaseStateOps[A](self: StateT[EitherInput, Database, A]) {

  def flipF: State[Database, EitherInput[A]] =
    State.apply[Database, EitherInput[A]] { db =>
      self.run(db).fold(
        err => (db, err.asLeft[A]),
        _.map(_.rightNec[InputError])
      )
    }

  def flipFValidated: State[Database, ValidatedInput[A]] =
    flipF.map(_.toValidated)

}

trait ToDatabaseStateOps {
  implicit def toDatabaseStateOps[A](s: StateT[EitherInput, Database, A]): DatabaseStateOps[A] =
    new DatabaseStateOps[A](s)
}

object databasestate extends ToDatabaseStateOps

