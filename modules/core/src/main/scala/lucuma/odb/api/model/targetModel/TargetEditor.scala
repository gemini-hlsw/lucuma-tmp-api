// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Monad
import cats.data.{Nested, State}
import cats.mtl.Stateful
import lucuma.odb.api.model.InputError
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.validated._
import lucuma.core.model.Target
import lucuma.odb.api.model.{DatabaseState, ValidatedInput}

import scala.collection.immutable.SortedSet

/**
 * Trait shared by single target edit inputs.
 */
trait TargetEditor {

  def select: SelectTargetInput

  def editor: ValidatedInput[State[Target, Unit]]

  private def checkTargetEnvironmentSelection(
    vs:  SortedSet[TargetEnvironment.Id]
  ): ValidatedInput[Unit] =
    if (select.names.exists(_.nonEmpty))
      SelectTargetEnvironmentInput.validateNonEmpty(vs, "specify a target environment when identifying targets by name.").void
    else
      ().validNec[InputError]

  private def editTargets[F[_]: Monad, T](
    db: DatabaseState[T],
    vs: SortedSet[TargetEnvironment.Id]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetModel]]] =
    for {
      v   <- vs.toList.traverse(v => db.targetEnvironment.lookupValidated[F](v)).map(_.sequence)
      tms <- db.target.findAll { case (_, tm) => select.matches(vs, tm) }
      tmsʹ = (v, checkTargetEnvironmentSelection(vs), editor).mapN { (_, _, e) =>
        tms.map(TargetModel.target.modify(t => e.runS(t).value))
      }
      _ <- tmsʹ.traverse(_.traverse(tm => db.target.update(tm.id, tm)))
    } yield tmsʹ

  def edit[F[_]: Monad, T](
    db: DatabaseState[T],
    vs: SortedSet[TargetEnvironment.Id]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEditResult]]] =
    Nested(editTargets(db, vs)).map(_.map(TargetEditResult.edit)).value

}