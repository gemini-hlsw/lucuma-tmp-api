// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Monad
import cats.data.Nested
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import lucuma.odb.api.model.{DatabaseState, ObservationModel, ProgramModel, ValidatedInput}
import lucuma.odb.api.model.syntax.validatedinput._

trait TargetEnvironmentContext {

  def targetEnvironment: TargetEnvironmentModel

  def observation:       Option[ObservationModel]

  def program:           ProgramModel

}

object TargetEnvironmentContext {

  def fromId[F[_]: Monad, T](
    db: DatabaseState[T],
    id: TargetEnvironment.Id
  )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetEnvironmentContext]] =
    for {
      v <- db.targetEnvironment.lookupValidated(id)
      o <- v.flatTraverse(_.observationId.traverse(oid => Nested(db.observation.lookupValidated(oid))).value)
      p <- v.flatTraverse(v => db.program.lookupValidated(v.programId))
    } yield (v, o, p).mapN { case (vʹ, oʹ, pʹ) =>
      new TargetEnvironmentContext {
        override def targetEnvironment: TargetEnvironmentModel   = vʹ
        override def observation:       Option[ObservationModel] = oʹ
        override def program:           ProgramModel             = pʹ
      }
    }

}
