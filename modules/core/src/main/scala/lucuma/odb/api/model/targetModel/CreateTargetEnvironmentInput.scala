// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad}
import cats.implicits.catsKernelOrderingForOrder
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.{Observation, Program}
import lucuma.odb.api.model.{CoordinatesModel, DatabaseState, ValidatedInput}


import scala.collection.immutable.SortedSet

final case class CreateTargetEnvironmentInput(
  explicitBase: Option[CoordinatesModel.Input],
  science:      Option[List[CreateTargetInput]]
) {

  // TODO: nothing stops you from creating one for an observation where one
  // TODO: already exists

  def create[F[_]: Monad, T](
    db:  DatabaseState[T],
    pid: Program.Id,
    oid: Option[Observation.Id],
  )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetEnvironmentModel]] =
    for {
      i <- db.targetEnvironment.cycleNextUnused
      p <- db.program.lookupValidated(pid)
      b  = explicitBase.traverse(_.toCoordinates)
      o <- oid.traverse(o => db.observation.lookupValidated(o)).map(_.sequence)
      t  = (p, o, b).mapN { (_, _, bʹ) =>
        TargetEnvironmentModel(i, pid, oid, bʹ)
      }
      _ <- db.targetEnvironment.saveNewIfValid(t)(_.id)
      s <- science.toList.flatten.traverse(_.createAll(db, SortedSet(i))).map(_.flatSequence)
    } yield s *> t

}

object CreateTargetEnvironmentInput {

  implicit val DecoderCreate: Decoder[CreateTargetEnvironmentInput] =
    deriveDecoder[CreateTargetEnvironmentInput]

  implicit val EqCreate: Eq[CreateTargetEnvironmentInput] =
    Eq.by { a => (
      a.explicitBase,
      a.science
    )}

  def single(science: CreateTargetInput): CreateTargetEnvironmentInput =
    CreateTargetEnvironmentInput(None, List(science).some)

  def singleNonsidereal(nonsidereal: CreateNonsiderealInput): CreateTargetEnvironmentInput =
    CreateTargetEnvironmentInput(None, List(CreateTargetInput.nonsidereal(nonsidereal)).some)

  def singleSidereal(sidereal: CreateSiderealInput): CreateTargetEnvironmentInput =
    CreateTargetEnvironmentInput(None, List(CreateTargetInput.sidereal(sidereal)).some)

  def fromSidereal(cs: IterableOnce[CreateSiderealInput]): CreateTargetEnvironmentInput =
    CreateTargetEnvironmentInput(None, cs.iterator.map(CreateTargetInput.sidereal).toList.some)

  def fromNonsidereal(cs: IterableOnce[CreateNonsiderealInput]): CreateTargetEnvironmentInput =
    CreateTargetEnvironmentInput(None, cs.iterator.map(CreateTargetInput.nonsidereal).toList.some)

}
