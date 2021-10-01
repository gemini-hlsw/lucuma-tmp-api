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
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.{Observation, Program}
import lucuma.odb.api.model.{CoordinatesModel, DatabaseState, InputError, ValidatedInput}

import scala.collection.immutable.SortedSet

final case class CreateTargetEnvironmentInput(
  targetEnvironmentId: Option[TargetEnvironment.Id],
  explicitBase:        Option[CoordinatesModel.Input],
  science:             Option[List[CreateTargetInput]]
) {

  // TODO: nothing stops you from creating one for an observation where one
  // TODO: already exists

  // TODO: Add a mutation to create an unaffiliated one

  // TODO: Add target environment edit events

  def createUnaffiliated[F[_]: Monad, T](
    db:  DatabaseState[T],
    pid: Program.Id
  )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetEnvironmentModel]] =
    create[F, T](db, pid, None)

  def create[F[_]: Monad, T](
    db:  DatabaseState[T],
    pid: Program.Id,
    oid: Option[Observation.Id],
  )(implicit S: Stateful[F, T]): F[ValidatedInput[TargetEnvironmentModel]] =
    for {
      i <- db.targetEnvironment.getUnusedId(targetEnvironmentId)
      p <- db.program.lookupValidated(pid)
      b  = explicitBase.traverse(_.toCoordinates)
      o <- oid.traverse(o => db.observation.lookupValidated(o)).map(_.sequence)
      t  = (i, p, o, b).mapN { (iʹ, _, _, bʹ) =>
        TargetEnvironmentModel(iʹ, pid, oid, bʹ)
      }
      _ <- db.targetEnvironment.saveNewIfValid(t)(_.id)
      s <- i.fold(
              _ => Monad[F].pure(List.empty[TargetEditResult].validNec[InputError]),
             id => science.toList.flatten.traverse(_.createAll(db, SortedSet(id))).map(_.flatSequence)
           )
    } yield s *> t

}

object CreateTargetEnvironmentInput {

  val Empty: CreateTargetEnvironmentInput =
    CreateTargetEnvironmentInput(None, None, None)

  implicit val DecoderCreate: Decoder[CreateTargetEnvironmentInput] =
    deriveDecoder[CreateTargetEnvironmentInput]

  implicit val EqCreate: Eq[CreateTargetEnvironmentInput] =
    Eq.by { a => (
      a.explicitBase,
      a.science
    )}

  def single(science: CreateTargetInput): CreateTargetEnvironmentInput =
    Empty.copy(science = List(science).some)

  def singleNonsidereal(nonsidereal: CreateNonsiderealInput): CreateTargetEnvironmentInput =
    Empty.copy(science = List(CreateTargetInput.nonsidereal(nonsidereal)).some)

  def singleSidereal(sidereal: CreateSiderealInput): CreateTargetEnvironmentInput =
    Empty.copy(science = List(CreateTargetInput.sidereal(sidereal)).some)

  def fromSidereal(cs: IterableOnce[CreateSiderealInput]): CreateTargetEnvironmentInput =
    Empty.copy(science = cs.iterator.map(CreateTargetInput.sidereal).toList.some)

  def fromNonsidereal(cs: IterableOnce[CreateNonsiderealInput]): CreateTargetEnvironmentInput =
    Empty.copy(science = cs.iterator.map(CreateTargetInput.nonsidereal).toList.some)

}
