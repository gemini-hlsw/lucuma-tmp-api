// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad}
import cats.data.NonEmptySet
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
import lucuma.core.model.{Observation, Program, TargetEnvironment}
import lucuma.odb.api.model.{DatabaseState, InputError, ValidatedInput}

import scala.collection.immutable.SortedSet

final case class SelectTargetEnvironmentInput(
  all:                Option[Program.Id],
  program:            Option[Program.Id],
  observations:       Option[List[Observation.Id]],
  targetEnvironments: Option[List[TargetEnvironment.Id]]
) {

  private val obsList: List[Observation.Id]          = observations.toList.flatten
  private val envList: List[TargetEnvironment.Id]    = targetEnvironments.toList.flatten
  private val allIncludes: Set[Program.Id]           = all.toSet
  private val prgIncludes: Set[Program.Id]           = program.toSet
  private val obsIncludes: Set[Observation.Id]       = obsList.toSet
  private val envIncludes: Set[TargetEnvironment.Id] = envList.toSet

  private def toSelected(sel: ValidatedInput[List[TargetEnvironmentModel]]): ValidatedInput[List[TargetEnvironmentModel]] =
    sel.getOrElse(List.empty[TargetEnvironmentModel]).map(_.programId).distinct match {
      case Nil      => sel
      case _ :: Nil => sel
      case pids     => InputError.fromMessage(s"Multiple program selected: ${pids.mkString(",")}").invalidNec[List[TargetEnvironmentModel]]
    }

  def select[F[_] : Monad, T](
    db: DatabaseState[T]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentModel]]] =

    for {
      a   <- db.program.lookupAllValidated(all.toList)
      p   <- db.program.lookupAllValidated(program.toList)
      os  <- db.observation.lookupAllValidated(obsList)
      vs  <- db.targetEnvironment.lookupAllValidated(envList)
      sel <- db.targetEnvironment.findAll { case (vid, v) =>
        envIncludes(vid) ||                                     // explicitly listed
          v.observationId.exists(obsIncludes) ||                // observation listed
          allIncludes(v.programId) ||                           // all for this program
          (v.observationId.isEmpty && prgIncludes(v.programId)) // associated with program but not an obs
      }
    } yield toSelected((a, p, os, vs).tupled.as(sel))

  def selectIds[F[_]: Monad, T](
    db: DatabaseState[T]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[SortedSet[TargetEnvironment.Id]]] =
    select(db).map(_.map(lst => SortedSet.from(lst.map(_.id))))

}


object SelectTargetEnvironmentInput {

  implicit val DecoderSelectTargetEnvironment: Decoder[SelectTargetEnvironmentInput] =
    deriveDecoder[SelectTargetEnvironmentInput]

  implicit val EqSelectTargetEnvironment: Eq[SelectTargetEnvironmentInput] =
    Eq.by { a => (
      a.all,
      a.program,
      a.observations,
      a.targetEnvironments
    )}

  val Empty: SelectTargetEnvironmentInput =
    SelectTargetEnvironmentInput(None, None, None, None)

  def all(p: Program.Id): SelectTargetEnvironmentInput =
    Empty.copy(all = p.some)

  def program(p: Program.Id): SelectTargetEnvironmentInput =
    Empty.copy(program = p.some)

  def observations(os: List[Observation.Id]): SelectTargetEnvironmentInput =
    Empty.copy(observations = os.some)

  def targetEnvironments(es: List[TargetEnvironment.Id]): SelectTargetEnvironmentInput =
    Empty.copy(targetEnvironments = es.some)

  def invalid(msg: String): ValidatedInput[NonEmptySet[TargetEnvironment.Id]] =
    InputError.fromMessage(s"No target environment was selected: $msg").invalidNec[NonEmptySet[TargetEnvironment.Id]]

  def validateNonEmpty(
    sel:   SortedSet[TargetEnvironment.Id],
    msg:   => String
  ): ValidatedInput[NonEmptySet[TargetEnvironment.Id]] =
    sel
      .headOption
      .fold(invalid(msg)) { h =>
        NonEmptySet(h, sel.tail).validNec[InputError]
      }

  def ids[F[_]: Monad, T](
    db: DatabaseState[T],
    sel: Option[SelectTargetEnvironmentInput]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[SortedSet[TargetEnvironment.Id]]] =
    sel
      .traverse(_.selectIds(db))
      .map(_.sequence.map(_.getOrElse(SortedSet.empty[TargetEnvironment.Id])))

}