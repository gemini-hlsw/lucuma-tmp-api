// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.core.model.Atom
import lucuma.odb.api.model.AtomModel

import cats.Functor
import cats.effect.Ref
import cats.syntax.all._

sealed trait AtomRepo[F[_]] {

  def selectAtom(
    aid: Atom.Id
  ): F[Option[AtomModel[_]]]

  def unsafeSelectAtom(
    aid: Atom.Id
  ): F[AtomModel[_]]  // or AtomModel[StepModel[_]] ???

}

object AtomRepo {

  def create[F[_]: Functor](
    tablesRef: Ref[F, Tables]
  ): AtomRepo[F] =

    new AtomRepo[F] {
      override def selectAtom(
        aid: Atom.Id
      ): F[Option[AtomModel[_]]] =
        tablesRef.get.map(Tables.atom(aid).get)

      override def unsafeSelectAtom(
        aid: Atom.Id
      ): F[AtomModel[_]] =
        selectAtom(aid).map(_.getOrElse(sys.error(s"Atom id '$aid' missing'")))

    }

}