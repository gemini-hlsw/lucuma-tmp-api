// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.mtl.Stateful

trait SharingState[T, A, B] {

  def link[F[_]](link: (A, B))(implicit S: Stateful[F, T]): F[Unit]

  def unlink[F[_]](link: (A, B))(implicit S: Stateful[F, T]): F[Unit]

  def linkAll[F[_]](links: List[(A, B)])(implicit S: Stateful[F, T]): F[Unit]

  def unlinkAll[F[_]](links: List[(A, B)])(implicit S: Stateful[F, T]): F[Unit]

}