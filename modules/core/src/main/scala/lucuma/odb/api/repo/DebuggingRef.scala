// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.data.State
import cats.effect.{Ref, Sync}
import cats.syntax.all._
import org.typelevel.log4cats.Logger

final case class DebuggingRef[F[_]: Logger: Sync, A](ref: Ref[F, A]) extends Ref[F, A] {

  import DebuggingRef._

  private def log(s: String): F[Unit] =
    Logger[F].trace(s)

  private def logAndDelegate[T](n: String, f: F[T]): F[T] =
    for {
      _ <- log(s"$n - start")
      t <- f
      _ <- log(trimMessage(s"$n - done: $t"))
    } yield t

  override def get: F[A] =
    logAndDelegate("get", ref.get)

  override def set(a: A): F[Unit] =
    logAndDelegate("set", ref.set(a))

  override def access: F[(A, A => F[Boolean])] =
    logAndDelegate("access", ref.access)

  override def tryUpdate(f: A => A): F[Boolean] =
    logAndDelegate("tryUpdate", ref.tryUpdate(f))

  override def update(f: A => A): F[Unit] = {
    def spin(i: Int): F[Unit] =
      if (i <= 0) {
        val ex = new RuntimeException(s"update failed")
        Logger[F].info(ex)(s"update failure") *> Sync[F].raiseError[Unit](ex)
      } else
        ref.tryUpdate(f).flatMap { res =>
          if (res) log(s"update - done (attempt ${SpinLimit + 1 - i}") *> Sync[F].unit
          else spin(i - 1)
        }

    log(s"update - start") *> spin(SpinLimit)
  }

  private def spinModify[B](name: String, fob: F[Option[B]]): F[B] = {
    def spin(i: Int): F[B] =
      if (i <= 0) {
        val ex = new RuntimeException(s"$name failed")
        Logger[F].info(ex)(s"$name failure") *> Sync[F].raiseError[B](ex)
      } else
        fob.flatMap {
          case None    => spin(i - 1)
          case Some(b) => log(s"$name - done (attempt ${SpinLimit + 1 - i}) $b") *> Sync[F].pure(b)
        }

    log(s"$name - start") *> spin(SpinLimit)
  }

  override def tryModify[B](f: A => (A, B)): F[Option[B]] =
    logAndDelegate("tryModify", ref.tryModify(f))

  override def modify[B](f: A => (A, B)): F[B] =
    spinModify("modify", ref.tryModify(f))

  override def tryModifyState[B](state: State[A, B]): F[Option[B]] =
    logAndDelegate("tryModifyState", ref.tryModifyState(state))

  override def modifyState[B](state: State[A, B]): F[B] =
    spinModify("modifyState", ref.tryModifyState(state))

}

object DebuggingRef {

  val SpinLimit: Int    =  10
  val MessageLimit: Int = 250

  def trimMessage(s: String): String =
    if (s.length <= MessageLimit) s
    else s"${s.take(MessageLimit)} ..."

}
