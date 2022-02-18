// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Database, TopLevelModel}
import lucuma.odb.api.model.syntax.toplevel._
import cats.effect.{Async, IO, Sync}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import fs2.Stream

import scala.collection.immutable.SortedMap

trait OdbRepoTest {

  def makeRepo(db: Database): IO[OdbRepo[IO]] =
    OdbRepo.fromDatabase[IO](db)(Async[IO])

  protected def allPages[F[_]: Sync, I, T: TopLevelModel[I, *]](
    pageSize: PosInt,
    afterGid: Option[I],
    repo:     TopLevelRepo[F, I, T]
  ): F[List[ResultPage[T]]] =
    allPagesFiltered[F, I, T](pageSize, afterGid, repo)(Function.const(true))

  protected def allPagesFiltered[F[_]: Sync, I, T: TopLevelModel[I, *]](
    pageSize: PosInt,
    afterGid: Option[I],
    repo:     TopLevelRepo[F, I, T]
  )(
    predicate: T => Boolean
  ): F[List[ResultPage[T]]] =
    Stream.unfoldLoopEval(afterGid) { gid =>
      repo.selectPageFiltered(Some(pageSize.value), gid)(predicate).map { page =>
        val next =
          if (page.hasNextPage)
            page.nodes.lastOption.map(t => Option(TopLevelModel[I, T].id(t)))
          else
            None
        (page, next)
      }
    }.compile.toList

  protected def selectOne[I, T: TopLevelModel[I, *]](
    m: SortedMap[I, T],
    rnd: PosInt,
    includeDeleted: Boolean = false
  ): Option[T] = {
    val v = m.values.filter(t => includeDeleted || t.isPresent).toVector
    if (v.isEmpty) none[T] else v.get((rnd.value % v.size).toLong)
  }

  def attemptAndRunTest[A](db: Database)(f: OdbRepo[IO] => IO[A]): Either[Throwable, A] =
    (for {
      o <- makeRepo(db)
      a <- f(o)
    } yield a).attempt.unsafeRunSync()

  def runTest[A](db: Database)(f: OdbRepo[IO] => IO[A]): A =
    (for {
      o <- makeRepo(db)
      a <- f(o)
    } yield a).unsafeRunSync()

}
