// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Sharing, TopLevelModel}
import lucuma.odb.api.model.syntax.toplevel._
import lucuma.odb.api.repo.arb._

import cats.effect.{Async, IO, Sync}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import fs2.Stream
import munit.Assertions.assertEquals
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import scala.collection.immutable.SortedMap

trait OdbRepoTest {

  import ArbTables._

  def makeRepo(t: Tables): IO[OdbRepo[IO]] =
    OdbRepo.fromTables[IO](t)(Async[IO])

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

  private def subset[A](as: List[A], is: List[Int]): List[A] = {
    val len = as.length
    val inc = if (len == 0) Set.empty[Int] else is.map(i => (i % len).abs).toSet
    as.zipWithIndex.collect { case (a, i) if inc(i) => a }
  }

  protected def selectOne[I, T: TopLevelModel[I, *]](
    m: SortedMap[I, T],
    rnd: PosInt,
    includeDeleted: Boolean = false
  ): Option[T] = {
    val v = m.values.filter(t => includeDeleted || t.isPresent).toVector
    if (v.isEmpty) none[T] else v.get((rnd.value % v.size).toLong)
  }

  def sharingTest[I, A, J, B](
    config: OdbRepo[IO] => (
      TopLevelRepo[IO, I, A],
      TopLevelRepo[IO, J, B],
      Sharing[I, J] => IO[A],
      Sharing[I, J] => IO[A],
      I => IO[ResultPage[B]]
    )
  )(
    implicit ev0: TopLevelModel[I, A], ev1: TopLevelModel[J, B]
  ): Prop =

    forAll { (t: Tables, is: List[Int]) =>

      val (initial, shared, unshared, some) =
        runTest(t) { odb =>
          val (repoA, repoB, share, unshare, lookup) = config(odb)
          for {
            as <- repoA.selectAll()
            bs <- repoB.selectAll()
            tp <- as.headOption.fold(IO((Set.empty[J], Set.empty[J], Set.empty[J], Set.empty[J]))) { a =>

              val ia = TopLevelModel[I, A].id(a)
              val some = subset(bs, is).map(b => TopLevelModel[J, B].id(b))
              for {
                initial  <- lookup(ia).map(_.nodes.map(TopLevelModel[J, B].id))
                _        <- share(Sharing(ia, some))
                shared   <- lookup(ia).map(_.nodes.map(TopLevelModel[J, B].id))
                _        <- unshare(Sharing(ia, some))
                unshared <- lookup(ia).map(_.nodes.map(TopLevelModel[J, B].id))
              } yield (initial.toSet, shared.toSet, unshared.toSet, some.toSet)

            }
          } yield tp
        }

      assertEquals(shared,   initial ++ some)
      assertEquals(unshared, initial -- some)

      (shared == initial ++ some) && (unshared == initial -- some)
    }

  def attemptAndRunTest[A](t: Tables)(f: OdbRepo[IO] => IO[A]): Either[Throwable, A] =
    (for {
      o <- makeRepo(t)
      a <- f(o)
    } yield a).attempt.unsafeRunSync()

  def runTest[A](t: Tables)(f: OdbRepo[IO] => IO[A]): A =
    (for {
      o <- makeRepo(t)
      a <- f(o)
    } yield a).unsafeRunSync()

}
