// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Sharing, TopLevelModel}
import lucuma.odb.api.repo.arb._
import cats.effect.{Concurrent, ContextShift, IO}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import munit.Assertions.assertEquals
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import scala.concurrent.ExecutionContext.global

trait OdbRepoTest {

  implicit val cs: ContextShift[IO] = IO.contextShift(global)

  import ArbTables._

  def makeRepo(t: Tables): IO[OdbRepo[IO]] =
    for {
      log <- Slf4jLogger.create[IO]
      odb <- OdbRepo.fromTables[IO](t)(Concurrent[IO], log)
    } yield odb

  private def subset[A](as: List[A], is: List[Int]): List[A] = {
    val len = as.length
    val inc = if (len == 0) Set.empty[Int] else is.map(i => (i % len).abs).toSet
    as.zipWithIndex.collect { case (a, i) if inc(i) => a }
  }

  def sharingTest[I, A, J, B](
    config: OdbRepo[IO] => (
      TopLevelRepo[IO, I, A],
      TopLevelRepo[IO, J, B],
      Sharing[I, J] => IO[A],
      Sharing[I, J] => IO[A],
      I => IO[List[B]]
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
                initial  <- lookup(ia).map(_.map(TopLevelModel[J, B].id))
                _        <- share(Sharing(ia, some))
                shared   <- lookup(ia).map(_.map(TopLevelModel[J, B].id))
                _        <- unshare(Sharing(ia, some))
                unshared <- lookup(ia).map(_.map(TopLevelModel[J, B].id))
              } yield (initial.toSet, shared.toSet, unshared.toSet, some.toSet)

            }
          } yield tp
        }

      assertEquals(shared,   initial ++ some)
      assertEquals(unshared, initial -- some)

      (shared == initial ++ some) && (unshared == initial -- some)
    }

  def runTest[A](t: Tables)(f: OdbRepo[IO] => IO[A]): A =
    (for {
      o <- makeRepo(t)
      a <- f(o)
    } yield a).unsafeRunSync()

}
