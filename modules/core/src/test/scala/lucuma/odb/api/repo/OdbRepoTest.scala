// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{Sharing, TopLevelModel}
import lucuma.odb.api.repo.arb._
import cats.effect.{Concurrent, ContextShift, IO, Sync}
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import fs2.Stream
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
      repo.selectPageFiltered(pageSize.value, gid)(predicate).map { page =>
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

  def shareWithOneUniqueTest[I, A, J, B](
    config: OdbRepo[IO] => (
      TopLevelRepo[IO, I, A],
      TopLevelRepo[IO, J, B],
      (I, J) => IO[A],
      I => IO[Option[B]]
    )
  )(
    implicit modelA: TopLevelModel[I, A], modelB: TopLevelModel[J, B]
  ): Prop =

    forAll { t: Tables =>

      val (oi, oj, shouldError) = runTest(t) { odb =>
        val (repoA, repoB, _, lookup) = config(odb)

        for {
          as <- repoA.selectAll()
          bs <- repoB.selectAll()
          oi = as.headOption.map(modelA.id)
          oj = bs.headOption.map(modelB.id)
          oInitB <- oi.traverse(i => lookup(i)).map(_.flatten)
          // if the current mapping from A -> B is different than the selected B,
          // there will be a constraint violiation.
          shouldError = (oj, oInitB).tupled.exists{case (j, b) => j != modelB.id(b)}
        }  yield (oi, oj, shouldError)
      }

      // if we don't have an a and a b, there is nothing to test
      (oi, oj).tupled.forall{ case (i, j) =>
        val result = attemptAndRunTest(t) { odb =>
          val (_, _, share, lookup) = config(odb)

          for {
            _      <- share(i, j)
            shared <- lookup(i).map(_.map(modelB.id))
          } yield shared
        }

        // returning false fails the test
        result match {
          case Left(_)    => shouldError
          case Right(opt) => opt.contains(j)
        }
      }
    }

  def unshareWithOneUniqueTest[I, A, J, B](
    config: OdbRepo[IO] => (
      TopLevelRepo[IO, I, A],
      TopLevelRepo[IO, J, B],
      (I, J) => IO[A],
      I => IO[Option[B]]
    )
  )(
    implicit modelA: TopLevelModel[I, A], modelB: TopLevelModel[J, B]
  ): Prop =

    forAll { t: Tables =>

      runTest(t) { odb =>
        val (repoA, repoB, unshare, lookup) = config(odb)
        for {
          as <- repoA.selectAll()
          bs <- repoB.selectAll()
          ab = (as.headOption, bs.headOption).tupled

          tp <- ab.fold(IO(true)) { case (a, b) =>

            val i = modelA.id(a)
            val j = modelB.id(b)
            for {
              initial  <- lookup(i).map(_.map(modelB.id))
              _        <- unshare(i, j)
              unshared <- lookup(i).map(_.map(modelB.id))
              passed = unshared.toSet == initial.toSet - j
            } yield passed
          }
        } yield tp
      }
    }

  def shareOneWithManyUniqueTest[I, A, J, B](
    config: OdbRepo[IO] => (
      TopLevelRepo[IO, I, A],
      TopLevelRepo[IO, J, B],
      Sharing[I, J] => IO[A],
      I => IO[ResultPage[B]]
    )
  )(
    implicit modelA: TopLevelModel[I, A], modelB: TopLevelModel[J, B]
  ): Prop =

    forAll { (t: Tables, selectors: List[Int]) =>

      val setupInfo =
        runTest(t) { odb =>
          val (repoA, repoB, _, lookup) = config(odb)
          for {
            as <- repoA.selectAll(true)
            bs <- repoB.selectAll(true)
            tp <- as.headOption.fold(IO(none[(I, List[J], List[J], Boolean)])) { a =>

              val i = modelA.id(a)
              val otherIs = as.map(modelA.id).filter(_ != i)
              val someJs = subset(bs, selectors).map(modelB.id)
              for {
                initial  <- lookup(i).map(_.nodes.map(modelB.id))
                // We will get a constraint violation if any of the Bs we selected
                // are currently mapped to another A
                mappedBs <- otherIs.traverse(lookup(_).map(_.nodes)).map(_.flatten)
                shouldError = mappedBs.map(modelB.id).exists(someJs.contains)
                // allIs = as.map(modelA.id)
                // allJs = bs.map(modelB.id)
                // _ = println(s"selectors: $selectors, \n\tmappedBs: $mappedBs, \n\tallIs: $allIs, \n\tallJs: $allJs, \n\totherIs: $otherIs")
              } yield (i, someJs, initial, shouldError).some

            }
          } yield tp
        }

        setupInfo.forall { case (i, someBs, initial, shouldError) =>
          val result = attemptAndRunTest(t) { odb =>
            val (_, _, share, lookup) = config(odb)

            for {
              _      <- share(Sharing(i, someBs))
              shared <- lookup(i).map(_.nodes.map(modelB.id))
            } yield shared.toSet
          }

          // val links = t.constraintSetObservation.all
          // val allIs = t.constraintSetObservation.allLeft
          // val allJs = t.constraintSetObservation.allRight
          // println(s"selectors: $selectors\n\tshouldError: $shouldError, i: $i, bs: $someBs, \n\tinitial: $initial, \n\tresult: $result, \n\tlinks: $links, \n\ti links: $allIs, \n\tj links: $allJs")
          result match {
            case Left(_)          => shouldError
            case Right(sharedSet) => sharedSet == initial.toSet ++ someBs
          }
        }
    }

  def unshareOneWithManyUniqueTest[I, A, J, B](
    config: OdbRepo[IO] => (
      TopLevelRepo[IO, I, A],
      TopLevelRepo[IO, J, B],
      Sharing[I, J] => IO[A],
      I => IO[ResultPage[B]]
    )
  )(
    implicit modelA: TopLevelModel[I, A], modelB: TopLevelModel[J, B]
  ): Prop =

    forAll { (t: Tables, is: List[Int]) =>

      val (initial, unshared, some) =
        runTest(t) { odb =>
          val (repoA, repoB, unshare, lookup) = config(odb)
          for {
            as <- repoA.selectAll()
            bs <- repoB.selectAll()
            tp <- as.headOption.fold(IO((Set.empty[J], Set.empty[J], Set.empty[J]))) { a =>

              val ia = modelA.id(a)
              val some = subset(bs, is).map(b => modelB.id(b))
              for {
                initial  <- lookup(ia).map(_.nodes.map(modelB.id))
                _        <- unshare(Sharing(ia, some))
                unshared <- lookup(ia).map(_.nodes.map(modelB.id))
              } yield (initial.toSet, unshared.toSet, some.toSet)

            }
          } yield tp
        }

      assertEquals(unshared, initial -- some)

      unshared == initial -- some
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
