// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package sangria.streaming

import _root_.fs2.Stream
import cats.effect.{Async, IO}
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.tests.CatsSuite
import org.scalatest.Assertion
import sangria.streaming

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Copied and adapted from https://github.com/dwhitney/sangria-fs2/blob/master/src/test/scala/sangria/streaming/FS2IntegrationSpec.scala
 */
final class FS2IntegrationSpec extends CatsSuite {

  def impl(d: Dispatcher[IO]): SubscriptionStream[Stream[IO, *]] =
    streaming.fs2.fs2SubscriptionStream[IO](d, Async[IO])

  test("support itself") {
    Dispatcher[IO].use { d =>
      IO.pure(impl(d).supported(streaming.fs2.fs2SubscriptionStream[IO](d, Async[IO])))
    }.unsafeRunSync() should be (true)
  }

  def execStream[A](testCase: SubscriptionStream[Stream[IO, *]] => Stream[IO, A]): List[A] =
    Dispatcher[IO].use { d => testCase(impl(d)).compile.toList }.unsafeRunSync()

  test("map") {
    execStream(_.map(Stream.emits[IO, Int](List(1, 2, 10)))(_ + 1)) should be (List(2, 3, 11))
  }

  test("singleFuture") {
    execStream(_.singleFuture(Future.successful("foo"))) should be (List("foo"))
  }

  test("single") {
    execStream(_.single("foo")) should be (List("foo"))
  }

  test("mapFuture") {
    execStream(_.mapFuture(Stream.emits[IO, Int](List(1, 2, 10)))(x => Future.successful(x + 1))) should be (List(2, 3, 11))
  }

  def execFuture[A](testCase: SubscriptionStream[Stream[IO, *]] => Future[A]): A =
    Dispatcher[IO].use { d =>
      IO.fromFuture(
        IO.pure(testCase(impl(d)))
      )
    }.unsafeRunSync()

  test("first") {
    execFuture(_.first(Stream.emits[IO, Int](List(1, 2, 3)))) should be (1)
  }

  test("first throws an error on empty") {
    an[NoSuchElementException] should be thrownBy execFuture(_.first(Stream.empty))
  }

  test("failed") {
    an [IllegalStateException] should be thrownBy execStream(_.failed(new IllegalStateException("foo")))
  }

  def execOnComplete(s: Stream[IO, _root_.fs2.INothing]): Assertion = {
    val count = new AtomicInteger(0)
    def inc(): Unit = { val _ = count.getAndIncrement }

    Dispatcher[IO].use { d =>
      val future = impl(d).onComplete(s)(inc()).compile.drain.unsafeToFuture()
      Await.result(future, 2.seconds)
      IO.pure(())
    }.handleErrorWith(_ => IO.unit).unsafeRunSync()

    count.get() should be (1)
  }

  test("onComplete handles success") {
    execOnComplete(Stream.empty)
  }

  test("onComplete handles failure") {
    execOnComplete(Stream.raiseError[IO](new IllegalStateException("foo")))
  }

  test("flatMapFuture") {
    execStream {
      _.flatMapFuture(Future.successful(1))(i => Stream.emits(List(i.toString, (i+1).toString)))
    } should be (List("1", "2"))
  }

  test("recover") {
    val stream = (Stream[IO, Int](1, 2) ++ Stream[IO, Int](3, 4)).map { i =>
      if (i == 3) throw new IllegalStateException("foo")
      else i
    }

    execStream(_.recover(stream)(_ => 100)) should be (List(1, 2, 100))
  }

  test("merge") {
    val stream1 = Stream.emits[IO, Int](List(1, 2))
    val stream2 = Stream.emits[IO, Int](List(3, 4))
    val stream3 = Stream.emits[IO, Int](List(100, 200))

    execStream(_.merge(Vector(stream1, stream2, stream3))).sorted should be (List(1, 2, 3, 4, 100, 200))
  }

  test("merge 2") {
    val stream1 = Stream.emits[IO, Int](List(1, 2))
    val stream2 = Stream.emits[IO, Int](List(100, 200))

    execStream(_.merge(Vector(stream1, stream2))).sorted should be (List(1, 2, 100, 200))
  }

  test("merge 1") {
    val stream = Stream.emits[IO, Int](List(1, 2))

    execStream(_.merge(Vector(stream))) should be (List(1, 2))
  }

  // Original implementation would throw an exception, but it is unclear
  // why this should be an exceptional case.
  //test("merge throws exception on empty") {
  //  an [IllegalStateException] should be thrownBy impl.merge(Vector.empty)
  //}

}
