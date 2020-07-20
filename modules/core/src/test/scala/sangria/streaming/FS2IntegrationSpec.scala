// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package sangria.streaming

import _root_.fs2.Stream
import cats.effect.{ContextShift, IO}
import cats.tests.CatsSuite
import sangria.streaming

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Copied and adapted from https://github.com/dwhitney/sangria-fs2/blob/master/src/test/scala/sangria/streaming/FS2IntegrationSpec.scala
 */
final class FS2IntegrationSpec extends CatsSuite {

  implicit val cs: ContextShift[IO] =
    IO.contextShift(global)

  val impl: SubscriptionStream[Stream[IO, *]] =
    streaming.fs2.fs2SubscriptionStream[IO]

  test("support itself") {
    impl.supported(streaming.fs2.fs2SubscriptionStream[IO]) should be (true)
  }

  test("map") {
    res(impl.map(Stream.emits[IO, Int](List(1, 2, 10)))(_ + 1)) should be (List(2, 3, 11))
  }

  test("singleFuture") {
    res(impl.singleFuture(Future.successful("foo"))) should be (List("foo"))
  }

  test("single") {
    res(impl.single("foo")) should be (List("foo"))
  }

  test("mapFuture") {
    res(impl.mapFuture(Stream.emits[IO, Int](List(1, 2, 10)))(x => Future.successful(x + 1))) should be (List(2, 3, 11))
  }

  test("first") {
    res(impl.first(Stream.emits[IO, Int](List(1, 2, 3)))) should be (1)
  }

  test("first throws an error on empty") {
    an[NoSuchElementException] should be thrownBy res(impl.first(Stream.empty))
  }

  test("failed") {
    an [IllegalStateException] should be thrownBy res(impl.failed(new IllegalStateException("foo")))
  }

  test("onComplete handles success") {
    val stream  = Stream.empty
    val count   = new AtomicInteger(0)
    def inc(): Unit = { val _ = count.getAndIncrement }

    val updated = impl.onComplete(stream)(inc())

    Await.ready(updated.compile.drain.unsafeToFuture(), 2.seconds)

    count.get() should be (1)
  }

  test("onComplete handles failure") {
    val stream = Stream.raiseError[IO](new IllegalStateException("foo"))
    val count  = new AtomicInteger(0)
    def inc(): Unit = { val _ = count.getAndIncrement }

    val updated = impl.onComplete(stream)(inc())

    Await.ready(updated.compile.drain.unsafeToFuture(), 2.seconds)

    count.get() should be (1)
  }

  test("flatMapFuture") {
    res(impl.flatMapFuture(Future.successful(1))(i => Stream.emits(List(i.toString, (i+1).toString)))) should be (List("1", "2"))
  }

  test("recover") {
    val stream = (Stream[IO, Int](1, 2) ++ Stream[IO, Int](3, 4)).map { i =>
      if (i == 3) throw new IllegalStateException("foo")
      else i
    }

    res(impl.recover(stream)(_ => 100)) should be (List(1, 2, 100))
  }

  test("merge") {
    val stream1 = Stream.emits[IO, Int](List(1, 2))
    val stream2 = Stream.emits[IO, Int](List(3, 4))
    val stream3 = Stream.emits[IO, Int](List(100, 200))

    val result = res(impl.merge(Vector(stream1, stream2, stream3)))
    result.sorted should be (List(1, 2, 3, 4, 100, 200))
  }

  test("merge 2") {
    val stream1 = Stream.emits[IO, Int](List(1, 2))
    val stream2 = Stream.emits[IO, Int](List(100, 200))

    val result = res(impl.merge(Vector(stream1, stream2)))
    result.sorted should be (List(1, 2, 100, 200))
  }

  test("merge 1") {
    val stream = Stream.emits[IO, Int](List(1, 2))

    val result = res(impl.merge(Vector(stream)))
    result should be (List(1, 2))
  }

  // Original implementation would throw an exception, but it is unclear
  // why this should be an exceptional case.
  //test("merge throws exception on empty") {
  //  an [IllegalStateException] should be thrownBy impl.merge(Vector.empty)
  //}

  def res[T](stream: Stream[IO, T]): List[T] =
    res(stream.compile.toList.unsafeToFuture())

  def res[T](f: Future[T]): T =
    Await.result(f, 2.seconds)
}
