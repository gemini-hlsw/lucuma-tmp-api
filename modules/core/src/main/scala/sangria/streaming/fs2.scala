// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package sangria.streaming

import _root_.fs2.Stream
import cats.effect.{ConcurrentEffect, ContextShift, IO}

import scala.concurrent.{ExecutionContext, Future}

/**
 * An FS2-based SubscriptionStream.  This is adapted from the original, now
 * out of date: https://github.com/dwhitney/sangria-fs2/blob/master/src/main/scala/sangria/streaming/fs2.scala
 */
object fs2 {

  class FS2SubscriptionStream[F[_]](
    implicit executionContext: ExecutionContext, C: ConcurrentEffect[F]
  ) extends SubscriptionStream[Stream[F, *]] {

    implicit val contextShift: ContextShift[IO] =
      IO.contextShift(executionContext)

    override def supported[T[_]](other: SubscriptionStream[T]): Boolean =
      other.isInstanceOf[FS2SubscriptionStream[F]]

    override def single[T](value: T): Stream[F, T] =
      Stream.emit(value)

    override def singleFuture[T](value: Future[T]): Stream[F, T] =
      Stream.eval[F, T](C.liftIO(IO.fromFuture(IO.pure(value))))

    override def first[T](s: Stream[F, T]): Future[T] =
      C.toIO(s.head.compile.lastOrError).attempt.unsafeToFuture().flatMap {
        case Left(e)  => Future.failed(e)
        case Right(t) => Future.successful(t)
      }

    override def failed[T](e: Throwable): Stream[F, T] =
      Stream.raiseError[F](e)

    override def onComplete[Ctx, Res](result: Stream[F, Res])(op: => Unit): Stream[F, Res] =
      result.onFinalize(C.delay(op))

    override def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Stream[F, Res]): Stream[F, Res] =
      Stream.eval(C.liftIO(IO.fromFuture(IO.pure(future)))).flatMap(resultFn)

    override def mapFuture[A, B](source: Stream[F, A])(fn: A => Future[B]): Stream[F, B] =
      source.evalMap(a => C.liftIO(IO.fromFuture(IO(fn(a)))))

    override def map[A, B](source: Stream[F, A])(fn: A => B): Stream[F, B] =
      source.map(fn)

    override def merge[T](streams: Vector[Stream[F, T]]): Stream[F, T] =
      streams.foldLeft(Stream.empty.covaryAll[F, T])(_.merge(_))

    override def recover[T](stream: Stream[F, T])(fn: Throwable => T): Stream[F, T] =
      stream.handleErrorWith(t => Stream.emit(fn(t)))

  }

  implicit def fs2SubscriptionStream[F[_]: ConcurrentEffect](implicit executionContext: ExecutionContext): SubscriptionStream[Stream[F, *]] =
    new FS2SubscriptionStream[F]()

}
