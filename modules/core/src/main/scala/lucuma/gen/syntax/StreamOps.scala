// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.syntax

import cats.Functor
import cats.syntax.functor._
import fs2.Stream


final class StreamOps[F[_], O](s: Stream[F, O]) {

      // Takes from the Stream while the provided stop condition evaluates `true`.
    def evalTakeWhile(f: F[Boolean]): Stream[F, O] =
      s zipLeft Stream.repeatEval(f).takeWhile(identity)

    // Takes from the Stream while the provided stop condition evaluates `false`.
    def evalTakeWhileNot(f: F[Boolean])(implicit ev: Functor[F]): Stream[F, O] =
      evalTakeWhile(f.map(b => !b))

}

trait ToStreamOps {
  implicit def toStreamOps[F[_], O](s: Stream[F, O]): StreamOps[F, O] =
    new StreamOps(s)
}

object stream extends ToStreamOps