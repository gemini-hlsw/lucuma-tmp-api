// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import org.scalacheck._

trait SplitSetHelper {

  /**
   * Generates a list of n unique sets from a set.
   */
  def splitSetIntoN[A](n: Int, as: Set[A]): Gen[List[Seq[A]]] = {
    // not stack-safe...
    def loop(rem: Int, set: Set[A], acc: List[Seq[A]]): Gen[List[Seq[A]]] =
      if (rem == 0) Gen.const(acc)
      else
        for {
          someAs <- Gen.someOf(set)
          result <- loop(rem - 1, set -- someAs, someAs.toSeq :: acc)
        } yield result

    loop(n, as, List.empty[Seq[A]])

  }
}
