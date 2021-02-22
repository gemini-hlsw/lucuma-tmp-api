// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import cats.Order
import lucuma.odb.api.repo.OneToManyUnique
import org.scalacheck._

trait ArbOneToManyUnique extends SplitSetHelper {

  implicit def arbOneToManyUnique[A: Order, B: Order](
    implicit a: Arbitrary[A],
    b:          Arbitrary[B]
  ): Arbitrary[OneToManyUnique[A, B]] =
    Arbitrary {
      for {
        as    <- Gen.containerOf[Set, A](a.arbitrary)
        bs    <- Gen.containerOf[Set, B](b.arbitrary)
        bList <- splitSetIntoN(as.size, bs)
      } yield {
        val tuples = as.zip(bList).foldLeft(List.empty[(A, B)]) { (acc, tuple) =>
          acc ++ tuple._2.map(b => (tuple._1, b))
        }
        OneToManyUnique(tuples: _*).toOption.get
      }
    }
}

object ArbOneToManyUnique extends ArbOneToManyUnique
