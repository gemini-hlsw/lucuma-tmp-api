// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.arb

import cats.Order
import lucuma.odb.api.repo.ManyToMany
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck._

/**
 *
 */
trait ArbManyToMany {

  implicit def arbManyToMany[A: Order, B: Order](
    implicit a: Arbitrary[A], b: Arbitrary[B]
  ): Arbitrary[ManyToMany[A, B]] =
    Arbitrary {
      for {
        s  <- sized(s => choose(0, s max 0))
        ls <- listOfN(s, arbitrary[(A, B)])
      } yield ManyToMany.empty[A, B] ++ ls
    }

}

object ArbManyToMany extends ArbManyToMany
