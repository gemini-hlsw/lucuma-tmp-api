// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema
package arb

import cats.Show
import lucuma.odb.api.schema.Paging.Cursor
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbCursor {

  def arbCursor[A: Arbitrary: Show]: Arbitrary[Cursor] =
    Arbitrary {
      Gen.frequency(
        (90, arbitrary[A].map(a => new Cursor(Show[A].show(a)))),
        (10, arbitrary[String].map(s => new Cursor(s)))
      )
    }

  implicit def cogCursor: Cogen[Cursor] =
    Cogen[String].contramap(_.toString)

}

object ArbCursor extends ArbCursor
