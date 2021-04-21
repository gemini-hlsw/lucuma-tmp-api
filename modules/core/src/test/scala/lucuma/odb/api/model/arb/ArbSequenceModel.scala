// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.odb.api.model.SequenceModel._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbSequenceModel extends Helper {

  import ArbAtomModel._

  implicit def arbSequence[D: Arbitrary]: Arbitrary[Sequence[D]] =
    Arbitrary {
      for {
        s  <- smallSize
        as <- Gen.listOfN(s, arbitrary[AtomModel[D]])
      } yield Sequence(as)
    }

  implicit def cogSequence[D: Cogen]: Cogen[Sequence[D]] =
    Cogen[List[AtomModel[D]]].contramap(_.atoms)

  implicit def arbSequenceCreate[D: Arbitrary]: Arbitrary[Sequence.Create[D]] =
    Arbitrary {
      for {
        s  <- smallSize
        as <- Gen.listOfN(s, arbitrary[AtomModel.Create[D]])
      } yield Sequence.Create(as)
    }

  implicit def cogSequenceCreate[D: Cogen]: Cogen[Sequence.Create[D]] =
    Cogen[List[AtomModel.Create[D]]].contramap(_.atoms)


}

object ArbSequenceModel extends ArbSequenceModel
