// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbSequenceModel extends Helper {

  import ArbAtomModel._

  implicit def arbSequence[D: Arbitrary]: Arbitrary[DereferencedSequence[D]] =
    Arbitrary {
      for {
        s  <- smallSize
        as <- Gen.listOfN(s, arbitrary[AtomModel[StepModel[D]]])
      } yield SequenceModel(as)
    }

  implicit def cogSequence[D: Cogen]: Cogen[DereferencedSequence[D]] =
    Cogen[List[AtomModel[StepModel[D]]]].contramap(_.atoms)

  implicit def arbSequenceCreate[D: Arbitrary]: Arbitrary[SequenceModel.Create[D]] =
    Arbitrary {
      for {
        s  <- smallSize
        as <- Gen.listOfN(s, arbitrary[AtomModel.Create[D]])
      } yield SequenceModel.Create(as)
    }

  def arbValidSequenceCreate[D: Arbitrary]: Arbitrary[SequenceModel.Create[D]] =
    Arbitrary {
      for {
        s  <- smallSize
        as <- Gen.listOfN(s, arbValidCreateAtom[D].arbitrary)
      } yield SequenceModel.Create(as)
    }

  implicit def cogSequenceCreate[D: Cogen]: Cogen[SequenceModel.Create[D]] =
    Cogen[List[AtomModel.Create[D]]].contramap(_.atoms)


}

object ArbSequenceModel extends ArbSequenceModel
