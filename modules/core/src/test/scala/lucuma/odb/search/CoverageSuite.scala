// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search

import cats.syntax.all._
import lucuma.core.math.Wavelength
import lucuma.odb.search.Coverage
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._

class CoverageSuite extends munit.DisciplineSuite {

  implicit def arbWavelength: Arbitrary[Wavelength] =
    Arbitrary {
      arbitrary[Int].map(_.abs).map(Wavelength.fromPicometers.getOption).flatMap {
        case Some(w) => Gen.const(w)
        case None    => Gen.fail
      }
    }

  implicit def arbCoverage: Arbitrary[Coverage] =
    Arbitrary {
      for {
        a <- arbitrary[Wavelength]
        b <- arbitrary[Wavelength]
      } yield Coverage(a, b)
    }

  test("construction.invariant") {
    forAll { (a: Wavelength, b: Wavelength) =>
      assertEquals(Coverage(a, b).range.isDefined, a < b)
    }
  }

  test("intersection.identity") {
    forAll { (a: Coverage) =>
      assertEquals((a ⋂ a), a)
    }
  }

  test("intersection.annihilation.right") {
    forAll { (a: Coverage) =>
      assertEquals((a ⋂ Coverage.Empty), Coverage.Empty)
    }
  }

  test("intersection.annihilation.left") {
    forAll { (a: Coverage) =>
      assertEquals((Coverage.Empty ⋂ a), Coverage.Empty)
    }
  }

  test("intersection.reduction") {
    forAll { (a: Coverage, b: Coverage) =>
      assert((a ⋂ b).width <= a.width)
      assert((a ⋂ b).width <= b.width)
    }
  }

}
