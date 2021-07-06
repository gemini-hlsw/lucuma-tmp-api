// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.tests.CatsSuite
import lucuma.odb.api.model.arb.ArbManyToMany
import lucuma.odb.api.repo.ManyToMany

final class ManyToManySpec extends CatsSuite {

  type A = Short
  type B = Byte

  import ArbManyToMany._

  test("two way linking") {
    forAll { m: ManyToMany[A, B] =>
      assert(m.all.forall { case (a, b) =>
        m.selectRight(a).contains(b) && m.selectLeft(b).contains(a)
      })
    }
  }

  test("+ link") {
    forAll { (m: ManyToMany[A, B], link: (A, B)) =>
      assert((m + link).contains(link))
    }
  }

  test("++ links") {
    forAll { (m: ManyToMany[A, B], links: List[(A, B)]) =>
      ((m ++ links).all & links.toSet) shouldEqual links.toSet
    }
  }

  test("- link") {
    forAll { m: ManyToMany[A, B] =>
      assert(m.all.headOption.forall { link =>
        val mʹ = m - link
        m.contains(link) && !mʹ.contains(link) && !mʹ.all.contains(link)
      })
    }
  }

  test("-- links") {
    forAll { (m: ManyToMany[A, B], i: Short) =>
      val links = m.all.take(Math.abs(i % 4))
      val mʹ    = m -- links
      (m.all &~ mʹ.all) shouldEqual links
    }
  }

  test("contains") {
    forAll { m: ManyToMany[A, B] =>
      assert(m.all.forall(m.contains))
    }
  }

  test("removeLeft") {
    forAll { m: ManyToMany[A, B] =>
      assert(m.allLeft.headOption.forall { a =>
        m.removeLeft(a).all.forall(_._1 != a)
      })
    }
  }

  test("removeRight") {
    forAll { m: ManyToMany[A, B] =>
      assert(m.allRight.headOption.forall { b =>
        m.removeRight(b).all.forall(_._2 != b)
      })
    }
  }

  test("allLeft") {
    forAll { m: ManyToMany[A, B] =>
      m.allLeft shouldEqual m.all.map(_._1)
    }
  }

  test("allRight") {
    forAll { m: ManyToMany[A, B] =>
      m.allRight shouldEqual m.all.map(_._2)
    }
  }

  test("selectLeft") {
    forAll { m: ManyToMany[A, B] =>
      assert(m.all.headOption.map(_._2).forall { b =>
        m.selectLeft(b) == m.all.collect { case (a0, b0) if b == b0 => a0 }
      })
    }
  }

  test("selectRight") {
    forAll { m: ManyToMany[A, B] =>
      assert(m.all.headOption.map(_._1).forall { a =>
        m.selectRight(a) == m.all.collect { case (a0, b0) if a == a0 => b0 }
      })
    }
  }

  test("size equals all links size") {
    forAll { m: ManyToMany[A, B] =>
      m.size shouldEqual m.all.size
    }
  }

  test("isEmpty") {
    forAll { m: ManyToMany[A, B] =>
      m.isEmpty shouldEqual (m.size == 0)
    }
  }

  test("nonEmpty") {
    forAll { m: ManyToMany[A, B] =>
      m.nonEmpty shouldEqual (m.size > 0)
    }
  }
}
