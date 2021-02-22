// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.tests.CatsSuite
import lucuma.odb.api.model.arb.ArbOneToManyUnique
import lucuma.odb.api.repo.OneToManyUnique

final class OneToManyUniqueSpec extends CatsSuite {
  type A = Short
  type B = Byte

  import ArbOneToManyUnique._

  test("two way linking") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.all.forall { case (a, b) =>
        o.selectRight(a).contains(b) && o.selectLeft(b).nonEmpty
      })
    }
  }

  test("+ link") {
    forAll { (o: OneToManyUnique[A, B], link: (A, B)) =>
      assert(
        (o.selectLeft(link._2), (o + link)) match {
          case (Some(a), Right(oo)) if a == link._1 => oo.contains(link)
          case (Some(_), Right(_))                  => false
          case (Some(a), Left(_)) if a != link._1   => true
          case (Some(_), Left(_))                   => false
          case (None, Right(oo))                    => oo.contains(link)
          case (None, Left(_))                      => false
        }
      )
    }
  }

  test("+ link already exists") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.all.headOption.forall { link =>
        (o + link).exists(_.contains(link))
      })
    }
  }

  test("+ link uniqueness violation") {
    forAll { (o: OneToManyUnique[A, B], a: A) =>
      assert(o.all.headOption.forall { link =>
        val oo = o + ((a, link._2))
        if (a == link._1) oo.exists(_.contains(link))
        else oo.isLeft
      })
    }
  }

  test("- link exists") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.all.headOption.forall { link =>
        val oo = o - link
        o.contains(link) && !oo.contains(link) && !oo.all.contains(link)
      })
    }
  }

  test("- link wrong A") {
    forAll { (o: OneToManyUnique[A, B], a: A) =>
      assert(o.all.headOption.forall { link =>
        val oo = o - ((a, link._2))
        if (a == link._1)
          o.contains(link) && !oo.contains(link) && !oo.all.contains(link)
        else
          o.all == oo.all && o.allLeft == oo.allLeft && o.allRight == oo.allRight
      })
    }
  }

  test("++ links") {
    forAll { (o: OneToManyUnique[A, B], links: List[(A, B)]) =>
      val oo                = o ++ links
      val linksInconsistent = links.groupMap(_._2)(_._1).exists { case (_, v) => v.toSet.size > 1 }
      val violation         = links.exists { case (a, b) => o.selectLeft(b).exists(_ != a) }
      if (linksInconsistent || violation) assert(oo.isLeft)
      else
        assert(oo.exists { ooo =>
          (ooo.all & links.toSet) == links.toSet
        })
    }
  }

  test("-- links") {
    forAll { (o: OneToManyUnique[A, B], i: Short) =>
      val links = o.all.take(Math.abs(i % 4))
      val oo    = o -- links
      (o.all &~ oo.all) shouldEqual links
    }
  }

  test("removeLeft") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.allLeft.headOption.forall { a =>
        o.removeLeft(a).all.forall(_._1 != a)
      })
    }
  }

  test("removeRight") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.allRight.headOption.forall { b =>
        o.removeRight(b).all.forall(_._2 != b)
      })
    }
  }

  test("contains") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.all.forall(o.contains))
    }
  }

  test("contains2") {
    forAll { (o: OneToManyUnique[A, B], link: (A, B)) =>
      o.contains(link) shouldEqual o.all.contains(link)
    }
  }

  test("allLeft") {
    forAll { o: OneToManyUnique[A, B] =>
      o.allLeft shouldEqual o.all.map(_._1)
    }

  }
  test("allRight") {
    forAll { o: OneToManyUnique[A, B] =>
      o.allRight shouldEqual o.all.map(_._2)
    }
  }

  test("selectLeft") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.all.headOption.map(_._2).forall { b =>
        o.selectLeft(b).toSet == o.all.collect { case (aa, bb) if b == bb => aa }
      })
    }
  }

  test("selectLeft2") {
    forAll { (o: OneToManyUnique[A, B], b: B) =>
      o.selectLeft(b).toSet shouldEqual o.all.collect { case (aa, bb) if b == bb => aa }
    }
  }

  test("selectRight") {
    forAll { o: OneToManyUnique[A, B] =>
      assert(o.all.headOption.map(_._1).forall { a =>
        o.selectRight(a) == o.all.collect { case (aa, bb) if a == aa => bb }
      })
    }
  }

  test("selectRight2") {
    forAll { (o: OneToManyUnique[A, B], a: A) =>
      o.selectRight(a) shouldEqual o.all.collect { case (aa, bb) if a == aa => bb }
    }
  }

  test("size equals all links size") {
    forAll { o: OneToManyUnique[A, B] =>
      o.size shouldEqual o.all.size
    }
  }

  test("isEmpty") {
    forAll { o: OneToManyUnique[A, B] =>
      o.isEmpty shouldEqual (o.size == 0)
    }
  }

  test("nonEmpty") {
    forAll { o: OneToManyUnique[A, B] =>
      o.nonEmpty shouldEqual (o.size > 0)
    }
  }
}
