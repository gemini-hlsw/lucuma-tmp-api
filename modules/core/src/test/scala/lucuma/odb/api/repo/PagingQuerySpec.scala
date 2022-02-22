// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.syntax.all._
import cats.kernel.instances.order._
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll
import eu.timepit.refined.types.all.PosInt
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.model.Program
import lucuma.odb.api.model.Database
import lucuma.odb.api.model.arb.ArbDatabase

import scala.collection.immutable.SortedSet


final class PagingQuerySpec extends ScalaCheckSuite with OdbRepoTest {

  import ArbDatabase._

  property("selectAll") {
    forAll {  (db: Database, pageSize: PosInt) =>

      val (a, b) = runTest(db) { odb =>
        val pages = allPages(pageSize, None, odb.program).map(flattenToNodes)
        val all   = odb.program.selectAll()
        (pages, all).tupled
      }

      assertEquals(a, b)
    }
  }

  property("totalCount same on all result pages") {
    forAll {  (db: Database, pageSize: PosInt) =>

      val sizes = runTest(db) { odb =>
        val pages = allPages(pageSize, None, odb.program)
        pages.map(_.map(_.totalCount).toSet)
      }

      assertEquals(sizes.size, 1)
    }
  }

  property("totalCount includes all matching items") {
    forAll {  (db: Database, pageSize: PosInt) =>

      val (a, b) = runTest(db) { odb =>
        val pages = allPages(pageSize, None, odb.program)
        (pages.map(_.map(_.totalCount).toSet.headOption.getOrElse(0)),
         pages.map(_.map(_.nodes.size).sum)
        ).tupled
      }

      assertEquals(a, b)
    }
  }

  property("filter") {
    forAll {  (db: Database, pageSize: PosInt) =>

      val (evens, all) = runTest(db) { odb =>
        val pages = allPagesFiltered(pageSize, None, odb.program) { p =>
          p.id.value.value % 2 === 0
        }.map(idSet(_.id))
        val all   = odb.program.selectAll().map(_.map(_.id).toSet)
        (pages, all).tupled
      }

      assert((all &~ evens).forall(_.value.value % 2 === 1))
    }
  }

  property("hasNextPage") {
    forAll { (db: Database, pageSize: PosInt) =>

      val res = runTest(db) { odb => allPages(pageSize, None, odb.program) }

      assert(res.init.forall(_.hasNextPage))
      assert(res.lastOption.forall(!_.hasNextPage))
    }
  }

  property("pageSize") {
    forAll { (db: Database, pageSize: PosInt) =>

      val res = runTest(db) { odb => allPages(pageSize, None, odb.program) }

      assert(res.init.forall(_.nodes.size == pageSize.value))
      assert(res.lastOption.forall(_.nodes.size <= pageSize.value))
    }
  }

  property("afterGid") {
    forAll { (db: Database, pageSize: PosInt, index: PosInt) =>

      val keys         = db.programs.rows.filter { case (_, p) => p.existence.isPresent }.keys
      val afterGid     = if (keys.isEmpty) Option.empty[Program.Id] else keys.toVector.get((index.value % keys.size).toLong)

      val (all, after) = runTest(db) { odb =>
        (allPages(PosInt.MaxValue, None, odb.program).map(idSet(_.id)),
          allPages(pageSize, afterGid, odb.program).map(idSet(_.id))
        ).tupled
      }

      val expected = afterGid.fold(SortedSet.empty[Program.Id]) { gid =>
        all.dropWhile(_ =!= gid)
      }.drop(1)

      assertEquals(after, expected)
    }
  }

  private def flattenToNodes[T](pages: List[ResultPage[T]]): List[T] =
    pages.flatMap(_.nodes)

  private def idSet[I: Ordering, T](toId: T => I)(pages: List[ResultPage[T]]): SortedSet[I] =
    SortedSet.from(flattenToNodes(pages).map(toId))

}
