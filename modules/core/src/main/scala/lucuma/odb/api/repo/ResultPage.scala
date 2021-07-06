// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats.{Eq, Order}
import cats.syntax.all._

import scala.collection.immutable.SortedSet

final case class ResultPage[A](
  nodes:       List[A],
  hasNextPage: Boolean,
  totalCount:  Int
)

object ResultPage {

  def empty[A]: ResultPage[A] =
    ResultPage(Nil, hasNextPage = false, 0)

  private def fromIterator[A](
    count:      Option[Int],
    it:         Iterator[A],
    totalCount: Int
  ): ResultPage[A] = {
    val res = scala.collection.mutable.Buffer.empty[A]
    while (it.hasNext && (res.size < count.getOrElse(Int.MaxValue))) res += it.next()
    ResultPage(res.toList, it.hasNext, totalCount)
  }

  def fromSeq[A, B: Eq](
    all:   Seq[A],
    count: Option[Int],
    after: Option[B],
    toB:   A => B
  ): ResultPage[A] =

    fromIterator[A](
      count,
      after.fold(all) { b =>
        all.dropWhile(a => toB(a) =!= b).dropWhile(a => toB(a) === b)
      }.iterator,
      all.size
    )

  def select[A: Order, B](
    count:   Option[Int],
    after:   Option[A],
    keys:    SortedSet[A],
    lookup:  A => B,
    include: B => Boolean
  ): ResultPage[B] =

    ResultPage.fromIterator(
      count,
      after
        .fold(keys.iterator)(a => keys.iteratorFrom(a).dropWhile(_ === a))
        .map(lookup)
        .filter(include),
      keys.count(k => include(lookup(k)))
    )


  def EqResultPage[A: Eq]: Eq[ResultPage[A]] =
    Eq.by { p => (
      p.nodes,
      p.hasNextPage,
      p.totalCount
    )}

}
