// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats._
import cats.implicits._

import scala.collection.immutable.{SortedMap, SortedSet, TreeSet}

sealed trait OneToMany[A, B] {

  def +(link: (A, B)): OneToMany[A, B]

  def ++(links: IterableOnce[(A, B)]): OneToMany[A, B]

  def -(link: (A, B)): OneToMany[A, B]

  def --(links: IterableOnce[(A, B)]): OneToMany[A, B]

  def remove(a: A): OneToMany[A, B]

  def all: SortedSet[(A, B)]

  def keySet: SortedSet[A]

  def select(a: A): SortedSet[B]

  def size: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean

}

object OneToMany {

  private def insert[A, B: Order](
    m: SortedMap[A, SortedSet[B]],
    l: (A, B)
  ): SortedMap[A, SortedSet[B]] =
    m.updatedWith(l._1) {
      case None     => Some(TreeSet(l._2))
      case Some(bs) => Some(bs + l._2)
    }

  private def delete[A, B](
    map:  SortedMap[A, SortedSet[B]],
    link: (A, B)
  ): SortedMap[A, SortedSet[B]] =
    map.updatedWith(link._1) {
      case None     => None
      case Some(bs) => Some(bs - link._2).filter(_.nonEmpty)
    }

  def fromMap[A: Order, B: Order](
    map: SortedMap[A, SortedSet[B]]
  ): OneToMany[A, B] =
    new OneToMany[A, B] {
      override def +(link: (A, B)): OneToMany[A, B] =
        fromMap(insert(map, link))

      override def ++(links: IterableOnce[(A, B)]): OneToMany[A, B] =
        fromMap(links.iterator.foldLeft(map)(insert))

      override def -(link: (A, B)): OneToMany[A, B] =
        fromMap(delete(map, link))

      override def --(links: IterableOnce[(A, B)]): OneToMany[A, B] =
        fromMap(links.iterator.foldLeft(map)(delete))

      override def remove(a: A): OneToMany[A, B] =
        fromMap(map.removed(a))

      override def all: SortedSet[(A, B)] =
        map.foldLeft(SortedSet.empty[(A, B)]) { case (s, (a, bs)) =>
          bs.foldLeft(s) { (sʹ, b) => sʹ + (a -> b) }
        }

      override def keySet: SortedSet[A] =
        map.keySet

      override def select(a: A): SortedSet[B] =
        map.getOrElse(a, TreeSet.empty[B])

      override def size: Int =
        map.foldLeft(0) { case (c, (_, bs)) => c + bs.size }

      override def isEmpty: Boolean =
        map.isEmpty

      override def nonEmpty: Boolean =
        !isEmpty
    }

  def apply[A: Order, B: Order](
    links: (A, B)*
  ): OneToMany[A, B] =
    fromMap(links.foldLeft(SortedMap.empty[A, SortedSet[B]])(insert))

  def empty[A: Order, B: Order]: OneToMany[A, B] =
    apply()

}
