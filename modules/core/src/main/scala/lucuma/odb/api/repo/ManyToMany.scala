// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats._
import cats.implicits._

import scala.collection.immutable.SortedSet

sealed trait ManyToMany[A, B] {

  def +(link: (A, B)): ManyToMany[A, B]

  def ++(links: IterableOnce[(A, B)]): ManyToMany[A, B]

  def -(link: (A, B)): ManyToMany[A, B]

  def --(links: IterableOnce[(A, B)]): ManyToMany[A, B]

  def removeLeft(a: A): ManyToMany[A, B]

  def removeRight(b: B): ManyToMany[A, B]

  def contains(link: (A, B)): Boolean

  def all: Set[(A, B)]

  def allLeft: SortedSet[A]

  def allRight: SortedSet[B]

  def selectLeft(b: B): SortedSet[A]

  def selectRight(a: A): SortedSet[B]

  def size: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean

  override def toString: String =
    s"ManyToMany(${all.toList.mkString(",")})"

}

object ManyToMany {

  def fromOneToMany[A: Order, B: Order](
    aToBs: OneToMany[A, B],
    bToAs: OneToMany[B, A]
  ): ManyToMany[A, B] =
    new ManyToMany[A, B] {

      def +(link: (A, B)): ManyToMany[A, B] =
        fromOneToMany(aToBs + link, bToAs + link.swap)

      def ++(links: IterableOnce[(A, B)]): ManyToMany[A, B] =
        fromOneToMany(aToBs ++ links, bToAs ++ links.iterator.map(_.swap))

      def -(link: (A, B)): ManyToMany[A, B] =
        fromOneToMany(aToBs - link, bToAs - link.swap)

      def --(links: IterableOnce[(A, B)]): ManyToMany[A, B] =
        fromOneToMany(aToBs -- links, bToAs -- links.iterator.map(_.swap))

      def all: SortedSet[(A, B)] =
        aToBs.all

      def removeLeft(a: A): ManyToMany[A, B] =
        fromOneToMany(
          aToBs.remove(a),
          bToAs -- selectRight(a).toList.tupleRight(a)
        )

      def removeRight(b: B): ManyToMany[A, B] =
        fromOneToMany(
          aToBs -- selectLeft(b).toList.tupleRight(b),
          bToAs.remove(b)
        )

      def contains(link: (A, B)): Boolean =
        aToBs.select(link._1).contains(link._2)

      def allLeft: SortedSet[A] =
        aToBs.keySet

      def allRight: SortedSet[B] =
        bToAs.keySet

      def selectLeft(b: B): SortedSet[A] =
        bToAs.select(b)

      def selectRight(a: A): SortedSet[B] =
        aToBs.select(a)

      def size: Int =
        aToBs.size

      def isEmpty: Boolean =
        aToBs.isEmpty

      def nonEmpty: Boolean =
        aToBs.nonEmpty
    }

  def empty[A: Order, B: Order]: ManyToMany[A, B] =
    fromOneToMany(OneToMany.empty, OneToMany.empty)

  def apply[A: Order, B: Order](links: (A, B)*): ManyToMany[A, B] =
    empty[A, B] ++ links.toList

}