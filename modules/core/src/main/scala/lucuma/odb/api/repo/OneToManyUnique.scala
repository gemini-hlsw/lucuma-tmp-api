// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import cats._
import cats.implicits._
import lucuma.odb.api.model.InputError
import scala.collection.immutable.{SortedMap, SortedSet}

trait OneToManyUnique[A, B] {
  def +(link: (A, B)): Either[InputError, OneToManyUnique[A, B]]

  def ++(links: IterableOnce[(A, B)]): Either[InputError, OneToManyUnique[A, B]]

  def -(link: (A, B)): OneToManyUnique[A, B]

  def --(links: IterableOnce[(A, B)]): OneToManyUnique[A, B]

  def removeLeft(a: A): OneToManyUnique[A, B]

  def removeRight(b: B): OneToManyUnique[A, B]

  def contains(link: (A, B)): Boolean

  def all: SortedSet[(A, B)]

  def allLeft: SortedSet[A]

  def allRight: SortedSet[B]

  def selectLeft(b: B): Option[A]

  def selectRight(a: A): SortedSet[B]

  def size: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean

  override def toString: String =
    s"OneToManyUnique(${all.toList.mkString(",")})"
}

object OneToManyUnique {
  private def fromOneToManyAndMap[A: Order, B: Order](
    aToBs: OneToMany[A, B],
    bToA:  SortedMap[B, A]
  ): OneToManyUnique[A, B] = new OneToManyUnique[A, B] {

    override def +(link: (A, B)): Either[InputError, OneToManyUnique[A, B]] =
      bToA.get(link._2) match {
        case None                    => fromOneToManyAndMap(aToBs + link, bToA + link.swap).asRight
        case Some(a) if a == link._1 => this.asRight
        case Some(a)                 =>
          (InputError(
            s"Uniqueness constraint violation: ${link._2} already shared with $a"
          )).asLeft
      }

    // Note: Fails fast. Errors/successes after the 1st error would be increasingly meaningless
    override def ++(links: IterableOnce[(A, B)]): Either[InputError, OneToManyUnique[A, B]] = {
      @annotation.tailrec
      def loop(
        ls: List[(A, B)],
        o:  OneToManyUnique[A, B]
      ): Either[InputError, OneToManyUnique[A, B]] = ls match {
        case h :: t =>
          o + h match {
            case Right(newO)   => loop(t, newO)
            case err @ Left(_) => err
          }
        case _      => o.asRight
      }
      loop(links.iterator.toList, this)
    }

    override def -(link: (A, B)): OneToManyUnique[A, B] = bToA.get(link._2) match {
      case Some(a) if a == link._1 => fromOneToManyAndMap(aToBs - link, bToA - link._2)
      case _                       => this
    }

    override def --(links: IterableOnce[(A, B)]): OneToManyUnique[A, B] =
      links.iterator.foldLeft(this)((acc, link) => acc - link)

    override def removeLeft(a: A): OneToManyUnique[A, B] =
      fromOneToManyAndMap(aToBs.remove(a), bToA -- selectRight(a))

    override def removeRight(b: B): OneToManyUnique[A, B] = bToA.get(b) match {
      case Some(a) => fromOneToManyAndMap(aToBs - ((a, b)), bToA - b)
      case None    => this
    }

    override def contains(link: (A, B)): Boolean =
      bToA.get(link._2).exists(_ == link._1)

    override def all: SortedSet[(A, B)] = aToBs.all

    override def allLeft: SortedSet[A] = aToBs.keySet

    override def allRight: SortedSet[B] = bToA.keySet

    override def selectLeft(b: B): Option[A] = bToA.get(b)

    override def selectRight(a: A): SortedSet[B] = aToBs.select(a)

    override def size: Int = aToBs.size

    override def isEmpty: Boolean = aToBs.isEmpty

    override def nonEmpty: Boolean = aToBs.nonEmpty
  }

  def empty[A: Order, B: Order]: OneToManyUnique[A, B] =
    fromOneToManyAndMap(OneToMany.empty, SortedMap.empty[B, A])

  def apply[A: Order, B: Order](links: (A, B)*): Either[InputError, OneToManyUnique[A, B]] =
    empty[A, B] ++ links.toList
}
