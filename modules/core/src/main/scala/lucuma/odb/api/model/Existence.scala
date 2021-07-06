// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.{Display, Enumerated}
import lucuma.core.syntax.enumerated._

import cats.implicits._
import monocle.Iso

sealed trait Existence extends Product with Serializable {

  def fold[B](ifDeleted: => B, ifPresent: => B): B =
    this match {
      case Existence.Deleted => ifDeleted
      case Existence.Present => ifPresent
    }

  def isDeleted: Boolean =
    this === Existence.Deleted

  def isPresent: Boolean =
    this === Existence.Present

}

object Existence {

  case object Present extends Existence

  case object Deleted extends Existence

  implicit val DisplayExistence: Display[Existence] =
    Display.by[Existence](_.tag, {
      case Present => "Present (Not Deleted)"
      case Deleted => "Deleted"
    })

  implicit val EnumeratedExistence: Enumerated[Existence] =
    Enumerated.of(Present, Deleted)

  def boolean: Iso[Existence, Boolean] =
    Iso[Existence, Boolean](_.isPresent)(b => if (b) Present else Deleted)

}
