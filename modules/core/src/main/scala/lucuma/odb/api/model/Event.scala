// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.util.Enumerated

trait Event {
  def id: Long
}

object Event {

  case object Initialize extends Event {
    def id: Long =
      0L
  }

  def initialize: Event =
    Initialize

  sealed trait EditType extends Product with Serializable

  object EditType {
    case object Created extends EditType
    case object Updated extends EditType

    implicit val EnumeratedEditType: Enumerated[EditType] =
      Enumerated.of(Created, Updated)
  }

  trait Edit[T] extends Event {
    def editType: EditType
    def value: T
  }

}
