// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

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


  trait Created[T] extends Event {
    def value: T
  }

  trait Edited[T] extends Event {
    def oldValue: T
    def newValue: T
  }

}
