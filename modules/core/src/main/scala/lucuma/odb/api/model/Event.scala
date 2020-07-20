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

//  object Created {
//
//    def apply[T](value: T)(id: Long): Created[T] =
//      Created(id, value)
//
//  }

  trait Edited[T] extends Event {
    def oldValue: T
    def newValue: T
  }

//  object Edited {
//
//    def apply[T](oldValue: T, newValue: T)(id: Long): Edited[T] =
//      Edited(id, oldValue, newValue)
//
//  }
}
