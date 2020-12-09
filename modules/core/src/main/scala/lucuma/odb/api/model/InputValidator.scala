// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.validated._

trait InputValidator[A, B] { self =>

  def validateAndCreate(a: A): ValidatedInput[B]

}

object InputValidator {

  def apply[A, B](implicit ev: InputValidator[A, B]): InputValidator[A, B] =
    ev

  def success[A, B](f: A => B): InputValidator[A, B] =
    (a: A) => f(a).validNec[InputError]

  def from[A, B](f: A => ValidatedInput[B]): InputValidator[A, B] =
    (a: A) => f(a)

}