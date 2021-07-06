// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.validated._

// This is probably sketchy.  It's basically a function from input type to
// a ValidatedNec of the the corresponding model type. I needed a way to
// do this generically in ManualSequence.  Perhaps there are better ideas?

/**
 * Validates an input type and creates the corresponding model type.
 *
 * @tparam A input type
 * @tparam B model type to (hopefully) create from the inputs
 */
trait InputValidator[A, B] { self =>

  def validateAndCreate(a: A): ValidatedInput[B]

}

object InputValidator {

  def apply[A, B](implicit ev: InputValidator[A, B]): InputValidator[A, B] =
    ev

  /**
   * Creates an InputValidator from a function.
   */
  def by[A, B](f: A => ValidatedInput[B]): InputValidator[A, B] =
    (a: A) => f(a)

  /**
   * Creates an InputValidator that will always successfully create the
   * corresponding model type.
   */
  def success[A, B](f: A => B): InputValidator[A, B] =
    (a: A) => f(a).validNec[InputError]

}