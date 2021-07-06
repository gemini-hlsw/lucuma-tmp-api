// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.NonEmptyChain
import cats.implicits._

// I'm guessing Sangria offers a better way to handle this but I couldn't
// divine it.

/**
 * Possible errors that are found in input types (arguments to mutations). These
 * are client-fixable issues, not execution errors.
 */
final case class InputError(message: String) {

  def toException: InputError.Exception =
    InputError.Exception(NonEmptyChain(this))

}

object InputError {

  /**
   * An exception containing the collection of `InputError`s that were
   * encountered.  A custom error handler will convert these into individual
   * error entries in the GraphQL result.
   */
  final case class Exception(nec: NonEmptyChain[InputError]) extends java.lang.Exception {
    override def getMessage: String =
      nec.map(_.message).intercalate("\n")
  }

  def fromMessage(m: String): InputError =
    InputError(m)

  /**
   * Indicates that an input value does not conform to expectations.  For
   * example, an RA field that cannot be parsed.
   */
  def invalidField[A](name: String, input: String, failure: String): InputError =
    fromMessage(s"Could not validate $name field value `$input`: $failure")

  def missingInput[A](what: String): InputError =
    fromMessage(s"No $what definition provided")

  /**
   * Indicates that an input contains an id whose referent could not be found.
   */
  def missingReference[A](name: String, value: String): InputError =
    fromMessage(s"Could not find $name '$value'")

  /**
   * Attempt to reuse an id in the creation of a new object.
   */
  def idClash[A](name: String, value: String): InputError =
    fromMessage(s"$name id '$value' is already in use")

}