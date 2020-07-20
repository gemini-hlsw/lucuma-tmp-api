// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.NonEmptyChain
import cats.implicits._

// QUESTION: Not sure about this at all.  For most low-level types we can create
// custom scalars and do the corresponding formatting and parsing / validating
// there. However some combinations of values in input types may not be valid
// and will need to be flagged as issues.
//
// I'm guessing Sangria offers a better way to handle this but I couldn't
// divine it.

/**
 * Possible errors that are found in input types (arguments to mutations). These
 * are client-fixable issues, not execution errors.
 */
sealed trait InputError {
  def message: String

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

  /**
   * Indicates that an input value does not conform to expectations.  For
   * example, an RA field that cannot be parsed.
   */
  final case class InvalidField(
    name:    String,
    input:   String,
    failure: String
  ) extends InputError {

    override def message: String =
      s"Could not validate $name field value `$input`: $failure"

  }

  /**
   * Indicates that an input contains an id whose referent could not be found.
   */
  final case class MissingReference(
    name:  String,
    value: String
  ) extends InputError {

    override def message: String =
      s"Could not find $name '$value''"

  }

}