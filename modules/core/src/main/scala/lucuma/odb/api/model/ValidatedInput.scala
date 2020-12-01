// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.either._
import cats.syntax.functorFilter._
import cats.syntax.validated._
import eu.timepit.refined.types.string._

object ValidatedInput {

  def requireOne[A](name: String, a: Option[ValidatedInput[A]]*): ValidatedInput[A] =
    requireOne(name, a.toList)

  def requireOne[A](name: String, as: List[Option[ValidatedInput[A]]]): ValidatedInput[A] =
    as.flattenOption match {

      case List(a) => a
      case Nil     => InputError.missingInput(name).invalidNec[A]
      case _       => InputError.fromMessage(s"Multiple '$name' definitions are not permitted").invalidNec[A]

    }

  def nonEmptyString(name: String, s: String): ValidatedInput[NonEmptyString] =
    NonEmptyString
      .from(s)
      .leftMap(err => InputError.fromMessage(s"'$name' may not be empty: $err"))
      .toValidatedNec
}
