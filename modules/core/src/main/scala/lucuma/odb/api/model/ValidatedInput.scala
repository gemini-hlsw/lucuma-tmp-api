// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric.Interval
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

  def optionEither[A, B](nameA: String, nameB: String, a: Option[ValidatedInput[A]], b: Option[ValidatedInput[B]]): ValidatedInput[Option[Either[A, B]]] =
    List(a.map(_.map(_.asLeft[B])), b.map(_.map(_.asRight[A]))).flattenOption match {
      case List(c) => c.map(_.some)
      case Nil     => Option.empty[Either[A, B]].validNec[InputError]
      case _       => InputError.fromMessage(s"Either $nameA or $nameB are permitted but not both").invalidNec
    }

  def nonEmptyString(name: String, s: String): ValidatedInput[NonEmptyString] =
    NonEmptyString
      .from(s)
      .leftMap(err => InputError.fromMessage(s"'$name' may not be empty: $err"))
      .toValidatedNec

  def closedInterval[T](
    name:  String, 
    value: T, 
    low:   T, 
    high:  T
  )(implicit
    v:     Validate[T, Interval.Closed[low.type, high.type]]
  ): ValidatedInput[T Refined Interval.Closed[low.type, high.type]] =
    refineV[Interval.Closed[low.type, high.type]](value)
      .leftMap(_ => InputError.fromMessage(s"'$name' out of range: must be $low<= $name <= $high "))
      .toValidatedNec
}
