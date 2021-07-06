// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package syntax

final class InputValidatorOps[A](a: A) {

  def validateAndCreate[B](implicit V: InputValidator[A, B]): ValidatedInput[B] =
    V.validateAndCreate(a)

}

trait ToInputValidatorOps {

  implicit def toInputValidatorOps[A](a: A): InputValidatorOps[A] =
    new InputValidatorOps[A](a)

}

object inputvalidator extends ToInputValidatorOps