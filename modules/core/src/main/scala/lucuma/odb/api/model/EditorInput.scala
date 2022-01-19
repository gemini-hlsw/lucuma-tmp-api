// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT

/**
 * Describes how to validate arguments and create a new A or else validate
 * arguments and edit an existing A.
 *
 * @tparam A type of the editing target
 */
trait EditorInput[A] {

  def create: ValidatedInput[A]

  def edit: StateT[EitherInput, A, Unit]

}
