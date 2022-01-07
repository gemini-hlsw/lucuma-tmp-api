// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.data.State
import cats.Eq
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Target
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.{InputError, ValidatedInput}


final case class EditAsterismInput(
  add:    Option[Target.Id],
  delete: Option[Target.Id]
) {

  val editor: ValidatedInput[State[TargetEnvironmentModel, Unit]] =
    (add, delete) match {
      case (Some(a), None) => TargetEnvironmentModel.asterism.mod_(_ + a).validNec[InputError]
      case (None, Some(d)) => TargetEnvironmentModel.asterism.mod_(_ - d).validNec[InputError]
      case (None, None)    => InputError.fromMessage(s"One of `add` or `delete` must be specified for each operation").invalidNec[State[TargetEnvironmentModel, Unit]]
      case _               => InputError.fromMessage(s"Select only one of `add` or `delete` for each operation").invalidNec[State[TargetEnvironmentModel, Unit]]
    }

}

object EditAsterismInput {

  implicit val DecoderEditTargetInput: Decoder[EditAsterismInput] =
    deriveDecoder[EditAsterismInput]

  implicit val EqEditTargetInput: Eq[EditAsterismInput] =
    Eq.by { a => (
      a.add,
      a.delete
    )}

  val Empty: EditAsterismInput =
    EditAsterismInput(None, None)

  def add(tid: Target.Id): EditAsterismInput =
    Empty.copy(add = tid.some)

  def delete(tid: Target.Id): EditAsterismInput =
    Empty.copy(delete = tid.some)

  def multiEditor(inputs: List[EditAsterismInput]): ValidatedInput[State[TargetEnvironmentModel, Unit]] =
    inputs.traverse(_.editor).map(_.sequence.void)

}
