// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.StateT
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.Target
import lucuma.odb.api.model.{EitherInput, InputError}
import lucuma.odb.api.model.syntax.lens._


final case class EditAsterismPatchInput(
  add:    Option[Target.Id],
  delete: Option[Target.Id]
) {

  val editor: StateT[EitherInput, TargetEnvironmentModel, Unit] =
    (add, delete) match {
      case (Some(a), None) => TargetEnvironmentModel.asterism.mod_(_ + a)
      case (None, Some(d)) => TargetEnvironmentModel.asterism.mod_(_ - d)
      case (None, None)    => StateT.setF(InputError.fromMessage(s"One of `add` or `delete` must be specified for each operation").leftNec)
      case _               => StateT.setF(InputError.fromMessage(s"Select only one of `add` or `delete` for each operation").leftNec)
    }

}

object EditAsterismPatchInput {

  implicit val DecoderEditTargetInput: Decoder[EditAsterismPatchInput] =
    deriveDecoder[EditAsterismPatchInput]

  implicit val EqEditTargetInput: Eq[EditAsterismPatchInput] =
    Eq.by { a => (
      a.add,
      a.delete
    )}

  val Empty: EditAsterismPatchInput =
    EditAsterismPatchInput(None, None)

  def add(tid: Target.Id): EditAsterismPatchInput =
    Empty.copy(add = tid.some)

  def delete(tid: Target.Id): EditAsterismPatchInput =
    Empty.copy(delete = tid.some)

  def multiEditor(inputs: List[EditAsterismPatchInput]): StateT[EitherInput, TargetEnvironmentModel, Unit] =
    inputs.traverse(_.editor).void

}
