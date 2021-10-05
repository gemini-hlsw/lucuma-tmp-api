// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class EditTargetInput(
  addSidereal:     Option[CreateSiderealInput],
  addNonsidereal:  Option[CreateNonsiderealInput],
  editSidereal:    Option[EditSiderealInput],
  editNonsidereal: Option[EditNonsiderealInput],
  delete:          Option[SelectTargetInput]
) extends EditTargetAction

object EditTargetInput {

  implicit val DecoderEditTargetInput: Decoder[EditTargetInput] =
    deriveDecoder[EditTargetInput]

  implicit val EqEditTargetInput: Eq[EditTargetInput] =
    Eq.by { a => (
      a.addSidereal,
      a.addNonsidereal,
      a.editSidereal,
      a.editNonsidereal,
      a.delete
    )}

  val Empty: EditTargetInput =
    EditTargetInput(None, None, None, None, None)

  def addSidereal(c: CreateSiderealInput): EditTargetInput =
    Empty.copy(addSidereal = c.some)

  def addNonsidereal(c: CreateNonsiderealInput): EditTargetInput =
    Empty.copy(addNonsidereal = c.some)

  def editSidereal(e: EditSiderealInput): EditTargetInput =
    Empty.copy(editSidereal = e.some)

  def editNonsidereal(e: EditNonsiderealInput): EditTargetInput =
    Empty.copy(editNonsidereal = e.some)

  def delete(d: SelectTargetInput): EditTargetInput =
    Empty.copy(delete = d.some)

}
