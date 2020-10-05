// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.ObservationModel
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import cats.syntax.functor._
import cats.syntax.flatMap._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationMutation {

  import AsterismSchema.AsterismIdType
  import GeneralSchema.EnumTypeExistence
  import ObservationSchema.{ObservationIdType, ObservationIdArgument, ObservationType}
  import ProgramSchema.ProgramIdType
  import context._
  import syntax.inputobjecttype._

  val InputObjectTypeObservationCreate: InputObjectType[ObservationModel.Create] =
    deriveInputObjectType[ObservationModel.Create](
      InputObjectTypeName("CreateObservationInput"),
      InputObjectTypeDescription("Observation creation parameters")
    )

  val ArgumentObservationCreate: Argument[ObservationModel.Create] =
    InputObjectTypeObservationCreate.argument(
      "input",
      "Observation description"
    )

  val InputObjectTypeObservationEdit: InputObjectType[ObservationModel.Edit] =
    deriveInputObjectType[ObservationModel.Edit](
      InputObjectTypeName("EditObservationInput"),
      InputObjectTypeDescription("Edit observation")
    )

  val ArgumentObservationEdit: Argument[ObservationModel.Edit] =
    InputObjectTypeObservationEdit.argument(
      "input",
      "Edit observation"
    )

  def create[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createObservation",
      fieldType = OptionType(ObservationType[F]),
      arguments = List(ArgumentObservationCreate),
      resolve   = c => c.observation(_.insert(c.arg(ArgumentObservationCreate)))
    )

  def update[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateObservation",
      fieldType = OptionType(ObservationType[F]),
      arguments = List(ArgumentObservationEdit),
      resolve   = c => c.observation { r =>
        val ed = c.arg(ArgumentObservationEdit)
        for {
          s <- ed.editor.liftTo[F]
          o <- r.edit(ed.id, s)
        } yield o
      }
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteObservation",
      fieldType = OptionType(ObservationType[F]),
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.delete(c.observationId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteObservation",
      fieldType = OptionType(ObservationType[F]),
      arguments = List(ObservationIdArgument),
      resolve   = c => c.observation(_.undelete(c.observationId))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      create,
      update,
      delete,
      undelete
    )

}

object ObservationMutation extends ObservationMutation
