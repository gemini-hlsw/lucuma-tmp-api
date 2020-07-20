// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.Observation
import lucuma.odb.api.model.json.ObservationJson
import lucuma.odb.api.repo.OdbRepo

import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait ObservationMutation extends ObservationJson {

  import AsterismSchema.AsterismIdType
  import GeneralSchema.EnumTypeExistence
  import ObservationSchema.{ObservationIdType, ObservationIdArgument, ObservationType}
  import ProgramSchema.ProgramIdType
  import context._
  import syntax.inputobjecttype._

  val InputObjectTypeObservationCreate: InputObjectType[Observation.Create] =
    deriveInputObjectType[Observation.Create](
      InputObjectTypeName("CreateObservationInput"),
      InputObjectTypeDescription("Observation creation parameters")
    )

  val ArgumentObservationCreate: Argument[Observation.Create] =
    InputObjectTypeObservationCreate.argument(
      "input",
      "Observation description"
    )

  val InputObjectTypeObservationEdit: InputObjectType[Observation.Edit] =
    deriveInputObjectType[Observation.Edit](
      InputObjectTypeName("EditObservationInput"),
      InputObjectTypeDescription("Edit observation")
    )

  val ArgumentObservationEdit: Argument[Observation.Edit] =
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
      resolve   = c => c.observation(_.edit(c.arg(ArgumentObservationEdit)))
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
