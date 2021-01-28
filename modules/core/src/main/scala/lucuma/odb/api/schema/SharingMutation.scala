// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.Sharing
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats.effect.Effect
import io.chrisdavenport.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._


trait SharingMutation {

  import AsterismSchema.{AsterismIdType, AsterismType}
  import ObservationSchema.ObservationIdType
  import ProgramSchema.ProgramIdType
  import TargetSchema.{TargetIdType, TargetType}

  import context._
  import syntax.inputobjecttype._

  // ---- Asterism Observations

  val InputObjectAsterismObservationLinks: InputObjectType[Sharing[Asterism.Id, Observation.Id]] =
    deriveInputObjectType[Sharing[Asterism.Id, Observation.Id]](
      InputObjectTypeName("AsterismObservationLinks"),
      InputObjectTypeDescription("Asterism and the observations with which they are associated"),
      RenameInputField("one", "asterismId"),
      RenameInputField("many", "observationIds")
    )

  val ArgumentAsterismObservationLinks: Argument[Sharing[Asterism.Id, Observation.Id]] =
    InputObjectAsterismObservationLinks.argument(
      "input",
      "Asterism/observation links"
    )

  def shareAsterismWithObservations[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareAsterismWithObservations",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismObservationLinks),
      resolve   = c => c.asterism(_.shareWithObservations(c.arg(ArgumentAsterismObservationLinks)))
    )

  def unshareAsterismWithObservations[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareAsterismWithObservations",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismObservationLinks),
      resolve   = c => c.asterism(_.unshareWithObservations(c.arg(ArgumentAsterismObservationLinks)))
    )

  // ---- Asterism Programs

  val InputObjectAsterismProgramLinks: InputObjectType[Sharing[Asterism.Id, Program.Id]] =
    deriveInputObjectType[Sharing[Asterism.Id, Program.Id]](
      InputObjectTypeName("AsterismProgramLinks"),
      InputObjectTypeDescription("Asterism and the programs with which they are associated"),
      RenameInputField("one", "asterismId"),
      RenameInputField("many", "programIds")
    )

  val ArgumentAsterismProgramLinks: Argument[Sharing[Asterism.Id, Program.Id]] =
    InputObjectAsterismProgramLinks.argument(
      "input",
      "Asterism/program links"
    )

  def shareAsterismWithPrograms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareAsterismWithPrograms",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismProgramLinks),
      resolve   = c => c.asterism(_.shareWithPrograms(c.arg(ArgumentAsterismProgramLinks)))
    )

  def unshareAsterismWithPrograms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareAsterismWithPrograms",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismProgramLinks),
      resolve   = c => c.asterism(_.unshareWithPrograms(c.arg(ArgumentAsterismProgramLinks)))
    )

  // ---- Asterism Targets

  val InputObjectAsterismTargetLinks: InputObjectType[Sharing[Asterism.Id, Target.Id]] =
    deriveInputObjectType[Sharing[Asterism.Id, Target.Id]](
      InputObjectTypeName("AsterismTargetLinks"),
      InputObjectTypeDescription("Asterism and the targets with which they are associated"),
      RenameInputField("one", "asterismId"),
      RenameInputField("many", "targetIds")
    )

  val ArgumentAsterismTargetLinks: Argument[Sharing[Asterism.Id, Target.Id]] =
    InputObjectAsterismTargetLinks.argument(
      "input",
      "Asterism/target links"
    )

  def shareAsterismWithTargets[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareAsterismWithTargets",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismTargetLinks),
      resolve   = c => c.asterism(_.shareWithTargets(c.arg(ArgumentAsterismTargetLinks)))
    )

  def unshareAsterismWithTargets[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareAsterismWithTargets",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismTargetLinks),
      resolve   = c => c.asterism(_.unshareWithTargets(c.arg(ArgumentAsterismTargetLinks)))
    )

  // ---- Target Asterisms

  val InputObjectTargetAsterismLinks: InputObjectType[Sharing[Target.Id, Asterism.Id]] =
    deriveInputObjectType[Sharing[Target.Id, Asterism.Id]](
      InputObjectTypeName("TargetAsterismLinks"),
      InputObjectTypeDescription("Targets and the asterisms with which they are associated"),
      RenameInputField("one", "targetId"),
      RenameInputField("many", "asterismIds")
    )

  val ArgumentTargetAsterismLinks: Argument[Sharing[Target.Id, Asterism.Id]] =
    InputObjectTargetAsterismLinks.argument(
      "input",
      "Target/observation links"
    )

  def shareTargetWithAsterisms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareTargetWithAsterisms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetAsterismLinks),
      resolve   = c => c.target(_.shareWithAsterisms(c.arg(ArgumentTargetAsterismLinks)))
    )

  def unshareTargetWithAsterisms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareTargetWithAsterisms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetAsterismLinks),
      resolve   = c => c.target(_.unshareWithAsterisms(c.arg(ArgumentTargetAsterismLinks)))
    )

  // ---- Target Observations

  val InputObjectTargetObservationLinks: InputObjectType[Sharing[Target.Id, Observation.Id]] =
    deriveInputObjectType[Sharing[Target.Id, Observation.Id]](
      InputObjectTypeName("TargetObservationLinks"),
      InputObjectTypeDescription("Targets and the observations with which they are associated"),
      RenameInputField("one", "targetId"),
      RenameInputField("many", "observationIds")
    )

  val ArgumentTargetObservationLinks: Argument[Sharing[Target.Id, Observation.Id]] =
    InputObjectTargetObservationLinks.argument(
      "input",
      "Target/observation links"
    )

  def shareTargetWithObservations[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareTargetWithObservations",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetObservationLinks),
      resolve   = c => c.target(_.shareWithObservations(c.arg(ArgumentTargetObservationLinks)))
    )

  def unshareTargetWithObservations[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareTargetWithObservations",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetObservationLinks),
      resolve   = c => c.target(_.unshareWithObservations(c.arg(ArgumentTargetObservationLinks)))
    )

  // ---- Target Programs

  val InputObjectTargetProgramLinks: InputObjectType[Sharing[Target.Id, Program.Id]] =
    deriveInputObjectType[Sharing[Target.Id, Program.Id]](
      InputObjectTypeName("TargetProgramLinks"),
      InputObjectTypeDescription("Targets and the programs with which they are associated"),
      RenameInputField("one", "targetId"),
      RenameInputField("many", "programIds")
    )

  val ArgumentTargetProgramLinks: Argument[Sharing[Target.Id, Program.Id]] =
    InputObjectTargetProgramLinks.argument(
      "input",
      "Target/program links"
    )

  def shareTargetWithPrograms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareTargetWithPrograms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetProgramLinks),
      resolve   = c => c.target(_.shareWithPrograms(c.arg(ArgumentTargetProgramLinks)))
    )

  def unshareTargetWithPrograms[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareTargetWithPrograms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetProgramLinks),
      resolve   = c => c.target(_.unshareWithPrograms(c.arg(ArgumentTargetProgramLinks)))
    )


  def allFields[F[_]: Effect: Logger]: List[Field[OdbRepo[F], Unit]] =
    List(
      shareAsterismWithObservations,
      unshareAsterismWithObservations,

      shareAsterismWithPrograms,
      unshareAsterismWithPrograms,

      shareAsterismWithTargets,
      unshareAsterismWithTargets,

      shareTargetWithAsterisms,
      unshareTargetWithAsterisms,

      shareTargetWithObservations,
      unshareTargetWithObservations,

      shareTargetWithPrograms,
      unshareTargetWithPrograms
    )

}

object SharingMutation extends SharingMutation