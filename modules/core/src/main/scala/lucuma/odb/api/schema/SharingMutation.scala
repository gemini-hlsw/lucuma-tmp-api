// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.Sharing
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.model.{Asterism, Program, Target}

import cats.MonadError
import cats.effect.std.Dispatcher
import io.circe.Decoder
import org.typelevel.log4cats.Logger
import sangria.macros.derive._
import sangria.marshalling.FromInput
import sangria.marshalling.circe._
import sangria.schema._


trait SharingMutation {

  import AsterismSchema.{AsterismIdType, AsterismType}
  import ProgramSchema.ProgramIdType
  import TargetSchema.{TargetIdType, TargetType}

  import context._
  import syntax.inputobjecttype._

  def linksArg[A: ScalarType: Decoder, B: ScalarType: Decoder](
    aName:       String,
    bName:       String
  ): Argument[Sharing[A, B]] = {

    val aField: String = s"${aName}Id"
    val bField: String = s"${bName}Ids"

    val iot: InputObjectType[Sharing[A, B]] =
      deriveInputObjectType[Sharing[A, B]](
        InputObjectTypeName(s"${aName.capitalize}${bName.capitalize}Links"),
        InputObjectTypeDescription(s"${aName.capitalize} and the ${bName}s with which it is associated"),
        RenameInputField("one", aField),
        RenameInputField("many", bField)
      )

    val fi: FromInput[Sharing[A, B]] =
      circeDecoderFromInput(Sharing.customDecoder[A, B](aField, bField))

    toInputObjectTypeOps(iot)(fi).argument(
      "input",
      s"${aName.capitalize} / $bName links"
    )
  }

  // ---- Asterism Programs

  val ArgumentAsterismProgramLinks: Argument[Sharing[Asterism.Id, Program.Id]] =
    linksArg[Asterism.Id, Program.Id]("asterism", "program")

  def shareAsterismWithPrograms[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareAsterismWithPrograms",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismProgramLinks),
      resolve   = c => c.asterism(_.shareWithPrograms(c.arg(ArgumentAsterismProgramLinks)))
    )

  def unshareAsterismWithPrograms[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareAsterismWithPrograms",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismProgramLinks),
      resolve   = c => c.asterism(_.unshareWithPrograms(c.arg(ArgumentAsterismProgramLinks)))
    )

  // ---- Asterism Targets

  val ArgumentAsterismTargetLinks: Argument[Sharing[Asterism.Id, Target.Id]] =
    linksArg[Asterism.Id, Target.Id]("asterism", "target")

  def shareAsterismWithTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareAsterismWithTargets",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismTargetLinks),
      resolve   = c => c.asterism(_.shareWithTargets(c.arg(ArgumentAsterismTargetLinks)))
    )

  def unshareAsterismWithTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareAsterismWithTargets",
      fieldType = AsterismType[F],
      arguments = List(ArgumentAsterismTargetLinks),
      resolve   = c => c.asterism(_.unshareWithTargets(c.arg(ArgumentAsterismTargetLinks)))
    )

  // ---- Target Asterisms
  val ArgumentTargetAsterismLinks: Argument[Sharing[Target.Id, Asterism.Id]] =
    linksArg[Target.Id, Asterism.Id]("target", "asterism")

  def shareTargetWithAsterisms[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareTargetWithAsterisms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetAsterismLinks),
      resolve   = c => c.target(_.shareWithAsterisms(c.arg(ArgumentTargetAsterismLinks)))
    )

  def unshareTargetWithAsterisms[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareTargetWithAsterisms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetAsterismLinks),
      resolve   = c => c.target(_.unshareWithAsterisms(c.arg(ArgumentTargetAsterismLinks)))
    )

  // ---- Target Programs
  val ArgumentTargetProgramLinks: Argument[Sharing[Target.Id, Program.Id]] =
    linksArg[Target.Id, Program.Id]("target", "program")

  def shareTargetWithPrograms[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "shareTargetWithPrograms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetProgramLinks),
      resolve   = c => c.target(_.shareWithPrograms(c.arg(ArgumentTargetProgramLinks)))
    )

  def unshareTargetWithPrograms[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name      = "unshareTargetWithPrograms",
      fieldType = TargetType[F],
      arguments = List(ArgumentTargetProgramLinks),
      resolve   = c => c.target(_.unshareWithPrograms(c.arg(ArgumentTargetProgramLinks)))
    )


  def allFields[F[_]: Logger: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      shareAsterismWithPrograms,
      unshareAsterismWithPrograms,

      shareAsterismWithTargets,
      unshareAsterismWithTargets,

      shareTargetWithAsterisms,
      unshareTargetWithAsterisms,

      shareTargetWithPrograms,
      unshareTargetWithPrograms,
    )

}

object SharingMutation extends SharingMutation
