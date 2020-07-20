// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.Target
import lucuma.odb.api.model.json.TargetJson
import lucuma.odb.api.repo.OdbRepo

import lucuma.core.math.{Coordinates, Offset, ProperVelocity}

import cats.effect.Effect
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._

trait TargetMutation extends TargetJson with TargetScalars {

  import GeneralSchema.EnumTypeExistence
  import ProgramSchema.ProgramIdType
  import TargetSchema.{EphemerisKeyType, TargetIdArgument, TargetIdType, TargetType}

  import context._

  import syntax.inputobjecttype._

  val InputObjectTypeTargetCreateNonsidereal: InputObjectType[Target.CreateNonsidereal] =
    deriveInputObjectType[Target.CreateNonsidereal](
      InputObjectTypeName("CreateNonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target parameters")
    )

  val ArgumentTargetCreateNonsidereal: Argument[Target.CreateNonsidereal] =
    InputObjectTypeTargetCreateNonsidereal.argument(
      "input",
      "Nonsidereal target description"
    )

  implicit val InputObjectTypeOffset: InputObjectType[Offset] =
    deriveInputObjectType[Offset](
      InputObjectTypeName("OffsetInput"),
      InputObjectTypeDescription("Offset in p and q")
    )

  implicit val InputObjectTypeProperVelocity: InputObjectType[ProperVelocity] =
    deriveInputObjectType[ProperVelocity](
      InputObjectTypeName("ProperVelocityInput"),
      InputObjectTypeDescription("ProperVelocity in RA and dec")
    )

  implicit val InputObjectTypeCoordinates: InputObjectType[Coordinates] =
    deriveInputObjectType[Coordinates](
      InputObjectTypeName("CoordinatesInput"),
      InputObjectTypeDescription("RA/Dec Coordinates")
    )

  val InputObjectTypeCreateSidereal: InputObjectType[Target.CreateSidereal] =
    deriveInputObjectType[Target.CreateSidereal](
      InputObjectTypeName("CreateSiderealInput"),
      InputObjectTypeDescription("Sidereal target parameters")
    )

  val ArgumentTargetCreateSidereal: Argument[Target.CreateSidereal] =
    InputObjectTypeCreateSidereal.argument(
      "input",
      "Sidereal target description"
    )

  val InputObjectTypeTargetEditSidereal: InputObjectType[Target.EditSidereal] =
    deriveInputObjectType[Target.EditSidereal](
      InputObjectTypeName("EditSiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters")
    )

  val ArgumentTargetEditSidereal: Argument[Target.EditSidereal] =
    InputObjectTypeTargetEditSidereal.argument(
      "input",
      "Sidereal target edit"
    )

  def createNonsidereal[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createNonsiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetCreateNonsidereal),
      resolve   = c => c.target(_.insertNonsidereal(c.arg(ArgumentTargetCreateNonsidereal)))
    )

  def createSidereal[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "createSiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetCreateSidereal),
      resolve   = c => c.target(_.insertSidereal(c.arg(ArgumentTargetCreateSidereal)))
    )

  //noinspection MutatorLikeMethodIsParameterless
  def updateSidereal[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "updateSiderealTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(ArgumentTargetEditSidereal),
      resolve   = c => c.target(_.edit(c.arg(ArgumentTargetEditSidereal)))
    )

  def delete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "deleteTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.delete(c.targetId))
    )

  def undelete[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name      = "undeleteTarget",
      fieldType = OptionType(TargetType[F]),
      arguments = List(TargetIdArgument),
      resolve   = c => c.target(_.undelete(c.targetId))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      createNonsidereal,
      createSidereal,
      updateSidereal,
      delete,
      undelete
    )

}

object TargetMutation extends TargetMutation
