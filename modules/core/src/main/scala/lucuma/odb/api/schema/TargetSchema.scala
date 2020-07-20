// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.schema.syntax.all._
import lucuma.odb.api.model.Target
import lucuma.odb.api.model.json.TargetJson
import lucuma.odb.api.schema.ProgramSchema.ProgramType
import lucuma.odb.api.repo.OdbRepo

import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.optics.SplitMono
import lucuma.core.math.{
  Angle,
  Coordinates,
  Declination,
  HourAngle,
  Offset,
  ProperMotion,
  ProperVelocity,
  RightAscension
}
import lucuma.core.model.EphemerisKey

import cats.effect.Effect
import sangria.schema._

object TargetSchema extends TargetJson with TargetScalars {

  import GeneralSchema.EnumTypeExistence
  import context._

  implicit val TargetIdType: ScalarType[Target.Id] =
    ObjectIdSchema.idType[Target.Id]("TargetId")

  val TargetIdArgument: Argument[Target.Id] =
    Argument(
      name         = "id",
      argumentType = TargetIdType,
      description  = "Target ID"
    )

  implicit val EphemerisKeyType: EnumType[EphemerisKeyType] =
    EnumType.fromEnumerated(
      "EphemerisKeyType",
      "Ephemeris key type options"
    )

  def NonsiderealType[F[_]: Effect]: ObjectType[OdbRepo[F], EphemerisKey] =
    ObjectType(
      name     = "Nonsidereal",
      fieldsFn = () => fields(

        Field(
          name        = "des",
          fieldType   = StringType,
          description = Some("Human readable designation that discriminates among ephemeris keys of the same type."),
          resolve     = _.value.des
        ),

        Field(
          name        = "keyType",
          fieldType   = EphemerisKeyType,
          description = Some("Nonsidereal target lookup type."),
          resolve     = _.value.keyType
        )
      )
    )

  def OffsetType[F[_]: Effect]: ObjectType[OdbRepo[F], Offset] =
    ObjectType(
      name     = "Offset",
      fieldsFn = () => fields(

        Field(
          name        = "p",
          fieldType   = OffsetPStringType,
          description = Some("Offset in p expressed as signed decimal arcseconds"),
          resolve     = _.value.p
        ),

        Field(
          name        = "q",
          fieldType   = OffsetQStringType,
          description = Some("Offset in q expressed as signed decimal arcseconds"),
          resolve     = _.value.q
        )
      )
    )

  def ProperVelocityType[F[_]: Effect]: ObjectType[OdbRepo[F], ProperVelocity] =
    ObjectType(
      name     = "ProperVelocity",
      fieldsFn = () => fields(

        Field(
          name        = "ra",
          fieldType   = ProperVelocityRaStringType,
          description = Some("Proper velocity in RA, signed decimal milliarcseconds per year (mas/y)"),
          resolve     = _.value.ra
        ),

        Field(
          name        = "dec",
          fieldType   = ProperVelocityDecStringType,
          description = Some("Proper velocity in dec, signed decimal milliarcseconds per year (mas/y)"),
          resolve     = _.value.dec
        )
      )
    )

  def RightAscensionType[F[_]: Effect]: ObjectType[OdbRepo[F], RightAscension] =
    ObjectType(
      name     = "RightAscension",
      fieldsFn = () => fields(

        Field(
          name        = "hms",
          fieldType   = HmsStringType,
          description = Some("Right Ascension (RA) in HH:MM:SS.SSS format"),
          resolve     = _.value
        ),

        Field(
          name        = "hours",
          fieldType   = FloatType,
          description = Some("Right Ascension (RA) in hours"),
          resolve     = v => RightAscension.fromHourAngle.reverseGet(v.value).toDoubleHours
        ),

        Field(
          name        = "degrees",
          fieldType   = FloatType,
          description = Some("Right Ascension (RA) in degrees"),
          resolve     = v =>
            SplitMono
              .fromIso(RightAscension.fromHourAngle.reverse)
              .composeSplitMono(HourAngle.angle)
              .get(v.value)
              .toDoubleDegrees
        ),

        Field(
          name        = "microarcsecs",
          fieldType   = LongType,
          description = Some("Right Ascension (RA) in µas"),
          resolve     = v =>
            RightAscension
              .fromHourAngle
              .reverseGet(v.value)
              .toMicroarcseconds
        )
      )
    )

  def DeclinationType[F[_]: Effect]: ObjectType[OdbRepo[F], Declination] =
    ObjectType(
      name     = "Declination",
      fieldsFn = () => fields(

        Field(
          name        = "dms",
          fieldType   = DmsStringType,
          description = Some("Declination in DD:MM:SS.SS format"),
          resolve     = _.value
        ),

        Field(
          name        = "degrees",
          fieldType   = FloatType,
          description = Some("Declination in signed degrees"),
          resolve     = _.value.toAngle.toSignedDoubleDegrees
        ),

        Field(
          name        = "microarcsecs",
          fieldType   = LongType,
          description = Some("Declination in signed µas"),
          resolve     = v => Angle.signedMicroarcseconds.get(v.value.toAngle)
        )
      )
    )

  def CoordinateType[F[_]: Effect]: ObjectType[OdbRepo[F], Coordinates] =
    ObjectType(
      name     = "Coordinates",
      fieldsFn = () => fields(

        Field(
          name        = "ra",
          fieldType   = RightAscensionType[F],
          description = Some("Right Ascension"),
          resolve     = _.value.ra
        ),

        Field(
          name        = "dec",
          fieldType   = DeclinationType[F],
          description = Some("Declination"),
          resolve     = _.value.dec
        )
      )
    )

  def SiderealType[F[_]: Effect]: ObjectType[OdbRepo[F], ProperMotion] =
    ObjectType(
      name     = "Sidereal",
      fieldsFn = () => fields(

        Field(
          name        = "coordinates",
          fieldType   = CoordinateType[F],
          description = Some("Coordinates at epoch"),
          resolve     = _.value.baseCoordinates
        ),

        Field(
          name        = "epoch",
          fieldType   = EpochStringType,
          description = Some("Epoch, time of base observation"),
          resolve     = _.value.epoch //v => math.Epoch.fromString.reverseGet(v.value.epoch)
        ),

        Field(
          name        = "properVelocity",
          fieldType   = OptionType(ProperVelocityType[F]),
          description = Some("Proper velocity per year in right ascension and declination"),
          resolve     = _.value.properVelocity
        ),

        Field(
          name        = "radialVelocity",
          fieldType   = OptionType(RadialVelocityType),
          description = Some("Radial velocity in m/s"),
          resolve     = _.value.radialVelocity
        )
      )
    )


  def TrackingType[F[_]: Effect]: OutputType[Either[EphemerisKey, ProperMotion]] =
    UnionType(
      name        = "Tracking",
      description = Some("Either Nonsidereal ephemeris lookup key or Sidereal proper motion."),
      types       = List(NonsiderealType[F], SiderealType[F])
    ).mapValue[Either[EphemerisKey, ProperMotion]](
      _.fold(
        key => key: Any,
        pm  => pm: Any
      )
    )

  def TargetType[F[_]: Effect]: ObjectType[OdbRepo[F], Target] =
    ObjectType(
      name     = "Target",
      fieldsFn = () => fields(
        Field(
          name        = "id",
          fieldType   = TargetIdType,
          description = Some("Target id."),
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = Some("Deleted or Present"),
          resolve     = _.value.existence
        ),

        Field(
          name        = "programs",
          fieldType   = ListType(ProgramType[F]),
          description = Some("The program associated with the target."),
          resolve     = c => c.program(_.selectAllForTarget(c.value.id))
        ),

        Field(
          name        = "name",
          fieldType   = StringType,
          description = Some("Target name."),
          resolve     = _.value.target.name
        ),

        Field(
          name        = "tracking",
          fieldType   = TrackingType[F],
          description = Some("Information required to find a target in the sky."),
          resolve     = _.value.target.track
        )
      )
    )

}