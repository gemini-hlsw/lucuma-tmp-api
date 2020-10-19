// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.schema.syntax.all._
import lucuma.odb.api.model.{DeclinationModel, ParallaxModel, ProperVelocityModel, RadialVelocityModel, RightAscensionModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.math.{Coordinates, Declination, Parallax, ProperVelocity, RadialVelocity, RightAscension, VelocityAxis}
import lucuma.core.model.{ EphemerisKey, SiderealTracking }
import cats.effect.Effect
import cats.syntax.eq._
import cats.syntax.functor._
import sangria.schema._

object TargetSchema extends TargetScalars {

  import AsterismSchema.AsterismType
  import GeneralSchema.{EnumTypeExistence, ArgumentIncludeDeleted}
  import ObservationSchema.ObservationType
  import ProgramSchema.{OptionalProgramIdArgument, ProgramType}
  import context._

  implicit val TargetIdType: ScalarType[TargetModel.Id] =
    ObjectIdSchema.idType[TargetModel.Id]("TargetId")

  val TargetIdArgument: Argument[TargetModel.Id] =
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

  def ProperVelocityComponentType[A, F[_]: Effect](
    name: String
  ): ObjectType[OdbRepo[F], ProperVelocity.AngularVelocityComponent[A]] =
    ObjectType(
      name     = s"ProperVelocity$name",
      fieldsFn = () => fields(

        Field(
          name        = "microarcsecondsPerYear",
          fieldType   = LongType,
          description = Some(s"Proper velocity in $name μas/year"),
          resolve     = v => ProperVelocityModel.Units.MicroarcsecondsPerYear.long.get(v.value)
        ),

        Field(
          name        = "milliarcsecondsPerYear",
          fieldType   = BigDecimalType,
          description = Some(s"Proper velocity in $name mas/year"),
          resolve     = v => ProperVelocityModel.Units.MilliarcsecondsPerYear.decimal.get(v.value)
        )

      )
    )

  def ProperVelocityType[F[_]: Effect]: ObjectType[OdbRepo[F], ProperVelocity] =
    ObjectType(
      name     = "ProperVelocity",
      fieldsFn = () => fields(

        Field(
          name        = "ra",
          fieldType   = ProperVelocityComponentType[VelocityAxis.RA, F]("RA"),
          description = Some("Proper velocity in RA"),
          resolve     = _.value.ra
        ),

        Field(
          name        = "dec",
          fieldType   = ProperVelocityComponentType[VelocityAxis.Dec, F]("declination"),
          description = Some("Proper velocity in declination"),
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
          fieldType   = BigDecimalType,// FloatType,
          description = Some("Right Ascension (RA) in hours"),
          resolve     = v => RightAscensionModel.Units.Hours.decimal.get(v.value)// RightAscension.fromHourAngle.reverseGet(v.value).toDoubleHours
        ),

        Field(
          name        = "degrees",
          fieldType   = BigDecimalType,
          description = Some("Right Ascension (RA) in degrees"),
          resolve     = v => RightAscensionModel.Units.Degrees.decimal.get(v.value)
        ),

        Field(
          name        = "microarcsecs",
          fieldType   = LongType,
          description = Some("Right Ascension (RA) in µas"),
          resolve     = v => RightAscensionModel.Units.Microarcseconds.long.get(v.value)
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
          fieldType   = BigDecimalType,
          description = Some("Declination in signed degrees"),
          resolve     = v => DeclinationModel.Units.Degrees.decimal.reverseGet(v.value)//.value.toAngle.toSignedDoubleDegrees
        ),

        Field(
          name        = "microarcsecs",
          fieldType   = LongType,
          description = Some("Declination in signed µas"),
          resolve     = v => DeclinationModel.Units.Microarcseconds.long.reverseGet(v.value)//.signedMicroarcseconds.get(v.value.toAngle)
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

  def RadialVelocityType[F[_]: Effect]: ObjectType[OdbRepo[F], RadialVelocity] =
    ObjectType(
      name     = "RadialVelocity",
      fieldsFn = () => fields(

        Field(
          name        = "centimetersPerSecond",
          fieldType   = LongType,
          description = Some("Radial velocity in cm/s"),
          resolve     = v => RadialVelocityModel.Units.CentimetersPerSecond.long.reverseGet(v.value)
        ),

        Field(
          name        = "metersPerSecond",
          fieldType   = BigDecimalType,
          description = Some("Radial velocity in m/s"),
          resolve     = v => RadialVelocityModel.Units.MetersPerSecond.decimal.reverseGet(v.value)
        ),

        Field(
          name        = "kilometersPerSecond",
          fieldType   = BigDecimalType,
          description = Some("Radial velocity in km/s"),
          resolve     = v => RadialVelocityModel.Units.KilometersPerSecond.decimal.reverseGet(v.value)
        )

      )
    )

  def ParallaxType[F[_]: Effect]: ObjectType[OdbRepo[F], Parallax] =
    ObjectType(
      name     = "Parallax",
      fieldsFn = () => fields(

        Field(
          name        = "microarcseconds",
          fieldType   = LongType,
          description = Some("Parallax in microarcseconds"),
          resolve     = v => ParallaxModel.Units.Microarcseconds.long.get(v.value)
        ),

        Field(
          name        = "milliarcseconds",
          fieldType   = BigDecimalType,
          description = Some("Parallax in milliarcseconds"),
          resolve     = v => ParallaxModel.Units.Milliarcseconds.decimal.get(v.value)
        )

      )
    )


  def SiderealType[F[_]: Effect]: ObjectType[OdbRepo[F], SiderealTracking] =
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
          fieldType   = OptionType(RadialVelocityType[F]),
          description = Some("Radial velocity"),
          resolve     = _.value.radialVelocity
        ),

        Field(
          name        = "parallax",
          fieldType   = OptionType(ParallaxType[F]),
          description = Some("Parallax"),
          resolve     = _.value.parallax
        )
      )
    )


  def TrackingType[F[_]: Effect]: OutputType[Either[EphemerisKey, SiderealTracking]] =
    UnionType(
      name        = "Tracking",
      description = Some("Either Nonsidereal ephemeris lookup key or Sidereal proper motion."),
      types       = List(NonsiderealType[F], SiderealType[F])
    ).mapValue[Either[EphemerisKey, SiderealTracking]](
      _.fold(
        key => key: Any,
        st  => st: Any
      )
    )

  def TargetType[F[_]: Effect]: ObjectType[OdbRepo[F], TargetModel] =
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
          name        = "asterisms",
          fieldType   = ListType(AsterismType[F]),
          arguments   = List(OptionalProgramIdArgument, ArgumentIncludeDeleted),
          description = Some("The asterisms associated with the target."),
          resolve     = c =>
            c.asterism { repo =>
              c.optionalProgramId.fold(
                repo.selectAllForTarget(c.value.id, c.includeDeleted)
              ) { pid =>
                repo.selectAllForProgram(pid, c.includeDeleted).map(_.filter(_.targets(c.value.id)))
              }
            }
        ),

        Field(
          name        = "observations",
          fieldType   = ListType(ObservationType[F]),
          arguments   = List(OptionalProgramIdArgument, ArgumentIncludeDeleted),
          description = Some("The observations associated with the target."),
          resolve     = c => c.observation(
            _.selectAllForTarget(c.value.id, c.includeDeleted)
             .map { obsList =>
               c.optionalProgramId.fold(obsList) { pid =>
                 obsList.filter(_.pid === pid)
               }
             }
          )
        ),

        Field(
          name        = "programs",
          fieldType   = ListType(ProgramType[F]),
          arguments   = List(ArgumentIncludeDeleted),
          description = Some("The programs associated with the target."),
          resolve     = c => c.program(_.selectAllForTarget(c.value.id, c.includeDeleted))
        ),

        Field(
          name        = "name",
          fieldType   = StringType,
          description = Some("Target name."),
          resolve     = _.value.target.name.value
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