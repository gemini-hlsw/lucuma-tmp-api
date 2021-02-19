// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.schema.syntax.all._
import lucuma.odb.api.model.{DeclinationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`.{CatalogName, EphemerisKeyType, MagnitudeBand, MagnitudeSystem}
import lucuma.core.math.{Coordinates, Declination, MagnitudeValue, Parallax, ProperMotion, RadialVelocity, RightAscension, VelocityAxis}
import lucuma.core.model.{CatalogId, EphemerisKey, Magnitude, SiderealTracking, Target}
import cats.effect.Effect
import sangria.schema._

object TargetSchema extends TargetScalars {

  import AsterismSchema.AsterismConnectionType
  import GeneralSchema.{EnumTypeExistence, ArgumentIncludeDeleted}
  import ObservationSchema.ObservationConnectionType
  import Paging._
  import ProgramSchema.{OptionalProgramIdArgument, ProgramConnectionType}
  import context._

  implicit val TargetIdType: ScalarType[Target.Id] =
    ObjectIdSchema.idType[Target.Id]("TargetId")

  val TargetIdArgument: Argument[Target.Id] =
    Argument(
      name         = "targetId",
      argumentType = TargetIdType,
      description  = "Target ID"
    )

  val OptionalTargetIdArgument: Argument[Option[Target.Id]] =
    Argument(
      name         = "targetId",
      argumentType = OptionInputType(TargetIdType),
      description  = "Target ID"
    )

  implicit val EnumTypeCatalogName: EnumType[CatalogName] =
    EnumType.fromEnumerated(
      "CatalogName",
      "Catalog name values"
    )

  implicit val EnumTypeMagnitudeBand: EnumType[MagnitudeBand] =
    EnumType.fromEnumerated(
      "MagnitudeBand",
      "Magnitude band"
    )

  implicit val EnumTypeMagnitudeSystem: EnumType[MagnitudeSystem] =
    EnumType.fromEnumerated(
      "MagnitudeSystem",
      "Magnitude system"
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

  def CatalogIdType[F[_]: Effect]: ObjectType[OdbRepo[F], CatalogId] =
    ObjectType(
      name = "CatalogId",
      fieldsFn = () => fields(

        Field(
          name        = "name",
          fieldType   = EnumTypeCatalogName,
          description = Some("Catalog name option"),
          resolve     = _.value.catalog
        ),

        Field(
          name        = "id",
          fieldType   = StringType,
          description = Some("Catalog id string"),
          resolve     = _.value.id.value
        )
      )
    )

  def MagnitudeType[F[_]: Effect]: ObjectType[OdbRepo[F], Magnitude] =
    ObjectType(
      name = "Magnitude",
      fieldsFn = () => fields(

        Field(
          name        = "value",
          fieldType   = BigDecimalType,
          description = Some("Magnitude value (unitless)"),
          resolve     = m => MagnitudeValue.fromBigDecimal.reverseGet(m.value.value)
        ),

        Field(
          name        = "band",
          fieldType   = EnumTypeMagnitudeBand,
          description = Some("Magnitude band"),
          resolve     = _.value.band
        ),

        Field(
          name        = "system",
          fieldType   = EnumTypeMagnitudeSystem,
          description = Some("Magnitude System"),
          resolve     = _.value.system
        )

      )
    )

  def ProperMotionComponentType[A, F[_]: Effect](
    name: String,
    componentName: String
  ): ObjectType[OdbRepo[F], ProperMotion.AngularVelocityComponent[A]] =
    ObjectType(
      name     = s"${name.capitalize}${componentName.capitalize}",
      fieldsFn = () => fields(

        Field(
          name        = "microarcsecondsPerYear",
          fieldType   = LongType,
          description = Some(s"Proper motion in $name μas/year"),
          resolve     = v => ProperMotionModel.Units.MicroarcsecondsPerYear.long.get(v.value)
        ),

        Field(
          name        = "milliarcsecondsPerYear",
          fieldType   = BigDecimalType,
          description = Some(s"Proper motion in $name mas/year"),
          resolve     = v => ProperMotionModel.Units.MilliarcsecondsPerYear.decimal.get(v.value)
        )

      )
    )

  def ProperMotionType[F[_]: Effect](name: String): ObjectType[OdbRepo[F], ProperMotion] =
    ObjectType(
      name     = name.capitalize,
      fieldsFn = () => fields(

        Field(
          name        = "ra",
          fieldType   = ProperMotionComponentType[VelocityAxis.RA, F](name, "RA"),
          description = Some("Proper motion in RA"),
          resolve     = _.value.ra
        ),

        Field(
          name        = "dec",
          fieldType   = ProperMotionComponentType[VelocityAxis.Dec, F](name, "declination"),
          description = Some("Proper motion in declination"),
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
          name        = "microarcseconds",
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
          name        = "microarcseconds",
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
          name        = "catalogId",
          fieldType   = OptionType(CatalogIdType[F]),
          description = Some("Catalog id, if any, describing from where the information in this target was obtained"),
          resolve     = _.value.catalogId
        ),

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
          name              = "properMotion",
          fieldType         = OptionType(ProperMotionType[F]("properMotion")),
          description       = Some("Proper motion per year in right ascension and declination"),
          resolve           = _.value.properMotion
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
          fieldType   = AsterismConnectionType[F],
          arguments   = List(
            OptionalProgramIdArgument,
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          description = Some("The asterisms associated with the target."),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingAsterismId) { gid =>
              c.ctx.asterism.selectAllForTarget(c.value.id, c.optionalProgramId, c.pagingFirst, gid, c.includeDeleted)
            }
        ),

        Field(
          name        = "observations",
          fieldType   = ObservationConnectionType[F],
          arguments   = List(
            OptionalProgramIdArgument,
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          description = Some("The observations associated with the target."),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingObservationId) { gid =>
              c.ctx.observation.selectAllForTarget(c.value.id, c.optionalProgramId, c.pagingFirst, gid, c.includeDeleted)
            }
        ),

        Field(
          name        = "programs",
          fieldType   = ProgramConnectionType[F],
          arguments   = List(
            ArgumentPagingFirst,
            ArgumentPagingCursor,
            ArgumentIncludeDeleted
          ),
          description = Some("The programs associated with the target."),
          resolve     = c =>
            unsafeSelectPageFuture(c.pagingProgramId) { gid =>
              c.ctx.program.selectAllForTarget(c.value.id, c.pagingFirst, gid, c.includeDeleted)
            }
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
        ),

        Field(
          name        = "magnitudes",
          fieldType   = ListType(MagnitudeType[F]),
          description = Some("Target magnitudes"),
          resolve     = _.value.target.magnitudes.values.toList
        )
      )
    )

  def TargetEdgeType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Edge[TargetModel]] =
    Paging.EdgeType(
      "TargetEdge",
      "A Target and its cursor",
      TargetType[F]
    )

  def TargetConnectionType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Connection[TargetModel]] =
    Paging.ConnectionType(
      "TargetConnection",
      "Targets in the current page",
      TargetType[F],
      TargetEdgeType[F]
    )

}