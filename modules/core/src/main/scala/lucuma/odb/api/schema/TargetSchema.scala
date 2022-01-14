// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.MonadError
import lucuma.odb.api.schema.syntax.all._
import lucuma.odb.api.model.{DeclinationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{TargetEnvironmentModel, TargetModel}
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`.{CatalogName, EphemerisKeyType => EphemerisKeyTypeEnum}
import lucuma.core.math.{Coordinates, Declination, Parallax, ProperMotion, RadialVelocity, RightAscension, VelocityAxis}
import lucuma.core.model.{AngularSize, CatalogInfo, Target}
import cats.syntax.all._
import cats.effect.std.Dispatcher
import lucuma.core.model.Target.{Nonsidereal, Sidereal}
import lucuma.odb.api.schema.GeneralSchema.EnumTypeExistence
import sangria.schema.{Field, _}

object TargetSchema extends TargetScalars {

  import AngleSchema.AngleType
  import GeneralSchema.{ArgumentIncludeDeleted, NonEmptyStringType}
  import ProgramSchema.ProgramType
  import SourceProfileSchema._

  import context._

  implicit val TargetIdType: ScalarType[Target.Id] =
    ObjectIdSchema.idType[Target.Id]("TargetId")

  val ArgumentTargetId: Argument[Target.Id] =
    Argument(
      name         = "targetId",
      argumentType = TargetIdType,
      description  = "Target ID"
    )

  val ArgumentOptionalTargetId: Argument[Option[Target.Id]] =
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

  implicit val EphemerisKeyTypeEnumType: EnumType[EphemerisKeyTypeEnum] =
    EnumType.fromEnumerated(
      "EphemerisKeyType",
      "Ephemeris key type options"
    )

  def NonsiderealType[F[_]]: ObjectType[OdbRepo[F], Nonsidereal] =
    ObjectType(
      name     = "Nonsidereal",
      fieldsFn = () => fields(

        Field(
          name        = "des",
          fieldType   = StringType,
          description = Some("Human readable designation that discriminates among ephemeris keys of the same type."),
          resolve     = _.value.ephemerisKey.des
        ),

        Field(
          name        = "keyType",
          fieldType   = EphemerisKeyTypeEnumType,
          description = Some("Nonsidereal target lookup type."),
          resolve     = _.value.ephemerisKey.keyType
        )
      )
    )

  val CatalogInfoType: ObjectType[Any, CatalogInfo] =
    ObjectType(
      name = "CatalogInfo",
      fieldsFn = () => fields(

        Field(
          name        = "name",
          fieldType   = EnumTypeCatalogName,
          description = "Catalog name option".some,
          resolve     = _.value.catalog
        ),

        Field(
          name        = "id",
          fieldType   = StringType,
          description = "Catalog id string".some,
          resolve     = _.value.id.value
        ),

        Field(
          name        = "objectType",
          fieldType   = OptionType(StringType),
          description = "Catalog description of object morphology".some,
          resolve     = _.value.objectType.map(_.value)

        )
      )
    )

  val AngularSizeType: ObjectType[Any, AngularSize] =
    ObjectType(
      name = "AngularSize",
      fieldsFn = () => fields(

        Field(
          name      = "majorAxis",
          fieldType = AngleType,
          resolve   = _.value.majorAxis
        ),

        Field(
          name      = "minorAxis",
          fieldType = AngleType,
          resolve   = _.value.minorAxis
        )
      )
    )

  def ProperMotionComponentType[A, F[_]](
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

  def ProperMotionType[F[_]](name: String): ObjectType[OdbRepo[F], ProperMotion] =
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

  def RightAscensionType[F[_]]: ObjectType[OdbRepo[F], RightAscension] =
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

  def DeclinationType[F[_]]: ObjectType[OdbRepo[F], Declination] =
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

  def CoordinateType[F[_]]: ObjectType[OdbRepo[F], Coordinates] =
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

  def RadialVelocityType[F[_]]: ObjectType[OdbRepo[F], RadialVelocity] =
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

  def ParallaxType[F[_]]: ObjectType[OdbRepo[F], Parallax] =
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


  def SiderealType[F[_]]: ObjectType[OdbRepo[F], Sidereal] =
    ObjectType(
      name     = "Sidereal",
      fieldsFn = () => fields(

        Field(
          name        = "catalogInfo",
          fieldType   = OptionType(CatalogInfoType),
          description = Some("Catalog info, if any, describing from where the information in this target was obtained"),
          resolve     = _.value.catalogInfo
        ),

        Field(
          name        = "ra",
          fieldType   = RightAscensionType[F],
          description = "Right ascension at epoch".some,
          resolve     = _.value.tracking.baseCoordinates.ra
        ),

        Field(
          name        = "dec",
          fieldType   = DeclinationType[F],
          description = "Declination at epoch".some,
          resolve     = _.value.tracking.baseCoordinates.dec
        ),

        Field(
          name        = "epoch",
          fieldType   = EpochStringType,
          description = Some("Epoch, time of base observation"),
          resolve     = _.value.tracking.epoch
        ),

        Field(
          name        = "properMotion",
          fieldType   = OptionType(ProperMotionType[F]("properMotion")),
          description = Some("Proper motion per year in right ascension and declination"),
          resolve     = _.value.tracking.properMotion
        ),

        Field(
          name        = "radialVelocity",
          fieldType   = OptionType(RadialVelocityType[F]),
          description = Some("Radial velocity"),
          resolve     = _.value.tracking.radialVelocity
        ),

        Field(
          name        = "parallax",
          fieldType   = OptionType(ParallaxType[F]),
          description = Some("Parallax"),
          resolve     = _.value.tracking.parallax
        )
      )
    )

  def TargetType[F[_]: Dispatcher](
    implicit ev: MonadError[F, Throwable]
  ): ObjectType[OdbRepo[F], TargetModel] =

    ObjectType[OdbRepo[F], TargetModel](
      name        = "Target",
      description = "Target description",
      fieldsFn    = () => fields[OdbRepo[F], TargetModel](

        Field(
          name        = "id",
          fieldType   = TargetIdType,
          description = "Target ID".some,
          resolve     = _.value.id
        ),

        Field(
          name        = "existence",
          fieldType   = EnumTypeExistence,
          description = "DELETED or PRESENT".some,
          resolve     = _.value.existence
        ),

        Field(
          name        = "name",
          fieldType   = NonEmptyStringType,
          description = Some("Target name."),
          resolve     = _.value.target.name
        ),

        Field(
          name        = "program",
          fieldType   = ProgramType[F],
          description = "Program that contains this target".some,
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.unsafeToFuture(
            c.ctx.program.unsafeSelect(c.value.programId, c.includeDeleted)
          )
        ),

        Field(
          name        = "sourceProfile",
          fieldType   = SourceProfileType,
          description = "source profile".some ,
          resolve     = _.value.target.sourceProfile
        ),

        Field(
          name        = "sidereal",
          fieldType   = OptionType(SiderealType[F]),
          description = "Sidereal tracking information, if this is a sidereal target".some,
          resolve     = c => Target.sidereal.getOption(c.value.target)
        ),

        Field(
          name        = "nonsidereal",
          fieldType   = OptionType(NonsiderealType[F]),
          description = "Nonsidereal tracking information, if this is a nonsidereal target".some,
          resolve     = c => Target.nonsidereal.getOption(c.value.target)
        ),

        Field(
          name        = "angularSize",
          fieldType   = OptionType(AngularSizeType),
          resolve     = c => Target.angularSize.get(c.value.target)
        )

      )
    )


  def TargetEdgeType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Edge[TargetModel]] =
    Paging.EdgeType(
      "TargetEdge",
      "A Target and its cursor",
      TargetType[F]
    )

  def TargetConnectionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Connection[TargetModel]] =
    Paging.ConnectionType(
      "TargetConnection",
      "Targets in the current page",
      TargetType[F],
      TargetEdgeType[F]
    )

  def TargetEnvironmentType[F[_]: Dispatcher](
    implicit ev: MonadError[F, Throwable]
  ): ObjectType[OdbRepo[F], TargetEnvironmentModel] = {

    def asterism(
      env:             TargetEnvironmentModel,
      includeDeleted:  Boolean,
      repo:            OdbRepo[F]
    ): F[List[TargetModel]] =
      env.asterism.toList.flatTraverse(tid => repo.target.select(tid, includeDeleted).map(_.toList))

    ObjectType(
      name = "TargetEnvironment",
      fieldsFn = () => fields(

        Field(
          name        = "asterism",
          fieldType   = ListType(TargetType[F]),
          description = "All the observation's science targets, if any".some,
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c =>
            c.unsafeToFuture(
              asterism(c.value, c.includeDeleted, c.ctx)
            )
        ),

        Field(
          name        = "firstScienceTarget",
          fieldType   = OptionType(TargetType[F]),
          description = "First, perhaps only, science target in the asterism".some,
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c =>
            c.unsafeToFuture(
              asterism(c.value, c.includeDeleted, c.ctx).map(_.headOption)
            )
        ),

        // TODO: a `base` field that takes a date and time and tells you where
        // TODO: the center of all the targets will be at that time (if defined
        // TODO: it would be the `explicitBase` otherwise the center of pm
        // TODO: corrected science targets)

        Field(
          name        = "explicitBase",
          fieldType   = OptionType(CoordinateType[F]),
          description = "When set, overrides the default base position of the target group".some,
          resolve     = _.value.explicitBase
        )

      )
    )
  }
}
