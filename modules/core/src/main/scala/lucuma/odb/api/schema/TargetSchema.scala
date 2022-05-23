// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import lucuma.odb.api.schema.syntax.all._
import lucuma.odb.api.model.{DeclinationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{TargetEnvironmentModel, TargetModel}
import lucuma.core.`enum`.{CatalogName, EphemerisKeyType => EphemerisKeyTypeEnum}
import lucuma.core.math.{Coordinates, Declination, Parallax, ProperMotion, RadialVelocity, RightAscension, VelocityAxis}
import lucuma.core.model.{CatalogInfo, EphemerisKey, Target}
import cats.syntax.all._
import cats.effect.std.Dispatcher
import lucuma.core.model.Target.{Nonsidereal, Sidereal}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.GeneralSchema.EnumTypeExistence
import org.typelevel.log4cats.Logger
import sangria.schema.{Field, _}

object TargetSchema extends TargetScalars {

  import GeneralSchema.ArgumentIncludeDeleted
  import ProgramSchema.ProgramType
  import RefinedSchema.NonEmptyStringType
  import SourceProfileSchema._

  import context._

  implicit val TargetIdType: ScalarType[Target.Id] =
    ObjectIdSchema.gidType[Target.Id]("TargetId")

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

  val NonsiderealType: ObjectType[Any, Nonsidereal] =
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
        ),

        Field(
          name        = "key",
          fieldType   = StringType,
          description = "Synthesis of `keyType` and `des`".some,
          resolve     = c => EphemerisKey.fromString.reverseGet(c.value.ephemerisKey)
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

  def ProperMotionComponentType[A](
    name: String,
    componentName: String
  ): ObjectType[Any, ProperMotion.AngularVelocityComponent[A]] =
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

  def ProperMotionType(name: String): ObjectType[Any, ProperMotion] =
    ObjectType(
      name     = name.capitalize,
      fieldsFn = () => fields(

        Field(
          name        = "ra",
          fieldType   = ProperMotionComponentType[VelocityAxis.RA](name, "RA"),
          description = Some("Proper motion in RA"),
          resolve     = _.value.ra
        ),

        Field(
          name        = "dec",
          fieldType   = ProperMotionComponentType[VelocityAxis.Dec](name, "declination"),
          description = Some("Proper motion in declination"),
          resolve     = _.value.dec
        )
      )
    )

  val RightAscensionType: ObjectType[Any, RightAscension] =
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

  val DeclinationType: ObjectType[Any, Declination] =
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

  val CoordinateType: ObjectType[Any, Coordinates] =
    ObjectType(
      name     = "Coordinates",
      fieldsFn = () => fields(

        Field(
          name        = "ra",
          fieldType   = RightAscensionType,
          description = Some("Right Ascension"),
          resolve     = _.value.ra
        ),

        Field(
          name        = "dec",
          fieldType   = DeclinationType,
          description = Some("Declination"),
          resolve     = _.value.dec
        )
      )
    )

  val RadialVelocityType: ObjectType[Any, RadialVelocity] =
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

  val ParallaxType: ObjectType[Any, Parallax] =
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


  val SiderealType: ObjectType[Any, Sidereal] =
    ObjectType(
      name     = "Sidereal",
      fieldsFn = () => fields(

        Field(
          name        = "ra",
          fieldType   = RightAscensionType,
          description = "Right ascension at epoch".some,
          resolve     = _.value.tracking.baseCoordinates.ra
        ),

        Field(
          name        = "dec",
          fieldType   = DeclinationType,
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
          fieldType   = OptionType(ProperMotionType("properMotion")),
          description = Some("Proper motion per year in right ascension and declination"),
          resolve     = _.value.tracking.properMotion
        ),

        Field(
          name        = "radialVelocity",
          fieldType   = OptionType(RadialVelocityType),
          description = Some("Radial velocity"),
          resolve     = _.value.tracking.radialVelocity
        ),

        Field(
          name        = "parallax",
          fieldType   = OptionType(ParallaxType),
          description = Some("Parallax"),
          resolve     = _.value.tracking.parallax
        ),

        Field(
          name        = "catalogInfo",
          fieldType   = OptionType(CatalogInfoType),
          description = Some("Catalog info, if any, describing from where the information in this target was obtained"),
          resolve     = _.value.catalogInfo
        )
      )
    )

  def TargetType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], TargetModel] =

    ObjectType[OdbCtx[F], TargetModel](
      name        = "Target",
      description = "Target description",
      fieldsFn    = () => fields[OdbCtx[F], TargetModel](

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
          name        = "program",
          fieldType   = ProgramType[F],
          description = "Program that contains this target".some,
          arguments   = List(ArgumentIncludeDeleted),
          resolve     = c => c.unsafeToFuture(
            c.ctx.odbRepo.program.unsafeSelect(c.value.programId, c.includeDeleted)
          )
        ),

        Field(
          name        = "name",
          fieldType   = NonEmptyStringType,
          description = Some("Target name."),
          resolve     = _.value.target.name
        ),

        Field(
          name        = "sourceProfile",
          fieldType   = SourceProfileType,
          description = "source profile".some ,
          resolve     = _.value.target.sourceProfile
        ),

        Field(
          name        = "sidereal",
          fieldType   = OptionType(SiderealType),
          description = "Sidereal tracking information, if this is a sidereal target".some,
          resolve     = c => Target.sidereal.getOption(c.value.target)
        ),

        Field(
          name        = "nonsidereal",
          fieldType   = OptionType(NonsiderealType),
          description = "Nonsidereal tracking information, if this is a nonsidereal target".some,
          resolve     = c => Target.nonsidereal.getOption(c.value.target)
        )

      )
    )


  def TargetEdgeType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Edge[TargetModel]] =
    Paging.EdgeType(
      "TargetEdge",
      "A Target and its cursor",
      TargetType[F]
    )

  def TargetConnectionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Connection[TargetModel]] =
    Paging.ConnectionType(
      "TargetConnection",
      "Targets in the current page",
      TargetType[F],
      TargetEdgeType[F]
    )

  def TargetEnvironmentType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], TargetEnvironmentModel] = {

    def asterism(
      env:             TargetEnvironmentModel,
      includeDeleted:  Boolean,
      ctx: OdbCtx[F]
    ): F[List[TargetModel]] =
      env.asterism.toList.flatTraverse(tid => ctx.odbRepo.target.select(tid, includeDeleted).map(_.toList))

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
          fieldType   = OptionType(CoordinateType),
          description = "When set, overrides the default base position of the target group".some,
          resolve     = _.value.explicitBase
        )

      )
    )
  }
}
