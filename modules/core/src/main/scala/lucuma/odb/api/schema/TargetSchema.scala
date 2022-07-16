// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import clue.data.syntax._
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.{Decoder, HCursor}
import io.circe.refined._
import lucuma.core.enums.{CatalogName, EphemerisKeyType => EphemerisKeyTypeEnum}
import lucuma.core.math.{Coordinates, Declination, Parallax, ProperMotion, RadialVelocity, RightAscension, VelocityAxis}
import lucuma.core.model.{CatalogInfo, EphemerisKey, Target}
import lucuma.core.model.Target.{Nonsidereal, Sidereal}
import lucuma.odb.api.model.{CoordinatesModel, DeclinationModel, Existence, ObservationModel, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel}
import lucuma.odb.api.model.targetModel.{CatalogInfoInput, EditAsterismPatchInput, NonsiderealInput, SiderealInput, TargetEnvironmentInput, TargetEnvironmentModel, TargetModel, WhereTargetInput}
import lucuma.odb.api.model.query.{SizeLimitedResult, WhereEqInput, WhereOrderInput}
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.GeneralSchema.EnumTypeExistence
import org.typelevel.log4cats.Logger
import sangria.macros.derive.{ReplaceInputField, _}
import sangria.marshalling.circe._
import sangria.schema.{Field, _}

object TargetSchema extends TargetScalars {

  import GeneralSchema.{ArgumentIncludeDeleted, InputObjectTypeWhereEqExistence}
  import ObservationSchema.{ArgumentObservationId, ObservationIdType}
  import ProgramSchema.{ProgramIdType, ProgramType, InputObjectWhereOrderProgramId}
  import RefinedSchema.{NonEmptyStringType, NonNegIntType}
  import SourceProfileSchema._
  import QuerySchema._

  import context._
  import syntax.all._

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

  implicit val InputObjectWhereOrderTargetId: InputObjectType[WhereOrderInput[Target.Id]] =
    inputObjectWhereOrder("TargetId", TargetIdType)

  implicit val InputObjectWhereTarget: InputObjectType[WhereTargetInput] =
    InputObjectType[WhereTargetInput](
      "WhereTarget",
      "Target filter options.  All specified items must match.",
      () =>
        combinatorFields(InputObjectWhereTarget, "target") :::
          List(
            InputObjectWhereOrderTargetId.optionField("id", "Matches the target id."),
            InputObjectWhereOrderProgramId.optionField("programId", "Matches the id of the associated program."),
            InputObjectWhereString.optionField("name", "Matches the target name."),
            InputField("existence", OptionInputType(InputObjectTypeWhereEqExistence), "By default matching is limited to PRESENT targets.  Use this filter to include DELETED targets as well, for example.", WhereEqInput.EQ(Existence.Present: Existence).some)
          )
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

  implicit val ArgumentOptionWhereTarget: Argument[Option[WhereTargetInput]] =
    Argument(
      name         = "WHERE",
      argumentType = OptionInputType(InputObjectWhereTarget),
      description  = "Filters the selection of targets."
    )

  implicit val ArgumentOptionOffsetTarget: Argument[Option[Target.Id]] =
    Argument(
      name         = "OFFSET",
      argumentType = OptionInputType(TargetIdType),
      description  = "Starts the result set at (or after if not existent) the given target id."
    )

  def target[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "target",
      fieldType   = OptionType(TargetType[F]),
      description = "Retrieves the target with the given id, if it exists".some,
      arguments   = List(
        ArgumentTargetId,
        ArgumentIncludeDeleted
      ),
      resolve     = c => c.target(_.select(c.targetId, c.includeDeleted))
    )

  implicit def TargetSelectResult[F[_]: Dispatcher: Async: Logger]: ObjectType[Any, SizeLimitedResult[TargetModel]] =
    SelectResultType[TargetModel]("target", TargetType[F])

  def targets[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "targets",
      fieldType   = TargetSelectResult[F],
      description = "Selects the first `LIMIT` matching targets based on the provided `WHERE` parameter, if any.".some,
      arguments   = List(ArgumentOptionWhereTarget, ArgumentOptionOffsetTarget, ArgumentOptionLimit, ArgumentIncludeDeleted),
      resolve     = c => {
        val where = c.arg(ArgumentOptionWhereTarget).getOrElse(WhereTargetInput.MatchAll)
        val off   = c.arg(ArgumentOptionOffsetTarget)
        val limit = c.resultSetLimit
        c.target(_.selectWhere(where, off, limit, c.includeDeleted))
      }
    )

  def asterism[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "asterism",
      fieldType   = ListType(TargetType[F]),
      description = "All science targets (if any) for the given observation (or environment)".some,
      arguments   = List(ArgumentObservationId, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationAsterism(c.observationId, c.includeDeleted).map(_.toList))
    )

  def targetEnvironment[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "targetEnvironment",
      fieldType   = OptionType(TargetEnvironmentType[F]),
      description = "Target environment for the given observation (or environment id)".some,
      arguments   = List(ArgumentObservationId),
      resolve     = c => c.target(_.selectObservationTargetEnvironment(c.observationId))
    )

  def queryFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      target[F],
      targets[F],
      asterism[F],
      targetEnvironment[F]
    )

  implicit val EnumTypeDeclinationUnits: EnumType[DeclinationModel.Units] =
    EnumType.fromEnumerated(
      "DeclinationUnits",
      "Unit options for Declination values"
    )

  implicit val EnumTypeRightAscensionUnits: EnumType[RightAscensionModel.Units] =
    EnumType.fromEnumerated(
      "RightAscensionUnits",
      "Unit options for RightAscension values"
    )

  implicit val EnumTypeProperMotionUnits: EnumType[ProperMotionModel.Units] =
    EnumType.fromEnumerated(
      "ProperMotionComponentUnits",
      "Unit options for proper motion components (RA and Dec)"
    )

  implicit val EnumTypeRadialVelocityUnits: EnumType[RadialVelocityModel.Units] =
    EnumType.fromEnumerated(
      "RadialVelocityUnits",
      "Unit options for radial velocity values"
    )

  implicit val EnumTypeParallaxUnits: EnumType[ParallaxModel.Units] =
    EnumType.fromEnumerated(
      "ParallaxUnits",
      "Unit options for parallax values"
    )

  implicit val InputObjectCatalogInfo: InputObjectType[CatalogInfoInput] =
    deriveInputObjectType[CatalogInfoInput](
      InputObjectTypeName("CatalogInfoInput"),
      InputObjectTypeDescription("Catalog id consisting of catalog name, string identifier and an optional object type"),
      ReplaceInputField("name",       EnumTypeCatalogName.notNullableField("name")),
      ReplaceInputField("id",         NonEmptyStringType.notNullableField("id")),
      ReplaceInputField("objectType", NonEmptyStringType.nullableField("objectType"))
    )

  implicit val InputObjectTypeCoordinates: InputObjectType[CoordinatesModel.Input] =
    deriveInputObjectType[CoordinatesModel.Input](
      InputObjectTypeName("CoordinatesInput"),
      InputObjectTypeDescription("Absolute coordinates relative base epoch")
    )

  implicit val InputObjectDeclination: InputObjectType[DeclinationModel.Input] =
    deriveInputObjectType[DeclinationModel.Input](
      InputObjectTypeName("DeclinationInput"),
      InputObjectTypeDescription("Declination, choose one of the available units")
    )

  implicit val InputObjectRightAscension: InputObjectType[RightAscensionModel.Input] =
    deriveInputObjectType[RightAscensionModel.Input](
      InputObjectTypeName("RightAscensionInput"),
      InputObjectTypeDescription("Right Ascension, choose one of the available units")
    )

  implicit val InputObjectProperMotionComponent: InputObjectType[ProperMotionModel.ComponentInput] =
    deriveInputObjectType[ProperMotionModel.ComponentInput](
      InputObjectTypeName("ProperMotionComponentInput"),
      InputObjectTypeDescription(s"Proper motion component, choose one of the available units")
    )

  implicit val InputObjectProperMotion: InputObjectType[ProperMotionModel.Input] =
    deriveInputObjectType[ProperMotionModel.Input](
      InputObjectTypeName("ProperMotionInput"),
      InputObjectTypeDescription("Proper motion, choose one of the available units")
    )

  implicit val InputObjectRadialVelocity: InputObjectType[RadialVelocityModel.Input] =
    deriveInputObjectType[RadialVelocityModel.Input](
      InputObjectTypeName("RadialVelocityInput"),
      InputObjectTypeDescription("Radial velocity, choose one of the available units")
    )

  implicit val InputObjectParallax: InputObjectType[ParallaxModel.Input] =
    deriveInputObjectType[ParallaxModel.Input](
      InputObjectTypeName("ParallaxInput"),
      InputObjectTypeDescription("Parallax, choose one of the available units")
    )

  implicit val InputObjectTypeNonsidereal: InputObjectType[NonsiderealInput] =
    deriveInputObjectType[NonsiderealInput](
      InputObjectTypeName("NonsiderealInput"),
      InputObjectTypeDescription("Nonsidereal target parameters.  Supply `keyType` and `des` or `key`"),

      ReplaceInputField("keyType",       EphemerisKeyTypeEnumType      .notNullableField("keyType"      )),
      ReplaceInputField("des",           NonEmptyStringType            .notNullableField("des"          )),
      ReplaceInputField("key",           NonEmptyStringType            .notNullableField("key"          )),
    )

  implicit val InputObjectTypeSidereal: InputObjectType[SiderealInput] =
    deriveInputObjectType[SiderealInput](
      InputObjectTypeName("SiderealInput"),
      InputObjectTypeDescription("Sidereal target edit parameters"),

      ReplaceInputField("ra",             InputObjectRightAscension.notNullableField("ra"         )),
      ReplaceInputField("dec",            InputObjectDeclination   .notNullableField("dec"        )),
      ReplaceInputField("epoch",          EpochStringType          .notNullableField("epoch"      )),
      ReplaceInputField("properMotion",   InputObjectProperMotion  .nullableField("properMotion"  )),
      ReplaceInputField("radialVelocity", InputObjectRadialVelocity.nullableField("radialVelocity")),
      ReplaceInputField("parallax",       InputObjectParallax      .nullableField("parallax"      )),
      ReplaceInputField("catalogInfo",    InputObjectCatalogInfo   .nullableField("catalogInfo"   ))
    )

  implicit val InputObjectTypeTargetProperties: InputObjectType[TargetModel.PropertiesInput] =
    InputObjectType[TargetModel.PropertiesInput](
      "TargetPropertiesInput",
      "Target properties",
      List(
        InputField("name",          OptionInputType(NonEmptyStringType)),
        InputField("sidereal",      OptionInputType(InputObjectTypeSidereal)),
        InputField("nonsidereal",   OptionInputType(InputObjectTypeNonsidereal)),
        InputField("sourceProfile", OptionInputType(InputObjectSourceProfile)),
        InputField("existence",     OptionInputType(EnumTypeExistence))
      )
    )

  implicit val InputObjectTypeCreateTarget: InputObjectType[TargetModel.CreateInput] =
    deriveInputObjectType[TargetModel.CreateInput](
      InputObjectTypeName("CreateTargetInput"),
      InputObjectTypeDescription("Target creation parameters")
    )

  val ArgumentTargetCreate: Argument[TargetModel.CreateInput] =
    InputObjectTypeCreateTarget.argument(
      "input",
      "Target description.  One (and only one) of sidereal or nonsidereal must be specified."
    )

  implicit val InputObjectTypeUpdateTargets: InputObjectType[TargetModel.UpdateInput] =
    InputObjectType[TargetModel.UpdateInput](
      "UpdateTargetsInput",
      "Target selection and update description. Use `SET` to specify the changes, `WHERE` to select the targets to update, and `LIMIT` to control the size of the return value.",
      List(
        InputField("SET",  InputObjectTypeTargetProperties, "Describes the target values to modify."),
        InputObjectWhereTarget.optionField("WHERE", "Filters the targets to be updated according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional targets match the WHERE clause they will be updated but not returned)."),
        InputField("includeDeleted", BooleanType, "Set to `true` to include deleted targets", false)
      )
    )

  val ArgumentUpdateTargets: Argument[TargetModel.UpdateInput] =
    InputObjectTypeUpdateTargets.argument(
      "input",
      "Parameters for updating existing targets. "
    )

  def existenceEditInput(name: String): InputObjectType[TargetModel.UpdateInput] =
    InputObjectType[TargetModel.UpdateInput](
      s"${name.capitalize}TargetsInput",
      s"Selects the targets for $name",
      List(
        InputObjectWhereTarget.optionField("WHERE", s"Filters the targets for $name according to those that match the given constraints."),
        NonNegIntType.optionField("LIMIT", "Caps the number of results returned to the given value (if additional targets match the WHERE clause they will be updated but not returned).")
        // leave out the "set" since that is implied
      )
    )

  val InputObjectTypeTargetDelete: InputObjectType[TargetModel.UpdateInput] =
    existenceEditInput("delete")

  val InputObjectTypeTargetUndelete: InputObjectType[TargetModel.UpdateInput] =
    existenceEditInput("undelete")


  implicit val InputObjectTypeTargetEnvironment: InputObjectType[TargetEnvironmentInput] =
    InputObjectType[TargetEnvironmentInput](
      "TargetEnvironmentInput",
      "Target environment editing and creation parameters",
      List(
        InputObjectTypeCoordinates.nullableField("explicitBase"),
        InputField("asterism", OptionInputType(ListInputType(TargetIdType)))
      )
    )

  implicit val InputObjectTypeEditAsterisms: InputObjectType[EditAsterismPatchInput] =
    deriveInputObjectType[EditAsterismPatchInput](
      InputObjectTypeName("EditAsterismsPatchInput"),
      InputObjectTypeDescription("Add or delete targets in an asterism")
    )

  implicit val InputObjectCloneTarget: InputObjectType[TargetModel.CloneInput] =
    deriveInputObjectType[TargetModel.CloneInput](
      InputObjectTypeName("CloneTargetInput"),
      InputObjectTypeDescription("Describes a target clone operation, making any edits in the `SET` parameter and replacing the target in the selected `REPLACE_IN` observations")
    )

  val ArgumentCloneTarget: Argument[TargetModel.CloneInput] =
    InputObjectCloneTarget.argument(
      "input",
      "Parameters for cloning an existing target"
    )

   def CreateTargetResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], TargetModel.CreateResult] =
    ObjectType(
      name        = "CreateTargetResult",
      description = "The result of creating a new target.",
      fieldsFn    = () => fields(

        Field(
          name        = "target",
          description = "The newly created target.".some,
          fieldType   = TargetType[F],
          resolve     = _.value.target
        )

      )
    )

  def createTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "createTarget",
      fieldType   = CreateTargetResultType[F],
      description = "Creates a new target according to the provided parameters.  Only one of sidereal or nonsidereal may be specified.".some,
      arguments   = List(ArgumentTargetCreate),
      resolve     = c => c.target(_.insert(c.arg(ArgumentTargetCreate)))
    )

  def CloneTargetResultType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], TargetModel.CloneResult] =
    ObjectType(
      name        = "CloneTargetResult",
      description = "The result of cloning a target, containing the original and new targets.",
      fieldsFn    = () => fields(

        Field(
          name        = "originalTarget",
          description = "The original unmodified target which was cloned".some,
          fieldType   = TargetType[F],
          resolve     = _.value.originalTarget
        ),

        Field(
          name        = "newTarget",
          description = "The new cloned (but possibly modified) target".some,
          fieldType   = TargetType[F],
          resolve     = _.value.newTarget
        )

      )
    )

  def cloneTarget[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "cloneTarget",
      fieldType   = CloneTargetResultType[F],
      description = "Makes a copy of an existing target, setting it to unobserved and to PRESENT.  If `REPLACE_IN` observationIds are specified in the input, the clone will replace the existing target in those observations".some,
      arguments   = List(ArgumentCloneTarget),
      resolve     = c => {
        val cloneInput = c.arg(ArgumentCloneTarget)
        c.unsafeToFuture(
          for {
            r <- c.ctx.odbRepo.target.clone(cloneInput)
            _ <- cloneInput.replaceWhereObservation.fold(Async[F].pure(())) { where =>
              c.ctx.odbRepo.observation.updateAsterism(
                ObservationModel.BulkEdit(
                  List(
                    EditAsterismPatchInput.delete(cloneInput.targetId),
                    EditAsterismPatchInput.add(r.newTarget.id)
                  ),
                  where.some,
                  None
                )
              ).void
            }
          } yield r
        )
      }
    )

  def updateTargets[F[_]: Dispatcher: Async: Logger]: Field[OdbCtx[F], Unit] =
    Field(
      name        = "updateTargets",
      fieldType   = UpdateResultType("UpdateTargetsResult", "targets", TargetType[F]),
      description = "Updates existing targets".some,
      arguments   = List(ArgumentUpdateTargets),
      resolve     = c => c.target(_.update(c.arg(ArgumentUpdateTargets)))
    )

  private def existenceEditField[F[_]: Dispatcher: Async: Logger](
    to: Existence
  ): Field[OdbCtx[F], Unit] = {
    val name = to.fold("delete", "undelete")
    val set  = TargetModel.PropertiesInput.Empty.copy(existence = to.assign)

    // Need a custom decoder because we don't want to look for a "patch" field.
    implicit val decoder: Decoder[TargetModel.UpdateInput] =
      (c: HCursor) => for {
        where <- c.downField("WHERE").as[Option[WhereTargetInput]]
        limit <- c.downField("LIMIT").as[Option[NonNegInt]]
      } yield TargetModel.UpdateInput(set, where, limit, includeDeleted = to.isPresent)

    val arg   =
      to.fold(InputObjectTypeTargetDelete, InputObjectTypeTargetUndelete)
         .argument(
           "input",
           s"Parameters used to select observations for $name"
         )

    Field(
      name        = s"${name}Targets",
      description = s"${name.capitalize}s all the targets identified by the `WHERE` field".some,
      fieldType   =  UpdateResultType(s"${name.capitalize}TargetsResult", "targets", TargetType[F]),
      arguments   = List(arg),
      resolve     = c => c.target(_.update(c.arg(arg)))
    )
  }

  def mutationFields[F[_]: Dispatcher: Async: Logger]: List[Field[OdbCtx[F], Unit]] =
    List(
      createTarget,
      cloneTarget,
      updateTargets,
      existenceEditField(Existence.Deleted),
      existenceEditField(Existence.Present)
    )

}
