// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.data.NonEmptyList
import cats.syntax.option._
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.`enum`.{Band, CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.{BrightnessUnits, BrightnessValue, Wavelength}
import lucuma.core.math.BrightnessUnits.{Brightness, FluxDensityContinuum, Integrated, LineFlux, Surface}
import lucuma.core.math.dimensional.{Measure, Of, Units}
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.UnnormalizedSED.{BlackBody, CoolStarModel, Galaxy, HIIRegion, Planet, PlanetaryNebula, PowerLaw, Quasar, StellarLibrary, UserDefined}
import lucuma.core.model.{BandBrightness, EmissionLine, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.core.syntax.string._
import sangria.schema.{Field, _}
import sangria.validation.FloatCoercionViolation

import scala.reflect.ClassTag

object SourceProfileSchema {

  import AngleSchema.AngleType
  import GeneralSchema.PosBigDecimalType
  import syntax.`enum`._
  import WavelengthSchema.WavelengthType

  implicit val EnumTypeBand: EnumType[Band] =
    EnumType.fromEnumerated(
      "Band",
      "Magnitude bands"
    )

  implicit val EnumTypeStellarLibrarySpectrum: EnumType[StellarLibrarySpectrum] =
    EnumType.fromEnumerated(
      "StellarLibrarySpectrum",
      "Stellar library spectrum"
    )

  implicit val EnumTypeCoolStarTemperature: EnumType[CoolStarTemperature] =
    EnumType.fromEnumeratedMapping(
      "CoolStarTemperature",
      "Cool star temperature options",
      cst => s"T${cst.name}",
      cst => s"${cst.temperature.show}"
    )

  implicit val EnumTypeGalaxySpectrum: EnumType[GalaxySpectrum] =
    EnumType.fromEnumerated(
      "GalaxySpectrum",
      "Galaxy spectrum"
    )

  implicit val EnumTypePlanetSpectrum: EnumType[PlanetSpectrum] =
    EnumType.fromEnumerated(
      "PlanetSpectrum",
      "Planet spectrum"
    )

  implicit val EnumTypeQuasarSpectrum: EnumType[QuasarSpectrum] =
    EnumType.fromEnumerated(
      "QuasarSpectrum",
      "Quasar spectrum"
    )

  implicit val EnumTypeHiiRegionSpectrum: EnumType[HIIRegionSpectrum] =
    EnumType.fromEnumerated(
      "HiiRegionSpectrum",
      "HII Region spectrum"
    )

  implicit val EnumTypePlanetaryNebulaSpectrum: EnumType[PlanetaryNebulaSpectrum] =
    EnumType.fromEnumerated(
      "PlanetaryNebulaSpectrum",
      "Planetary nebula spectrum"
    )

  def UnnormalizedSedType: OutputType[UnnormalizedSED] =
    UnionType(
      name        = "UnnormalizedSed",
      description = "Un-normalized spectral energy distribution".some,
      types       = List(
        StellarLibraryType,
        CoolStarModelType,
        GalaxyType,
        PlanetType,
        QuasarType,
        HiiRegionType,
        PlanetaryNebulaType,
        PowerLawType,
        BlackBodyType,
        UserDefinedType
      )
    ).mapValue[UnnormalizedSED](identity)

  private def SpectrumEnumBasedSed[T: ClassTag, E](
    name:        String,
    enumType:    EnumType[E],
    extractEnum: T => E
  ): ObjectType[Any, T] =
    ObjectType(
      name        = name,
      fieldsFn    = () => fields(

        Field(
          name        = "spectrum",
          fieldType   = enumType,
          resolve     = c => extractEnum(c.value)
        )
      )
    )

  val StellarLibraryType: ObjectType[Any, StellarLibrary]  =
    SpectrumEnumBasedSed[StellarLibrary, StellarLibrarySpectrum](
      "StellarLibrary",
      EnumTypeStellarLibrarySpectrum,
      _.librarySpectrum
    )

  val CoolStarModelType: ObjectType[Any, CoolStarModel] =
    ObjectType(
      name        = "CoolStarModel",
      fieldsFn    = () => fields(

        Field(
          name      = "temperature",
          fieldType = EnumTypeCoolStarTemperature,
          resolve   = _.value.temperature
        )

      )
    )

  val GalaxyType: ObjectType[Any, Galaxy]  =
    SpectrumEnumBasedSed[Galaxy, GalaxySpectrum](
      "Galaxy",
      EnumTypeGalaxySpectrum,
      _.galaxySpectrum
    )

  val PlanetType: ObjectType[Any, Planet]  =
    SpectrumEnumBasedSed[Planet, PlanetSpectrum](
      "Planet",
      EnumTypePlanetSpectrum,
      _.planetSpectrum
    )

  val QuasarType: ObjectType[Any, Quasar]  =
    SpectrumEnumBasedSed[Quasar, QuasarSpectrum](
      "Quasar",
      EnumTypeQuasarSpectrum,
      _.quasarSpectrum
    )

  val HiiRegionType: ObjectType[Any, HIIRegion]  =
    SpectrumEnumBasedSed[HIIRegion, HIIRegionSpectrum](
      "HiiRegion",
      EnumTypeHiiRegionSpectrum,
      _.hiiRegionSpectrum
    )

  val PlanetaryNebulaType: ObjectType[Any, PlanetaryNebula] =
    SpectrumEnumBasedSed[PlanetaryNebula, PlanetaryNebulaSpectrum](
      "PlanetaryNebula",
      EnumTypePlanetaryNebulaSpectrum,
      _.planetaryNebulaSpectrum
    )

  val PowerLawType: ObjectType[Any, PowerLaw] =
    ObjectType(
      name        = "PowerLaw",
      description = "Power law SED",
      fieldsFn    = () => fields(
        Field(
          name        = "index",
          fieldType   = BigDecimalType,
          resolve     = _.value.index
        )
      )
    )

  val BlackBodyType: ObjectType[Any, BlackBody] =
    ObjectType(
      name        = "BlackBody",
      description = "Black body SED",
      fieldsFn    = () => fields(
        Field(
          name        = "temperature",
          description = "Kelvin".some,
          fieldType   = PosBigDecimalType,
          resolve     = _.value.temperature.value
        )
      )
    )

  val FluxDensityEntryType: ObjectType[Any, (Wavelength, PosBigDecimal)] =
    ObjectType(
      name        = "FluxDensityEntry",
      fieldsFn    = () => fields(

        Field(
          name      = "wavelength",
          fieldType = WavelengthType,
          resolve   = _.value._1
        ),

        Field(
          name      = "value",
          fieldType = PosBigDecimalType,
          resolve   = _.value._2
        )
      )
    )

  val UserDefinedType: ObjectType[Any, UserDefined] =
    ObjectType(
      name        = "UserDefined",
      description = "User defined SED",
      fieldsFn    = () => fields(
        Field(
          name        = "fluxDensities",
          fieldType   = ListType(FluxDensityEntryType),
          resolve     = _.value.fluxDensities.toNel.toList
        )
      )
    )

  // We are limited in the characters we can use for enum names.  These mappings
  // define how to map illegal characters into something valid for GraphQL.
  private val replacements: List[(String, String)] =
    List(
      " " -> "_",
      "²" -> "_SQUARED",
      "/" -> "_PER_",
      "Å" -> "A",
      "µ" -> "U"
    )

  def toGraphQLName(n: String): String =
    replacements
      .foldLeft(n) { case (n, (a, b)) => n.replaceAll(a, b) }
      .toScreamingSnakeCase

  private def defineUnitsEnum[UG](
    name:        String,
    description: String,
    values:      NonEmptyList[Units Of UG]
  ): EnumType[Units Of UG] =
    EnumType(
      name        = name,
      description = description.some,
      values      = values.toList.map { gut =>
        EnumValue(
          name        = toGraphQLName(gut.name),
          description = gut.abbv.some,
          value       = gut
        )
      }
    )

  val EnumTypeBrightnessIntegrated: EnumType[Units Of Brightness[Integrated]] =
    defineUnitsEnum(
      "BrightnessIntegratedUnits",
      "Brightness integrated units",
      BrightnessUnits.Brightness.Integrated.all
    )

  val EnumTypeBrightnessSurface: EnumType[Units Of Brightness[Surface]] =
    defineUnitsEnum(
      "BrightnessSurfaceUnits",
      "Brightness surface units",
      BrightnessUnits.Brightness.Surface.all
    )

  val EnumTypeLineFluxIntegrated: EnumType[Units Of LineFlux[Integrated]] =
    defineUnitsEnum(
      "LineFluxIntegratedUnits",
      "Line flux integrated units",
      BrightnessUnits.LineFlux.Integrated.all
    )

  val EnumTypeLineFluxSurface: EnumType[Units Of LineFlux[Surface]] =
    defineUnitsEnum(
      "LineFluxSurfaceUnits",
      "Line flux surface units",
      BrightnessUnits.LineFlux.Surface.all
    )

  val EnumTypeFluxDensityContinuumIntegrated: EnumType[Units Of FluxDensityContinuum[Integrated]] =
    defineUnitsEnum(
      "FluxDensityContinuumIntegratedUnits",
      "Flux density continuum integrated units",
      BrightnessUnits.FluxDensityContinuum.Integrated.all
    )

  val EnumTypeFluxDensityContinuumSurface: EnumType[Units Of FluxDensityContinuum[Surface]] =
    defineUnitsEnum(
      "FluxDensityContinuumSurfaceUnits",
      "Flux density continuum surface units",
      BrightnessUnits.FluxDensityContinuum.Surface.all
    )

  val BrightnessValueType: ScalarAlias[BrightnessValue, BigDecimal] =
    ScalarAlias(
      BigDecimalType,
      BrightnessValue.fromBigDecimal.reverseGet,
      bd => BrightnessValue.fromBigDecimal.getOption(bd).toRight(FloatCoercionViolation)
    )

  private def GroupedUnitQtyType[N, UG](
    name:      String,
    valueType: OutputType[N],
    unitsType: EnumType[Units Of UG]
  ): ObjectType[Any, Measure[N] Of UG] =
    ObjectType(
      name      = name,
      fieldsFn  = () => fields(

        Field(
          name      = "value",
          fieldType = valueType,
          resolve   = _.value.value
        ),

        Field(
          name      = "units",
          fieldType = unitsType,
          resolve   = c => Measure.unitsTagged[N, UG].get(c.value)
        )
      )
    )

  private def BandBrightnessType[T](
    unitCategoryName: String,
    qtyType:          ObjectType[Any, Measure[BrightnessValue] Of Brightness[T]]
  ): ObjectType[Any, BandBrightness[T]] =
    ObjectType(
      name     = s"BandBrightness$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "magnitude",
          fieldType   = qtyType,
          resolve     = _.value.quantity
        ),

        Field(
          name        = "band",
          fieldType   = EnumTypeBand,
          description = "Magnitude band".some,
          resolve     = _.value.band
        ),

        Field(
          name        = "error",
          fieldType   = OptionType(BigDecimalType),
          description = "Error, if any".some,
          resolve     = _.value.error.map(BrightnessValue.fromBigDecimal.reverseGet)
        )

      )
    )

  val BrightnessIntegrated: ObjectType[Any, Measure[BrightnessValue] Of Brightness[Integrated]] =
    GroupedUnitQtyType[BrightnessValue, Brightness[Integrated]](
      "BrightnessIntegrated",
      BrightnessValueType,
      EnumTypeBrightnessIntegrated
    )

  val BrightnessSurface: ObjectType[Any, Measure[BrightnessValue] Of Brightness[Surface]] =
    GroupedUnitQtyType[BrightnessValue, Brightness[Surface]](
      "BrightnessSurface",
      BrightnessValueType,
      EnumTypeBrightnessSurface
    )

  val BandBrightnessIntegrated: ObjectType[Any, BandBrightness[Integrated]] =
    BandBrightnessType[Integrated](
      "Integrated",
      BrightnessIntegrated
    )

  val BandBrightnessSurface: ObjectType[Any, BandBrightness[Surface]] =
    BandBrightnessType[Surface](
      "Surface",
      BrightnessSurface
    )

  val BandNormalizedType: InterfaceType[Any, BandNormalized[_]] =
    InterfaceType[Any, BandNormalized[_]](
      name        = "BandNormalized",
      description = "Band normalized common interface",
      fields[Any, BandNormalized[_]](

        Field(
          name        = "sed",
          fieldType   = UnnormalizedSedType,
          description = "Un-normalized spectral energy distribution".some,
          resolve     = _.value.sed
        )
      )
    )

  private def BandNormalizedType[T](
    unitCategoryName:   String,
    bandBrightnessType: ObjectType[Any, BandBrightness[T]]
  ): ObjectType[Any, BandNormalized[T]] =
    ObjectType(
      name       = s"BandNormalized$unitCategoryName",
      interfaces = List(PossibleInterface.apply[Any, BandNormalized[T]](BandNormalizedType)),
      fieldsFn   = () => fields(
        Field(
          name        = "brightnesses",
          fieldType   = ListType(bandBrightnessType),
          resolve     = _.value.brightnesses.toList.map(_._2)
        )
      )
    )

  val BandNormalizedIntegrated: ObjectType[Any, BandNormalized[Integrated]] =
    BandNormalizedType[Integrated](
      "Integrated",
      BandBrightnessIntegrated
    )

  val BandNormalizedSurface: ObjectType[Any, BandNormalized[Surface]] =
    BandNormalizedType[Surface](
      "Surface",
      BandBrightnessSurface
    )

  private def EmissionLineType[T](
    unitCategoryName: String,
    lineFluxType:     ObjectType[Any, Measure[PosBigDecimal] Of LineFlux[T]]
  ): ObjectType[Any, EmissionLine[T]] =
    ObjectType(
      name     = s"EmissionLine$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "wavelength",
          fieldType   = WavelengthType,
          resolve     = _.value.wavelength
        ),

        // TODO: units like -> "kilometersPerSecond": 1  ?
        Field(
          name        = "lineWidth",
          fieldType   = PosBigDecimalType,
          description = "km/s".some,
          resolve     = _.value.lineWidth.value
        ),

        Field(
          name        = "lineFlux",
          fieldType   = lineFluxType,
          resolve     = _.value.lineFlux
        )
      )
    )

  val LineFluxIntegratedType: ObjectType[Any, Measure[PosBigDecimal] Of LineFlux[Integrated]] =
    GroupedUnitQtyType[PosBigDecimal, LineFlux[Integrated]](
      "LineFluxIntegrated",
      PosBigDecimalType,
      EnumTypeLineFluxIntegrated
    )

  val LineFluxSurfaceType: ObjectType[Any, Measure[PosBigDecimal] Of LineFlux[Surface]] =
    GroupedUnitQtyType[PosBigDecimal, LineFlux[Surface]](
      "LineFluxSurface",
      PosBigDecimalType,
      EnumTypeLineFluxSurface
    )

  val EmissionLineIntegrated: ObjectType[Any, EmissionLine[Integrated]] =
    EmissionLineType[Integrated](
      "Integrated",
      LineFluxIntegratedType
    )

  val EmissionLineSurface: ObjectType[Any, EmissionLine[Surface]] =
    EmissionLineType[Surface](
      "Surface",
      LineFluxSurfaceType
    )

  private def EmissionLinesType[T](
    unitCategoryName: String,
    lineType:         ObjectType[Any, EmissionLine[T]],
    fdcType:          ObjectType[Any, Measure[PosBigDecimal] Of FluxDensityContinuum[T]]
  ): ObjectType[Any, EmissionLines[T]] =
    ObjectType(
      name     = s"EmissionLines$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name      = "lines",
          fieldType = ListType(lineType),
          resolve   = _.value.lines.values.toList
        ),

        Field(
          name      = "fluxDensityContinuum",
          fieldType = fdcType,
          resolve   = _.value.fluxDensityContinuum
        )
      )
    )

  val FluxDensityContinuumIntegratedType: ObjectType[Any, Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]] =
    GroupedUnitQtyType[PosBigDecimal, FluxDensityContinuum[Integrated]](
      "FluxDensityContinuumIntegrated",
      PosBigDecimalType,
      EnumTypeFluxDensityContinuumIntegrated
    )

  val FluxDensityContinuumSurfaceType: ObjectType[Any, Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]] =
    GroupedUnitQtyType[PosBigDecimal, FluxDensityContinuum[Surface]](
      "FluxDensityContinuumSurface",
      PosBigDecimalType,
      EnumTypeFluxDensityContinuumSurface
    )

  val EmissionLinesIntegrated: ObjectType[Any, EmissionLines[Integrated]] =
    EmissionLinesType[Integrated](
      "Integrated",
      EmissionLineIntegrated,
      FluxDensityContinuumIntegratedType
    )

  val EmissionLinesSurface: ObjectType[Any, EmissionLines[Surface]] =
    EmissionLinesType[Surface](
      "Surface",
      EmissionLineSurface,
      FluxDensityContinuumSurfaceType
    )

  def SpectralDefinitionType[T](
    unitCategoryName: String,
    bandNormalizedType: ObjectType[Any, BandNormalized[T]],
    emissionLinesType:  ObjectType[Any, EmissionLines[T]]
  ): OutputType[SpectralDefinition[T]] =
    UnionType.apply(
      name         = s"SpectralDefinition$unitCategoryName",
      description  = s"Spectral definition ${unitCategoryName.toLowerCase}".some,
      types        = List(
        bandNormalizedType,
        emissionLinesType,
      )
    ).mapValue[SpectralDefinition[T]](identity)

  val SpectralDefinitionIntegrated: OutputType[SpectralDefinition[Integrated]] =
    SpectralDefinitionType[Integrated](
      "Integrated",
      BandNormalizedIntegrated,
      EmissionLinesIntegrated
    )

  val SpectralDefinitionSurface: OutputType[SpectralDefinition[Surface]] =
    SpectralDefinitionType[Surface](
      "Surface",
      BandNormalizedSurface,
      EmissionLinesSurface
    )

  val PointType: ObjectType[Any, SourceProfile.Point] =
    ObjectType(
      name     = "PointSource",
      fieldsFn = () => fields(
         Field(
           name        = "spectralDefinition",
           description = "Point source".some,
           fieldType   = SpectralDefinitionIntegrated,
           resolve     = _.value.spectralDefinition
         )
      )
    )

  val UniformType: ObjectType[Any, SourceProfile.Uniform] =
    ObjectType(
      name     = "UniformSource",
      fieldsFn = () => fields(
        Field(
          name         = "spectralDefinition",
          description  = "Uniform source".some,
          fieldType    = SpectralDefinitionSurface,
          resolve      = _.value.spectralDefinition
        )
      )
    )

  val GaussianType: ObjectType[Any, SourceProfile.Gaussian] =
    ObjectType(
      name     = "GaussianSource",
      fieldsFn = () => fields(

        Field(
          name         = "fwhm",
          description  = "full width at half maximum".some,
          fieldType    = AngleType,
          resolve      = _.value.fwhm
        ),

        Field(
          name         = "spectralDefinition",
          description  = "Gaussian source".some,
          fieldType    = SpectralDefinitionIntegrated,
          resolve      = _.value.spectralDefinition
        )
      )
    )

  val SourceProfileType: OutputType[SourceProfile] =
    UnionType(
      name        = "SourceProfile",
      description = "source profile".some,
      types       = List(
        PointType,
        UniformType,
        GaussianType
      )
    ).mapValue[SourceProfile](identity)

}
