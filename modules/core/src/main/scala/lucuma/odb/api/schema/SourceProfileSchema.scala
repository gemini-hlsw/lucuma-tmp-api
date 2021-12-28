// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.data.NonEmptyList
import cats.syntax.option._
import coulomb.Quantity
import coulomb.si.Kelvin
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.`enum`.{Band, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.{BrightnessUnits, BrightnessValue, Wavelength}
import lucuma.core.math.BrightnessUnits.{Brightness, FluxDensityContinuum, Integrated, LineFlux, Surface}
import lucuma.core.math.dimensional.{Qty, UnitType}
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.UnnormalizedSED.{BlackBody, CoolStarModel, Galaxy, HIIRegion, Planet, PlanetaryNebula, PowerLaw, Quasar, StellarLibrary, UserDefined}
import lucuma.core.model.{BandBrightness, EmissionLine, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.core.syntax.string._
import sangria.schema.{Field, _}
import sangria.validation.FloatCoercionViolation
import shapeless.tag.@@

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

  private def KelvinBasedSed[T: ClassTag](
    name:          String,
    description:   String,
    extractKelvin: T => Quantity[PosBigDecimal, Kelvin]
  ): ObjectType[Any, T] =
    ObjectType(
      name        = name,
      description = description,
      fieldsFn    = () => fields(
        Field(
          name        = "temperature",
          description = "Kelvin".some,
          fieldType   = PosBigDecimalType,
          resolve     = c => extractKelvin(c.value).value
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
    KelvinBasedSed[CoolStarModel](
      name        = "CoolStarModel",
      description = "Cool star model SED",
      _.temperature
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
    KelvinBasedSed[BlackBody](
      name        = "BlackBody",
      description = "Black body SED",
      _.temperature
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
      "²" -> "2",
      "/" -> "_PER_",
      "Å" -> "A",
      "µ" -> "U"
    )

  private def defineUnitsEnum[UG](
    name:        String,
    description: String,
    values:      NonEmptyList[UnitType @@ UG]
  ): EnumType[UnitType @@ UG] =
    EnumType(
      name        = name,
      description = description.some,
      values      = values.toList.map { gut =>
//        println(gut.name + " -> " + replacements.foldLeft(gut.name) { case (n, (a, b)) => n.replaceAll(a, b)}.toScreamingSnakeCase)
        EnumValue(
          name        = replacements
                          .foldLeft(gut.name.toUpperCase) { case (n, (a, b)) => n.replaceAll(a, b) }
                          .toScreamingSnakeCase,
          description = gut.abbv.some,
          value       = gut
        )
      }
    )

  val EnumTypeBrightnessIntegrated: EnumType[UnitType @@ Brightness[Integrated]] =
    defineUnitsEnum(
      "BrightnessIntegrated",
      "Brightness integrated units",
      BrightnessUnits.Brightness.Integrated.all
    )

  val EnumTypeBrightnessSurface: EnumType[UnitType @@ Brightness[Surface]] =
    defineUnitsEnum(
      "BrightnessSurface",
      "Brightness surface units",
      BrightnessUnits.Brightness.Surface.all
    )

  val EnumTypeLineFluxIntegrated: EnumType[UnitType @@ LineFlux[Integrated]] =
    defineUnitsEnum(
      "LineFluxIntegrated",
      "Line flux integrated units",
      BrightnessUnits.LineFlux.Integrated.all
    )

  val EnumTypeLineFluxSurface: EnumType[UnitType @@ LineFlux[Surface]] =
    defineUnitsEnum(
      "LineFluxSurface",
      "Line flux surface units",
      BrightnessUnits.LineFlux.Surface.all
    )

  val EnumTypeFluxDensityContinuumIntegrated: EnumType[UnitType @@ FluxDensityContinuum[Integrated]] =
    defineUnitsEnum(
      "FluxDensityContinuumIntegrated",
      "Flux density continuum integrated units",
      BrightnessUnits.FluxDensityContinuum.Integrated.all
    )

  val EnumTypeFluxDensityContinuumSurface: EnumType[UnitType @@ FluxDensityContinuum[Surface]] =
    defineUnitsEnum(
      "FluxDensityContinuumSurface",
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
    unitsType: EnumType[UnitType @@ UG]
  ): ObjectType[Any, Qty[N] @@ UG] =
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
          resolve   = c => shapeless.tag[UG](c.value.unit): UnitType @@ UG
        )
      )
    )

  private def BandBrightnessType[T](
    unitCategoryName: String,
    unitsType:        EnumType[UnitType @@ Brightness[T]]
  ): ObjectType[Any, BandBrightness[T]] =
    ObjectType(
      name     = s"BandBrightness$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "magnitude",
          fieldType   = GroupedUnitQtyType[BrightnessValue, Brightness[T]](
                   s"Magnitude$unitCategoryName",
                          BrightnessValueType,
                          unitsType
                        ),
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

  val BandBrightnessIntegrated: ObjectType[Any, BandBrightness[Integrated]] =
    BandBrightnessType[Integrated](
      "Integrated",
      EnumTypeBrightnessIntegrated
    )

  val BandBrightnessSurface: ObjectType[Any, BandBrightness[Surface]] =
    BandBrightnessType[Surface](
      "Surface",
      EnumTypeBrightnessSurface
    )

  private def BandNormalizedType[T](
    unitCategoryName:   String,
    bandBrightnessType: ObjectType[Any, BandBrightness[T]]
  ): ObjectType[Any, BandNormalized[T]] =
    ObjectType(
      name     = s"BandNormalized$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "sed",
          fieldType   = UnnormalizedSedType,
          description = "Un-normalized spectral energy distribution".some,
          resolve     = _.value.sed
        ),

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
    lineFluxType: ObjectType[Any, Qty[PosBigDecimal] @@ LineFlux[T]]
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

  val LineFluxIntegratedType: ObjectType[Any, Qty[PosBigDecimal] @@ LineFlux[Integrated]] =
    GroupedUnitQtyType[PosBigDecimal, LineFlux[Integrated]](
      "LineFluxIntegrated",
      PosBigDecimalType,
      EnumTypeLineFluxIntegrated
    )

  val LineFluxSurfaceType: ObjectType[Any, Qty[PosBigDecimal] @@ LineFlux[Surface]] =
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
    fdcType:          ObjectType[Any, Qty[PosBigDecimal] @@ FluxDensityContinuum[T]]
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

  val FluxDensityContinuumIntegratedType: ObjectType[Any, Qty[PosBigDecimal] @@ FluxDensityContinuum[Integrated]] =
    GroupedUnitQtyType[PosBigDecimal, FluxDensityContinuum[Integrated]](
      "FluxDensityContinuumIntegrated",
      PosBigDecimalType,
      EnumTypeFluxDensityContinuumIntegrated
    )

  val FluxDensityContinuumSurfaceType: ObjectType[Any, Qty[PosBigDecimal] @@ FluxDensityContinuum[Surface]] =
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
          resolve      = _.value.source.fwhm
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
