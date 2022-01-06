// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.either._
import cats.syntax.option._
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.`enum`.{Band, CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.{BrightnessValue, Wavelength}
import lucuma.core.math.BrightnessUnits.{Brightness, FluxDensityContinuum, Integrated, LineFlux, Surface}
import lucuma.core.math.dimensional.{Measure, Of, Units}
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.UnnormalizedSED.{BlackBody, CoolStarModel, Galaxy, HIIRegion, Planet, PlanetaryNebula, PowerLaw, Quasar, StellarLibrary, UserDefined}
import lucuma.core.model.{BandBrightness, EmissionLine, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.targetModel.SourceProfileModel.{CreateBandBrightnessInput, CreateBandNormalizedInput, CreateEmissionLineInput, CreateEmissionLinesInput, CreateGaussianInput, CreateMeasureInput, CreateSpectralDefinitionInput, FluxDensityInput, UnnormalizedSedInput}
import sangria.schema.{Field, _}
import sangria.macros.derive._
import sangria.marshalling.circe._

import scala.reflect.ClassTag

object SourceProfileSchema {

  import AngleSchema.{AngleType, InputObjectAngle}
  import GeneralSchema.PosBigDecimalType
  import syntax.`enum`._
  import syntax.inputobjecttype._
  import WavelengthSchema._

  implicit val EnumTypeBand: EnumType[Band] =
    EnumType.fromEnumerated(
      "Band",
      "Brightness bands"
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

//  implicit def DecoderUnitsOfUG[UG](implicit ev: Enumerated[Units Of UG]): Decoder[Units Of UG] =
//    (c: HCursor) =>
//      c.as[String].flatMap { s =>
//        ev.all
//          .find(e => e.serialized.toScreamingSnakeCase === s)
//          .toRight(DecodingFailure(s"Could not parse enumerated type value '$s", Nil))
//      }

  private def defineUnitsEnum[UG](
    name:        String,
    description: String,
  )(implicit ev: Enumerated[Units Of UG]): EnumType[Units Of UG] =
    EnumType.fromEnumeratedMapping[Units Of UG](
      name,
      description,
      _.serialized,
      _.abbv
    )

  val EnumTypeBrightnessIntegrated: EnumType[Units Of Brightness[Integrated]] =
    defineUnitsEnum(
      "BrightnessIntegratedUnits",
      "Brightness integrated units"
    )

  val EnumTypeBrightnessSurface: EnumType[Units Of Brightness[Surface]] =
    defineUnitsEnum(
      "BrightnessSurfaceUnits",
      "Brightness surface units"
    )

  val EnumTypeLineFluxIntegrated: EnumType[Units Of LineFlux[Integrated]] =
    defineUnitsEnum(
      "LineFluxIntegratedUnits",
      "Line flux integrated units"
    )

  val EnumTypeLineFluxSurface: EnumType[Units Of LineFlux[Surface]] =
    defineUnitsEnum(
      "LineFluxSurfaceUnits",
      "Line flux surface units"
    )

  val EnumTypeFluxDensityContinuumIntegrated: EnumType[Units Of FluxDensityContinuum[Integrated]] =
    defineUnitsEnum(
      "FluxDensityContinuumIntegratedUnits",
      "Flux density continuum integrated units"
    )

  val EnumTypeFluxDensityContinuumSurface: EnumType[Units Of FluxDensityContinuum[Surface]] =
    defineUnitsEnum(
      "FluxDensityContinuumSurfaceUnits",
      "Flux density continuum surface units"
    )

  val BrightnessValueType: ScalarAlias[BrightnessValue, BigDecimal] =
    ScalarAlias(
      BigDecimalType,
      BrightnessValue.fromBigDecimal.reverseGet,
      bd => BrightnessValue.fromBigDecimal.get(bd).asRight
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
      name        = "PointSource",
      description = "Point source",
      fieldsFn = () => fields(
         Field(
           name        = "spectralDefinition",
           description = "Integrated units".some,
           fieldType   = SpectralDefinitionIntegrated,
           resolve     = _.value.spectralDefinition
         )
      )
    )

  val UniformType: ObjectType[Any, SourceProfile.Uniform] =
    ObjectType(
      name        = "UniformSource",
      description = "Uniform source",
      fieldsFn = () => fields(
        Field(
          name         = "spectralDefinition",
          description  = "Surface units".some,
          fieldType    = SpectralDefinitionSurface,
          resolve      = _.value.spectralDefinition
        )
      )
    )

  val GaussianType: ObjectType[Any, SourceProfile.Gaussian] =
    ObjectType(
      name        = "GaussianSource",
      description = "Gaussian source",
      fieldsFn = () => fields(

        Field(
          name         = "fwhm",
          description  = "full width at half maximum".some,
          fieldType    = AngleType,
          resolve      = _.value.fwhm
        ),

        Field(
          name         = "spectralDefinition",
          description  = "Integrated units".some,
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

  // Inputs

  implicit val InputObjectFluxDensity: InputObjectType[FluxDensityInput] =
    deriveInputObjectType[FluxDensityInput](
      InputObjectTypeName("FluxDensity"),
      InputObjectTypeDescription("Flux density entry")
    )

  implicit val InputObjectUnnormalizedSed: InputObjectType[UnnormalizedSedInput] =
    deriveInputObjectType[UnnormalizedSedInput](
      InputObjectTypeName("UnnormalizedSedInput"),
      InputObjectTypeDescription("Un-normalized SED input parameters.  Define one value only.")
    )

  private def createBrightnessInputObjectType[T](
    groupName: String,
    e: EnumType[Units Of Brightness[T]]
  ): InputObjectType[CreateMeasureInput[BigDecimal, Brightness[T]]] = {
    implicit val unitsInput: InputType[Units Of Brightness[T]] = e

    deriveInputObjectType[CreateMeasureInput[BigDecimal, Brightness[T]]](
      InputObjectTypeName(s"CreateBrightness${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create a brightness value with $groupName units")
    )
  }

  implicit val InputObjectCreateBrightnessIntegrated: InputObjectType[CreateMeasureInput[BigDecimal, Brightness[Integrated]]] =
    createBrightnessInputObjectType("integrated", EnumTypeBrightnessIntegrated)

  implicit val InputObjectCreateBrightnessSurface: InputObjectType[CreateMeasureInput[BigDecimal, Brightness[Surface]]] =
    createBrightnessInputObjectType("surface", EnumTypeBrightnessSurface)

  private def createBandBrightnessInputObjectType[T](
    groupName: String
  )(implicit ev: InputType[CreateMeasureInput[BigDecimal, Brightness[T]]]): InputObjectType[CreateBandBrightnessInput[T]] =
    deriveInputObjectType[CreateBandBrightnessInput[T]](
      InputObjectTypeName(s"CreateBandBrightness${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create a band brightness value with $groupName magnitude units")
    )

  implicit val InputObjectCreateBandBrightnessIntegrated: InputObjectType[CreateBandBrightnessInput[Integrated]] =
    createBandBrightnessInputObjectType[Integrated]("integrated")

  implicit val InputObjectCreateBandBrightnessSurface: InputObjectType[CreateBandBrightnessInput[Surface]] =
    createBandBrightnessInputObjectType[Surface]("surface")

  private def createBandNormalizedInputObjectType[T](
    groupName: String
  )(implicit ev: InputType[CreateBandBrightnessInput[T]]): InputObjectType[CreateBandNormalizedInput[T]] =
    deriveInputObjectType(
      InputObjectTypeName(s"CreateBandNormalized${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create a band normalized value with $groupName magnitude units")
    )

  implicit val InputObjectCreateBandNormalizedIntegrated: InputObjectType[CreateBandNormalizedInput[Integrated]] =
    createBandNormalizedInputObjectType[Integrated]("integrated")

  implicit val InputObjectCreateBandNormalizedSurface: InputObjectType[CreateBandNormalizedInput[Surface]] =
    createBandNormalizedInputObjectType[Surface]("surface")

  private def createLineFluxInputObjectType[T](
    groupName: String,
    e: EnumType[Units Of LineFlux[T]]
  ): InputObjectType[CreateMeasureInput[PosBigDecimal, LineFlux[T]]] = {
    implicit val unitsInput: InputType[Units Of LineFlux[T]] = e

    deriveInputObjectType[CreateMeasureInput[PosBigDecimal, LineFlux[T]]](
      InputObjectTypeName(s"CreateLineFlux${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create a line flux value with $groupName units")
    )
  }

  implicit val InputObjectCreateLineFluxIntegrated: InputObjectType[CreateMeasureInput[PosBigDecimal, LineFlux[Integrated]]] =
    createLineFluxInputObjectType("integrated", EnumTypeLineFluxIntegrated)

  implicit val InputObjectCreateLineFluxSurface: InputObjectType[CreateMeasureInput[PosBigDecimal, LineFlux[Surface]]] =
    createLineFluxInputObjectType("surface", EnumTypeLineFluxSurface)

  private def createEmissionLineInputObjectType[T](
    groupName: String
  )(implicit ev: InputType[CreateMeasureInput[PosBigDecimal, LineFlux[T]]]): InputObjectType[CreateEmissionLineInput[T]] =
    deriveInputObjectType(
      InputObjectTypeName(s"CreateEmissionLine${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create an emission line with $groupName line flux units")
    )

  implicit val InputObjectEmissionLineIntegrated: InputObjectType[CreateEmissionLineInput[Integrated]] =
    createEmissionLineInputObjectType[Integrated]("integrated")

  implicit val InputObjectEmissionLineSurface: InputObjectType[CreateEmissionLineInput[Surface]] =
    createEmissionLineInputObjectType[Surface]("surface")

  private def createFluxDensityContinuumInputObjectType[T](
    groupName: String,
    e: EnumType[Units Of FluxDensityContinuum[T]]
  ): InputObjectType[CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]] = {
    implicit val unitsInput: InputType[Units Of FluxDensityContinuum[T]] = e

    deriveInputObjectType[CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]](
      InputObjectTypeName(s"CreateFluxDensityContinuum${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create a flux density continuum value with $groupName units")
    )
  }

  implicit val InputObjectCreateFluxDensityContinuumIntegrated: InputObjectType[CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[Integrated]]] =
    createFluxDensityContinuumInputObjectType("integrated", EnumTypeFluxDensityContinuumIntegrated)

  implicit val InputObjectCreateFluxDensityContinuumSurface: InputObjectType[CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[Surface]]] =
    createFluxDensityContinuumInputObjectType("surface", EnumTypeFluxDensityContinuumSurface)

  private def createEmissionLinesInputObjectType[T](
    groupName: String
  )(implicit ev0: InputType[CreateEmissionLineInput[T]], ev1: InputType[CreateMeasureInput[PosBigDecimal, FluxDensityContinuum[T]]]): InputObjectType[CreateEmissionLinesInput[T]] =
    deriveInputObjectType(
      InputObjectTypeName(s"CreateEmissionLines${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create an emission lines with $groupName line flux and flux density continuum units")
    )

  implicit val InputObjectCreateEmissionLinesIntegrated: InputObjectType[CreateEmissionLinesInput[Integrated]] =
    createEmissionLinesInputObjectType("integrated")

  implicit val InputObjectCreateEmissionLinesSurface: InputObjectType[CreateEmissionLinesInput[Surface]] =
    createEmissionLinesInputObjectType("surface")

  private def createSpectralDefinitionInputObjectType[T](
    groupName: String
  )(
    implicit ev0: InputType[CreateBandNormalizedInput[T]],
             ev1: InputType[CreateEmissionLinesInput[T]]
  ): InputObjectType[CreateSpectralDefinitionInput[T]] =
    deriveInputObjectType(
      InputObjectTypeName(s"CreateSpectralDefinition${groupName.capitalize}"),
      InputObjectTypeDescription(s"Create a spectral definition with $groupName units")
    )

  implicit val InputObjectCreateSpectralDefinitionIntegrated: InputObjectType[CreateSpectralDefinitionInput[Integrated]] =
    createSpectralDefinitionInputObjectType("integrated")

  implicit val InputObjectCreateSpectralDefinitionSurface: InputObjectType[CreateSpectralDefinitionInput[Surface]] =
    createSpectralDefinitionInputObjectType("surface")

  implicit val InputObjectCreateGaussianInput: InputObjectType[CreateGaussianInput] =
    deriveInputObjectType(
      InputObjectTypeName("CreateGaussian"),
      InputObjectTypeDescription("Create a gaussian source definition")
    )

  // Arguments

  // TODO: Remove

  val ArgumentCreateBandNormalizedIntegrated: Argument[CreateBandNormalizedInput[Integrated]] =
    InputObjectCreateBandNormalizedIntegrated.argument("mag", "magnitude")
}
