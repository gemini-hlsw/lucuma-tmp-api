// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.enums.{Band, CoolStarTemperature, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.Wavelength
import lucuma.core.math.BrightnessUnits.{Brightness, FluxDensityContinuum, Integrated, LineFlux, Surface}
import lucuma.core.math.dimensional.{Measure, Of, Units}
import lucuma.core.model.SpectralDefinition.{BandNormalized, EmissionLines}
import lucuma.core.model.{SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.targetModel.SourceProfileModel.{BandBrightnessInput, BandBrightnessPair, BandNormalizedInput, EmissionLineInput, MeasureInput, EmissionLinesInput, FluxDensityInput, GaussianInput, SourceProfileInput, SpectralDefinitionInput, UnnormalizedSedInput, WavelengthEmissionLinePair}
import lucuma.odb.api.schema.syntax.inputtype._
import monocle.Prism
import sangria.schema.{Field, _}
import sangria.macros.derive._


object SourceProfileSchema {

  import AngleSchema.{AngleType, InputObjectAngle}
  import RefinedSchema.{PosBigDecimalType, PosIntType}
  import syntax.`enum`._
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

  def UnnormalizedSedType: ObjectType[Any, UnnormalizedSED] = {

    def enumField[A, E](
      n: String,
      f: A => E,
      p: Prism[UnnormalizedSED, A]
    )(implicit ev: OutputType[E]): Field[Any, UnnormalizedSED] =
      Field(
        name       = n,
        fieldType  = OptionType(ev),
        resolve    = c => p.getOption(c.value).map(f)
      )

    import UnnormalizedSED._

    ObjectType(
      name        = "UnnormalizedSed",
      description = "Un-normalized spectral energy distribution.  Exactly one of the definitions will be non-null.",
      fieldsFn    = () => fields(

        enumField[StellarLibrary, StellarLibrarySpectrum](
          "stellarLibrary",
          _.librarySpectrum,
          stellarLibrary
        ),

        enumField[CoolStarModel, CoolStarTemperature](
          "coolStar",
          _.temperature,
          coolStarModel
        ),

        enumField[Galaxy, GalaxySpectrum](
          "galaxy",
          _.galaxySpectrum,
          galaxy
        ),

        enumField[Planet, PlanetSpectrum](
          "planet",
          _.planetSpectrum,
          planet
        ),

        enumField[Quasar, QuasarSpectrum](
          "quasar",
          _.quasarSpectrum,
          quasar
        ),

        enumField[HIIRegion, HIIRegionSpectrum](
          "hiiRegion",
          _.hiiRegionSpectrum,
          hiiRegion
        ),

        enumField[PlanetaryNebula, PlanetaryNebulaSpectrum](
          "planetaryNebula",
          _.planetaryNebulaSpectrum,
          planetaryNebula
        ),

        Field(
          name      = "powerLaw",
          fieldType = OptionType(BigDecimalType),
          resolve   = c => powerLaw.getOption(c.value).map(_.index)
        ),

        Field(
          name      = "blackBodyTempK",
          fieldType = OptionType(PosIntType),
          resolve   = c => blackBody.getOption(c.value).map(_.temperature.value)
        ),

        Field(
          name      = "fluxDensities",
          fieldType = OptionType(ListType(FluxDensityEntryType)),
          resolve   = c => userDefined.getOption(c.value).map(_.fluxDensities.toNel.toList)
        )
      )
    )
  }

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
          name      = "density",
          fieldType = PosBigDecimalType,
          resolve   = _.value._2
        )
      )
    )

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

  implicit val EnumTypeBrightnessIntegrated: EnumType[Units Of Brightness[Integrated]] =
    defineUnitsEnum(
      "BrightnessIntegratedUnits",
      "Brightness integrated units"
    )

  implicit val EnumTypeBrightnessSurface: EnumType[Units Of Brightness[Surface]] =
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
    unitsType:        EnumType[Units Of Brightness[T]]
  ): ObjectType[Any, BandBrightnessPair[T]] =
    ObjectType(
      name     = s"BandBrightness$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name        = "band",
          fieldType   = EnumTypeBand,
          description = "Magnitude band".some,
          resolve     = _.value.band
        ),

        Field(
          name        = "value",
          fieldType   = BigDecimalType,
          resolve     = _.value.measure.value
        ),

        Field(
          name        = "units",
          fieldType   = unitsType,
          resolve     = c => Measure.unitsTagged[BigDecimal, Brightness[T]].get(c.value.measure)
        ),

        Field(
          name        = "error",
          fieldType   = OptionType(BigDecimalType),
          description = "Error, if any".some,
          resolve     = _.value.measure.error
        )

      )
    )

  val BandBrightnessIntegrated: ObjectType[Any, BandBrightnessPair[Integrated]] =
    BandBrightnessType[Integrated](
      "Integrated",
      EnumTypeBrightnessIntegrated
    )

  val BandBrightnessSurface: ObjectType[Any, BandBrightnessPair[Surface]] =
    BandBrightnessType[Surface](
      "Surface",
      EnumTypeBrightnessSurface
    )

  val BandNormalizedType: InterfaceType[Any, BandNormalized[_]] =
    InterfaceType[Any, BandNormalized[_]](
      name        = "BandNormalized",
      description = "Band normalized common interface",
      fields[Any, BandNormalized[_]](

        Field(
          name        = "sed",
          fieldType   = OptionType(UnnormalizedSedType),
          description = "Un-normalized spectral energy distribution".some,
          resolve     = _.value.sed
        )
      )
    )

  private def BandNormalizedType[T](
    unitCategoryName:   String,
    bandBrightnessType: ObjectType[Any, BandBrightnessPair[T]]
  ): ObjectType[Any, BandNormalized[T]] =
    ObjectType(
      name       = s"BandNormalized$unitCategoryName",
      interfaces = List(PossibleInterface.apply[Any, BandNormalized[T]](BandNormalizedType)),
      fieldsFn   = () => fields(
        Field(
          name        = "brightnesses",
          fieldType   = ListType(bandBrightnessType),
          resolve     = _.value.brightnesses.toList.map { case (band, measure) => BandBrightnessPair[T](band, measure) }
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

  private def WavelengthEmissionLineType[T](
    unitCategoryName: String,
    lineFluxType:     ObjectType[Any, Measure[PosBigDecimal] Of LineFlux[T]]
  ): ObjectType[Any, WavelengthEmissionLinePair[T]] =
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
          resolve     = _.value.line.lineWidth.value
        ),

        Field(
          name        = "lineFlux",
          fieldType   = lineFluxType,
          resolve     = _.value.line.lineFlux
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

  val WavelengthEmissionLinePairIntegrated: ObjectType[Any, WavelengthEmissionLinePair[Integrated]] =
    WavelengthEmissionLineType[Integrated](
      "Integrated",
      LineFluxIntegratedType
    )

  val WavelengthEmissionLinePairSurface: ObjectType[Any, WavelengthEmissionLinePair[Surface]] =
    WavelengthEmissionLineType[Surface](
      "Surface",
      LineFluxSurfaceType
    )

  private def EmissionLinesType[T](
    unitCategoryName: String,
    lineType:         ObjectType[Any, WavelengthEmissionLinePair[T]],
    fdcType:          ObjectType[Any, Measure[PosBigDecimal] Of FluxDensityContinuum[T]]
  ): ObjectType[Any, EmissionLines[T]] =
    ObjectType(
      name     = s"EmissionLines$unitCategoryName",
      fieldsFn = () => fields(

        Field(
          name      = "lines",
          fieldType = ListType(lineType),
          resolve   = _.value.lines.map { case (wave, line) => WavelengthEmissionLinePair(wave, line) }.toList
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
      WavelengthEmissionLinePairIntegrated,
      FluxDensityContinuumIntegratedType
    )

  val EmissionLinesSurface: ObjectType[Any, EmissionLines[Surface]] =
    EmissionLinesType[Surface](
      "Surface",
      WavelengthEmissionLinePairSurface,
      FluxDensityContinuumSurfaceType
    )

  private def spectralDefinitionFields[A, T](
    get:                A => SpectralDefinition[T],
    bandNormalizedType: OutputType[BandNormalized[T]],
    emissionLinesType:  OutputType[EmissionLines[T]]
  ): List[Field[Any, A]] =
    List(
      Field(
        name        = "bandNormalized",
        description = "Band normalized spectral definition".some,
        fieldType   = OptionType(bandNormalizedType),
        resolve     = c => SpectralDefinition.bandNormalized.getOption(get(c.value))
      ),

      Field(
        name        = "emissionLines",
        description = "Emission lines spectral definition".some,
        fieldType   = OptionType(emissionLinesType),
        resolve     = c => SpectralDefinition.emissionLines.getOption(get(c.value))
      )
    )

  def SpectralDefinitionType[T](
    unitCategoryName: String,
    bandNormalizedType: OutputType[BandNormalized[T]],
    emissionLinesType:  OutputType[EmissionLines[T]]
  ): ObjectType[Any, SpectralDefinition[T]] =
    ObjectType(
      name         = s"SpectralDefinition${unitCategoryName.capitalize}",
      description  = s"Spectral definition ${unitCategoryName.toLowerCase}.  Exactly one of the fields will be defined.",
      fieldsFn     = () => spectralDefinitionFields[SpectralDefinition[T], T](identity, bandNormalizedType, emissionLinesType)
    )

  val SpectralDefinitionIntegrated: OutputType[SpectralDefinition[Integrated]] =
    SpectralDefinitionType[Integrated](
      "integrated",
      BandNormalizedIntegrated,
      EmissionLinesIntegrated
    )

  val SpectralDefinitionSurface: OutputType[SpectralDefinition[Surface]] =
    SpectralDefinitionType[Surface](
      "surface",
      BandNormalizedSurface,
      EmissionLinesSurface
    )

  val GaussianType: ObjectType[Any, SourceProfile.Gaussian] =
    ObjectType[Any, SourceProfile.Gaussian](
      name        = "GaussianSource",
      description = "Gaussian source, one of bandNormalized and emissionLines will be defined.",
      fieldsFn    = () =>

        Field(
          name         = "fwhm",
          description  = "full width at half maximum".some,
          fieldType    = AngleType,
          resolve      = (c: Context[Any, SourceProfile.Gaussian]) => c.value.fwhm
        ) :: spectralDefinitionFields[SourceProfile.Gaussian, Integrated](
               _.spectralDefinition,
               BandNormalizedIntegrated,
               EmissionLinesIntegrated
             )
    )

  val SourceProfileType: ObjectType[Any, SourceProfile] =
    ObjectType(
      name        = "SourceProfile",
      description = "Source profile, exactly one of the fields will be defined",
      fieldsFn    = () => fields(

        Field(
          name        = "point",
          description = "point source, integrated units".some,
          fieldType   = OptionType(SpectralDefinitionIntegrated),
          resolve     = c => SourceProfile.point.getOption(c.value).map(_.spectralDefinition)
        ),

        Field(
          name        = "uniform",
          description = "uniform source, surface units".some,
          fieldType   = OptionType(SpectralDefinitionSurface),
          resolve     = c => SourceProfile.uniform.getOption(c.value).map(_.spectralDefinition)
        ),

        Field(
          name        = "gaussian",
          description = "gaussian source, integrated units".some,
          fieldType   = OptionType(GaussianType),
          resolve     = c => SourceProfile.gaussian.getOption(c.value)
        )

      )
    )

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

  private def createBandBrightnessInputObjectType[T](
    groupName: String
  )(implicit ev: InputType[Units Of Brightness[T]]): InputObjectType[BandBrightnessInput[T]] = {
    val typeName = s"BandBrightness${groupName.capitalize}"

    InputObjectType[BandBrightnessInput[T]](
      s"${typeName}Input",
      s"""Create or edit a band brightness value with $groupName magnitude units.  When creating a new value, all fields except "error" are required.""",
      List(
        InputField("band", EnumTypeBand),
        BigDecimalType.createRequiredEditOptional("value", typeName),
        ev.createRequiredEditOptional("units", typeName),
        BigDecimalType.optionField("error", "Error values are optional")
      )
    )
  }

  implicit val InputObjectBandBrightnessIntegrated: InputObjectType[BandBrightnessInput[Integrated]] =
    createBandBrightnessInputObjectType[Integrated]("integrated")

  implicit val InputObjectBandBrightnessSurface: InputObjectType[BandBrightnessInput[Surface]] =
    createBandBrightnessInputObjectType[Surface]("surface")

  private def createBandNormalizedInputObjectType[T](
    groupName: String
  )(implicit ev: InputType[BandBrightnessInput[T]]): InputObjectType[BandNormalizedInput[T]] =
    InputObjectType[BandNormalizedInput[T]](
      s"BandNormalized${groupName.capitalize}Input",
      s"""Create or edit a band normalized value with $groupName magnitude units.  Specify "brightnesses" and an optional "sed" parameter when creating a new BandNormalized${groupName.capitalize}.""",
      List(
        InputObjectUnnormalizedSed.createRequiredEditOptional("sed", s"BandNormalized${groupName.capitalize}"),
        ListInputType(ev).createRequiredEditOptional("brightnesses", s"BandNormalized${groupName.capitalize}"),
      )
    )

  implicit val InputObjectBandNormalizedIntegrated: InputObjectType[BandNormalizedInput[Integrated]] =
    createBandNormalizedInputObjectType[Integrated]("integrated")

  implicit val InputObjectBandNormalizedSurface: InputObjectType[BandNormalizedInput[Surface]] =
    createBandNormalizedInputObjectType[Surface]("surface")

  private def createLineFluxInputObjectType[T](
    groupName: String,
    e: EnumType[Units Of LineFlux[T]]
  ): InputObjectType[MeasureInput[PosBigDecimal, LineFlux[T]]] = {
    implicit val unitsInput: InputType[Units Of LineFlux[T]] = e

    deriveInputObjectType[MeasureInput[PosBigDecimal, LineFlux[T]]](
      InputObjectTypeName(s"LineFlux${groupName.capitalize}Input"),
      InputObjectTypeDescription(s"A line flux value with $groupName units")
    )
  }

  implicit val InputObjectLineFluxIntegrated: InputObjectType[MeasureInput[PosBigDecimal, LineFlux[Integrated]]] =
    createLineFluxInputObjectType("integrated", EnumTypeLineFluxIntegrated)

  implicit val InputObjectLineFluxSurface: InputObjectType[MeasureInput[PosBigDecimal, LineFlux[Surface]]] =
    createLineFluxInputObjectType("surface", EnumTypeLineFluxSurface)

  private def createEmissionLineInputObjectType[T](
    groupName: String
  )(implicit ev: InputType[MeasureInput[PosBigDecimal, LineFlux[T]]]): InputObjectType[EmissionLineInput[T]] = {
    val typeName = s"EmissionLine${groupName.capitalize}"

    InputObjectType[EmissionLineInput[T]](
      s"${typeName}Input",
      s"Create or edit an emission line with $groupName line flux units.  When creating a new value, all fields are required.",
      List(
        InputField("wavelength", WavelengthSchema.InputWavelength),
        PosBigDecimalType.createRequiredEditOptional("lineWidth", typeName),
        ev.createRequiredEditOptional("lineFlux", typeName)
      )
    )
  }

  implicit val InputObjectEmissionLineIntegrated: InputObjectType[EmissionLineInput[Integrated]] =
    createEmissionLineInputObjectType[Integrated]("integrated")

  implicit val InputObjectEmissionLineSurface: InputObjectType[EmissionLineInput[Surface]] =
    createEmissionLineInputObjectType[Surface]("surface")

  private def createFluxDensityContinuumInputObjectType[T](
    groupName: String,
    e: EnumType[Units Of FluxDensityContinuum[T]]
  ): InputObjectType[MeasureInput[PosBigDecimal, FluxDensityContinuum[T]]] = {
    implicit val unitsInput: InputType[Units Of FluxDensityContinuum[T]] = e

    deriveInputObjectType[MeasureInput[PosBigDecimal, FluxDensityContinuum[T]]](
      InputObjectTypeName(s"FluxDensityContinuum${groupName.capitalize}Input"),
      InputObjectTypeDescription(s"A flux density continuum value with $groupName units")
    )
  }

  implicit val InputObjectFluxDensityContinuumIntegrated: InputObjectType[MeasureInput[PosBigDecimal, FluxDensityContinuum[Integrated]]] =
    createFluxDensityContinuumInputObjectType("integrated", EnumTypeFluxDensityContinuumIntegrated)

  implicit val InputObjectFluxDensityContinuumSurface: InputObjectType[MeasureInput[PosBigDecimal, FluxDensityContinuum[Surface]]] =
    createFluxDensityContinuumInputObjectType("surface", EnumTypeFluxDensityContinuumSurface)

  private def createEmissionLinesInputObjectType[T](
    groupName: String
  )(implicit ev0: InputType[EmissionLineInput[T]], ev1: InputType[MeasureInput[PosBigDecimal, FluxDensityContinuum[T]]]): InputObjectType[EmissionLinesInput[T]] =
    InputObjectType[EmissionLinesInput[T]](
      s"EmissionLines${groupName.capitalize}Input",
      s"""Create or edit emission lines with $groupName line flux and flux density continuum units. Both "lines" and "fluxDensityContinuum" are required when creating a new EmissionLines${groupName.capitalize}.""",
      List(
        ListInputType(ev0).createRequiredEditOptional("lines", s"EmissionLines${groupName.capitalize}"),
        ev1.createRequiredEditOptional("fluxDensityContinuum", s"EmissionLines${groupName.capitalize}")
      )
    )

  implicit val InputObjectEmissionLinesIntegrated: InputObjectType[EmissionLinesInput[Integrated]] =
    createEmissionLinesInputObjectType("integrated")

  implicit val InputObjectEmissionLinesSurface: InputObjectType[EmissionLinesInput[Surface]] =
    createEmissionLinesInputObjectType("surface")

  private def spectralDefinitionInputObjectType[T](
    groupName: String
  )(
    implicit ev0: InputType[BandNormalizedInput[T]],
             ev1: InputType[EmissionLinesInput[T]]
  ): InputObjectType[SpectralDefinitionInput[T]] =
    InputObjectType[SpectralDefinitionInput[T]](
      s"SpectralDefinition${groupName.capitalize}Input",
      s"""Spectral definition input with $groupName units.  Specify exactly one of "bandNormalized" or "emissionLines" """,
      List(
        ev0.optionField("bandNormalized"),
        ev1.optionField("emissionLines")
      )
    )

  implicit val InputObjectSpectralDefinitionIntegrated: InputObjectType[SpectralDefinitionInput[Integrated]] =
    spectralDefinitionInputObjectType("integrated")

  implicit val InputObjectSpectralDefinitionSurface: InputObjectType[SpectralDefinitionInput[Surface]] =
    spectralDefinitionInputObjectType("surface")

  implicit val InputObjectGaussian: InputObjectType[GaussianInput] =
    InputObjectType[GaussianInput](
      "GaussianInput",
      """Create or edit a gaussian source.  Specify both "fwhm" and "spectralDefinition" when creating a new Gaussian.""",
      List(
        InputObjectAngle.createRequiredEditOptional("fwhm", "Gaussian"),
        InputObjectSpectralDefinitionIntegrated.createRequiredEditOptional("spectralDefinition", "Gaussian")
      )
    )

  implicit val InputObjectSourceProfile: InputObjectType[SourceProfileInput] =
    InputObjectType[SourceProfileInput](
      "SourceProfileInput",
      """Create or edit a source profile.  Exactly one of "point", "uniform" or "gaussian" is required.""",
      List(
        InputObjectSpectralDefinitionIntegrated.optionField("point"),
        InputObjectSpectralDefinitionSurface.optionField("uniform"),
        InputObjectGaussian.optionField("gaussian")
      )
    )

}
