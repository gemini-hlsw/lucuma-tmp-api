// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.data.NonEmptyList
import cats.syntax.option._
import coulomb.Quantity
import coulomb.define.UnitDefinition
import coulomb.si.Kelvin
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.`enum`.{Band, GalaxySpectrum, HIIRegionSpectrum, PlanetSpectrum, PlanetaryNebulaSpectrum, QuasarSpectrum, StellarLibrarySpectrum}
import lucuma.core.math.{BrightnessUnits, BrightnessValue, Wavelength}
import lucuma.core.math.BrightnessUnits.{Integrated, Surface}
import lucuma.core.math.dimensional.GroupedUnitType
import lucuma.core.model.UnnormalizedSED.{BlackBody, CoolStarModel, Galaxy, HIIRegion, Planet, PlanetaryNebula, PowerLaw, Quasar, StellarLibrary, UserDefined}
import lucuma.core.model.{BandBrightness, UnnormalizedSED}
import lucuma.core.syntax.string._
import lucuma.odb.api.repo.OdbRepo
import sangria.schema._

import scala.reflect.ClassTag

object SourceProfileSchema {

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
      "StellarLibrary",
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

  def UnnormalizedSedType[F[_]]: OutputType[UnnormalizedSED] =
    UnionType(
      name        = "UnnormalizedSed",
      description = "Un-normalized spectral energy distribution".some,
      types       = List(
        StellarLibraryType[F],
        CoolStarModelType[F],
        GalaxyType[F],
        PlanetType[F],
        QuasarType[F],
        HiiRegionType[F],
        PlanetaryNebulaType[F],
        PowerLawType[F],
        BlackBodyType[F],
        UserDefinedType[F]
      )
    ).mapValue[UnnormalizedSED](identity)

  private def SpectrumEnumBasedSed[F[_], T: ClassTag, E](
    name:        String,
    enumType:    EnumType[E],
    extractEnum: T => E
  ): ObjectType[OdbRepo[F], T] =
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

  private def KelvinBasedSed[F[_], T: ClassTag](
    name:          String,
    description:   String,
    extractKelvin: T => Quantity[PosBigDecimal, Kelvin]
  ): ObjectType[OdbRepo[F], T] =
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


  def StellarLibraryType[F[_]]: ObjectType[OdbRepo[F], StellarLibrary]  =
    SpectrumEnumBasedSed[F, StellarLibrary, StellarLibrarySpectrum](
      "StellarLibrary",
      EnumTypeStellarLibrarySpectrum,
      _.librarySpectrum
    )

  def CoolStarModelType[F[_]]: ObjectType[OdbRepo[F], CoolStarModel] =
    KelvinBasedSed[F, CoolStarModel](
      name        = "CoolStarModel",
      description = "Cool star model SED",
      _.temperature
    )

  def GalaxyType[F[_]]: ObjectType[OdbRepo[F], Galaxy]  =
    SpectrumEnumBasedSed[F, Galaxy, GalaxySpectrum](
      "Galaxy",
      EnumTypeGalaxySpectrum,
      _.galaxySpectrum
    )

  def PlanetType[F[_]]: ObjectType[OdbRepo[F], Planet]  =
    SpectrumEnumBasedSed[F, Planet, PlanetSpectrum](
      "Planet",
      EnumTypePlanetSpectrum,
      _.planetSpectrum
    )

  def QuasarType[F[_]]: ObjectType[OdbRepo[F], Quasar]  =
    SpectrumEnumBasedSed[F, Quasar, QuasarSpectrum](
      "Quasar",
      EnumTypeQuasarSpectrum,
      _.quasarSpectrum
    )

  def HiiRegionType[F[_]]: ObjectType[OdbRepo[F], HIIRegion]  =
    SpectrumEnumBasedSed[F, HIIRegion, HIIRegionSpectrum](
      "HiiRegion",
      EnumTypeHiiRegionSpectrum,
      _.hiiRegionSpectrum
    )

  def PlanetaryNebulaType[F[_]]: ObjectType[OdbRepo[F], PlanetaryNebula] =
    SpectrumEnumBasedSed[F, PlanetaryNebula, PlanetaryNebulaSpectrum](
      "PlanetaryNebula",
      EnumTypePlanetaryNebulaSpectrum,
      _.planetaryNebulaSpectrum
    )

  def PowerLawType[F[_]]: ObjectType[OdbRepo[F], PowerLaw] =
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

  def BlackBodyType[F[_]]: ObjectType[OdbRepo[F], BlackBody] =
    KelvinBasedSed[F, BlackBody](
      name        = "BlackBocy",
      description = "Black body SED",
      _.temperature
    )

  def FluxDensityEntryType[F[_]]: ObjectType[OdbRepo[F], (Wavelength, PosBigDecimal)] =
    ObjectType(
      name        = "FluxDensityEntry",
      fieldsFn    = () => fields(

        Field(
          name      = "wavelength",
          fieldType = WavelengthType[F],
          resolve   = _.value._1
        ),

        Field(
          name      = "value",
          fieldType = PosBigDecimalType,
          resolve   = _.value._2
        )
      )
    )

  def UserDefinedType[F[_]]: ObjectType[OdbRepo[F], UserDefined] =
    ObjectType(
      name        = "UserDefined",
      description = "User defined SED",
      fieldsFn    = () => fields(
        Field(
          name        = "fluxDensities",
          fieldType   = ListType(FluxDensityEntryType[F]),
          resolve     = _.value.fluxDensities.toNel.toList
        )
      )
    )


  // We are limited in the characters we can use for enum names.  These mappings
  // define how to map illegal characters into something valid for GraphQL.
  private val replacements: List[(String, String)] =
    List(
      " " -> "_",
      "²" -> "2"
    )

  private def defineUnitsEnum(
    name:        String,
    description: String,
    values:      NonEmptyList[GroupedUnitType[_]]
  ): EnumType[UnitDefinition] =

    EnumType(
      name        = name,
      description = description.some,
      values      = values.toList.map { gut =>
        val ud = gut.ungrouped.definition
        println(ud.name)
        EnumValue(
          name        = replacements
                          .foldLeft(ud.name) { case (n, (a, b)) => n.replaceAll(a, b) }
                          .toScreamingSnakeCase,
          description = ud.abbv.some,
          value       = ud
        )
      }
    )

  val EnumTypeBrightnessIntegrated: EnumType[UnitDefinition] =
    defineUnitsEnum(
      "BrightnessIntegrated",
      "Brightness integrated units",
      BrightnessUnits.Brightness.Integrated.all
    )

  val EnumTypeBrightnessSurface: EnumType[UnitDefinition] =
    defineUnitsEnum(
      "BrightnessSurface",
      "Brightness surface units",
      BrightnessUnits.Brightness.Surface.all
    )

  val EnumTypeLineFluxIntegrated: EnumType[UnitDefinition] =
    defineUnitsEnum(
      "LineFluxIntegrated",
      "Line flux integrated units",
      BrightnessUnits.LineFlux.Integrated.all
    )

  val EnumTypeLineFluxSurface: EnumType[UnitDefinition] =
    defineUnitsEnum(
      "LineFluxSurface",
      "Line flux surface units",
      BrightnessUnits.LineFlux.Surface.all
    )

  val EnumTypeFluxDensityContinuumIntegrated: EnumType[UnitDefinition] =
    defineUnitsEnum(
      "FluxDensityContinuumIntegrated",
      "Flux density continuum integrated units",
      BrightnessUnits.FluxDensityContinuum.Integrated.all
    )

  val EnumTypeFluxDensityContinuumSurface: EnumType[UnitDefinition] =
    defineUnitsEnum(
      "FluxDensityContinuumSurface",
      "Flux density continuum surface units",
      BrightnessUnits.FluxDensityContinuum.Surface.all
    )

  private def BandBrightnessType[F[_], T](
    name:      String,
    unitsType: EnumType[UnitDefinition]
  ): ObjectType[OdbRepo[F], BandBrightness[T]] =
    ObjectType(
      name     = name,
      fieldsFn = () => fields(

        Field(
          name        = "value",
          fieldType   = BigDecimalType,
          description = "Magnitude value".some,
          resolve     = c => BrightnessValue.fromBigDecimal.reverseGet(c.value.quantity.value)
        ),

        Field(
          name        = "units",
          fieldType   = unitsType,
          description = "Units in which the magnitude value is expressed".some,
          resolve     = _.value.quantity.unit.definition
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

  def BandBrightnessIntegrated[F[_]]: ObjectType[OdbRepo[F], BandBrightness[Integrated]] =
    BandBrightnessType[F, Integrated](
      "BandBrightnessIntegrated",
      EnumTypeBrightnessIntegrated
    )

  def BandBrightnessSurface[F[_]]: ObjectType[OdbRepo[F], BandBrightness[Surface]] =
    BandBrightnessType[F, Surface](
      "BandBrightnessSurface",
      EnumTypeBrightnessSurface
    )

}
