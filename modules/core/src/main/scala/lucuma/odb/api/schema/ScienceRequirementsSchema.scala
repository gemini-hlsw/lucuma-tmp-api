// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.math.Angle
import lucuma.core.enum.FocalPlane
import lucuma.core.enum.SpectroscopyCapabilities
import lucuma.core.enum.ScienceMode
import lucuma.odb.api.model.{FocalPlaneAngleInput, ScienceRequirements, SpectroscopyScienceRequirements}
import lucuma.odb.api.schema.syntax.all._
import sangria.schema._
import sangria.macros.derive._

object ScienceRequirementsSchema {
  import WavelengthSchema._
  import RefinedSchema._

  implicit val EnumTypeScienceRequirementMode: EnumType[ScienceMode] =
    EnumType.fromEnumerated("ScienceRequirementMode", "Mode Spectroscopy/Imaging")

  implicit val EnumTypeFocalPlane: EnumType[FocalPlane] =
    EnumType.fromEnumerated("FocalPlane", "Focal plane Single/Multi/IFU")

  implicit val EnumTypeSpectroscopyCapabilities: EnumType[SpectroscopyCapabilities] =
    EnumType.fromEnumerated("SpectroscopyCapabilities", "Spectroscopy capabilities Nod&Shuffle/Polarimetry/Corongraphy")

  implicit val EnumSpectroscopyModelAngleUnits: EnumType[FocalPlaneAngleInput.Units] =
    EnumType.fromEnumerated[FocalPlaneAngleInput.Units](
      "FocalPlaneAngleUnits",
      "Focal plane angle units"
    )

  implicit val InputFocalPlaneAngleInput: InputType[FocalPlaneAngleInput] =
    deriveInputObjectType[FocalPlaneAngleInput](
      InputObjectTypeName("FocalPlaneAngleInput"),
      InputObjectTypeDescription("Focal plane angle source angle in appropriate units"),
    )

  val FocalPlaneAngleType: ObjectType[Any, Angle]=
    ObjectType(
      name     = "focalPlaneAngle",
      fieldsFn = () => fields(

        Field(
          name        = "microarcseconds",
          fieldType   = LongType,
          description = Some("Focal plane angle in µas"),
          resolve     = v => Angle.signedMicroarcseconds.get(v.value)
        ),

        Field(
          name        = "milliarcseconds",
          fieldType   = BigDecimalType,
          description = Some("Focal plane angle in mas"),
          resolve     = v => Angle.signedDecimalMilliarcseconds.get(v.value)
        ),

        Field(
          name        = "arcseconds",
          fieldType   = BigDecimalType,
          description = Some("Focal plane angle in arcsec"),
          resolve     = v => Angle.signedDecimalArcseconds.get(v.value)
        )

      )
    )

  val SpectroscopyRequirementsType: ObjectType[Any, SpectroscopyScienceRequirements] =
    ObjectType(
      name     = "SpectroscopyScienceRequirements",
      fieldsFn = () =>
        fields(
          Field(
            name        = "wavelength",
            fieldType   = OptionType(WavelengthType),
            description = Some("Requested central wavelength"),
            resolve     = _.value.wavelength
          ),

          Field(
            name        = "resolution",
            fieldType   = OptionType(PosIntType),
            description = Some("Requested resolution"),
            resolve     = _.value.resolution
          ),

          Field(
            name        = "signalToNoise",
            fieldType   = OptionType(PosBigDecimalType),
            description = Some("Requested signal to noise ratio"),
            resolve     = _.value.signalToNoise
          ),

          Field(
            name        = "signalToNoiseAt",
            fieldType   = OptionType(WavelengthType),
            description = Some("Requested wavelength for the requested signal to noise"),
            resolve     = _.value.signalToNoiseAt
          ),

          Field(
            name        = "wavelengthCoverage",
            fieldType   = OptionType(WavelengthType),
            description = Some("Wavelength range"),
            resolve     = _.value.wavelengthCoverage
          ),

          Field(
            name        = "focalPlane",
            fieldType   = OptionType(EnumTypeFocalPlane),
            description = Some("Focal plane choice"),
            resolve     = _.value.focalPlane
          ),

          Field(
            name        = "focalPlaneAngle",
            fieldType   = OptionType(FocalPlaneAngleType),
            description = Some("Focal plane angle"),
            resolve     = _.value.focalPlaneAngle
          ),

          Field(
            name        = "capabilities",
            fieldType   = OptionType(EnumTypeSpectroscopyCapabilities),
            description = Some("Spectroscopy Capabilities"),
            resolve     = _.value.capabilities
          ),

        )
    )

  def ScienceRequirementsType[F[_]]: ObjectType[Any, ScienceRequirements] =
    ObjectType(
      name     = "ScienceRequirements",
      fieldsFn = () =>
        fields(
          Field(
            name        = "mode",
            fieldType   = EnumTypeScienceRequirementMode,
            description = Some("Science requirement mode"),
            resolve     = _.value.mode
          ),

          Field(
            name        = "spectroscopy",
            fieldType   = SpectroscopyRequirementsType,
            description = Some("Spectroscopy requirements"),
            resolve     = _.value.spectroscopy
          ),
        )
    )

}
