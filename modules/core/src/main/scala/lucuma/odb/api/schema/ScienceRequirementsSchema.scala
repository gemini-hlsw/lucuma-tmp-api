// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.math.Angle
import lucuma.core.enum.FocalPlane
import lucuma.core.enum.SpectroscopyCapabilities
import lucuma.core.enum.ScienceMode
import lucuma.odb.api.model.SpectroscopyScienceRequirements
import lucuma.odb.api.model.SpectroscopyScienceRequirementsModel
import lucuma.odb.api.model.ScienceRequirements
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.all._

import sangria.schema._
import sangria.macros.derive._

object ScienceRequirementsSchema {
  import WavelengthSchema._
  import RefinedSchema._

  implicit val EnumTypeScienceMode: EnumType[ScienceMode] =
    EnumType.fromEnumerated("ScienceMode", "Mode Spectroscopy/Imaging")

  implicit val EnumTypeFocalPlane: EnumType[FocalPlane] =
    EnumType.fromEnumerated("FocalPlane", "Focal plane Single/Multi/IFU")

  implicit val EnumTypeSpectroscopyCapabilities: EnumType[SpectroscopyCapabilities] =
    EnumType.fromEnumerated("SpectroscopyCapabilities", "Spectroscopy capabilities Nod&Shuffle/Polarimetry/Corongraphy")

  implicit val EnumSpectroscopyModelAnglUnits: EnumType[SpectroscopyScienceRequirementsModel.Units] =
    EnumType.fromEnumerated[SpectroscopyScienceRequirementsModel.Units](
      "FocalPlaneAngleUnits",
      "Focal plane angle units"
    )

  implicit val InputFocalPlaneAngleInput: InputType[SpectroscopyScienceRequirementsModel.FocalPlaneAngleInput] =
    deriveInputObjectType[SpectroscopyScienceRequirementsModel.FocalPlaneAngleInput](
      InputObjectTypeName("FocalPlaneAngleInput"),
      InputObjectTypeDescription("Focal plane angle source angle in appropriate units"),
    )

  def FocalPlaneAngleType[F[_]]: ObjectType[OdbRepo[F], Angle]=
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

  def SpectroscopyRequirementsType[F[_]]: ObjectType[OdbRepo[F], SpectroscopyScienceRequirements] =
    ObjectType(
      name     = "SpectroscopyScienceRequirements",
      fieldsFn = () =>
        fields(
          Field(
            name        = "wavelength",
            fieldType   = OptionType(WavelengthType[F]),
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
            fieldType   = OptionType(WavelengthType[F]),
            description = Some("Requested wavelength for the requested signal to noise"),
            resolve     = _.value.signalToNoiseAt
          ),

          Field(
            name        = "wavelengthRange",
            fieldType   = OptionType(WavelengthType[F]),
            description = Some("Wavelength range"),
            resolve     = _.value.wavelengthRange
          ),

          Field(
            name        = "focalPlane",
            fieldType   = OptionType(EnumTypeFocalPlane),
            description = Some("Focal plane choice"),
            resolve     = _.value.focalPlane
          ),

          Field(
            name        = "focalPlaneAngle",
            fieldType   = OptionType(FocalPlaneAngleType[F]),
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

  def ScienceRequirementsType[F[_]]: ObjectType[OdbRepo[F], ScienceRequirements] =
    ObjectType(
      name     = "ScienceRequirements",
      fieldsFn = () =>
        fields(
          Field(
            name        = "mode",
            fieldType   = EnumTypeScienceMode,
            description = Some("Science mode"),
            resolve     = _.value.mode
          ),

          Field(
            name        = "spectroscopyRequirements",
            fieldType   = SpectroscopyRequirementsType[F],
            description = Some("Spectroscopy requirements"),
            resolve     = _.value.spectroscopyRequirements
          ),
        )
    )

}
