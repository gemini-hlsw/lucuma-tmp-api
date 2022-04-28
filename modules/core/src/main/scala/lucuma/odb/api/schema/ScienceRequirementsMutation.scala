// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{ScienceRequirementsInput, SpectroscopyScienceRequirementsInput}
import sangria.schema._

trait ScienceRequirementsMutation {

  import ScienceRequirementsSchema._
  import WavelengthSchema._
  import RefinedSchema._
  import syntax.inputtype._

  implicit val InputObjectTypeSpectroscopyScienceRequirements: InputObjectType[SpectroscopyScienceRequirementsInput] =
    InputObjectType[SpectroscopyScienceRequirementsInput](
      "SpectroscopyScienceRequirementsInput",
      "Edit or create spectroscopy science requirements",
      List(
        InputWavelength.nullableField("wavelength"),
        InputObjectPosInt.nullableField("resolution"),
        InputObjectPosBigDecimal.nullableField("signalToNoise"),
        InputWavelength.nullableField("signalToNoiseAt"),
        InputWavelength.nullableField("wavelengthCoverage"),
        EnumTypeFocalPlane.nullableField("focalPlane"),
        InputFocalPlaneAngleInput.nullableField("focalPlaneAngle"),
        EnumTypeSpectroscopyCapabilities.nullableField("capabilities")
      )
    )

  implicit val InputObjectTypeScienceRequirements: InputObjectType[ScienceRequirementsInput] =
    InputObjectType[ScienceRequirementsInput](
      "ScienceRequirementsInput",
      "Edit science requirements",
      List(
        EnumTypeScienceRequirementMode.notNullableField("mode"),
        InputObjectTypeSpectroscopyScienceRequirements.notNullableField("spectroscopy")
      )
    )

}

object ScienceRequirementsMutation extends ScienceRequirementsMutation
