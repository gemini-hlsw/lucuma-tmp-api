// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.enum

sealed abstract class SurfaceBrightness(
  val tag:       String,
  val ocs2Tag:   String
) extends Product with Serializable

object SurfaceBrightness {

  case object Vega           extends SurfaceBrightness("Vega", "Vega mag/arcsec²")
  case object AB             extends SurfaceBrightness("AB", "AB mag/arcsec²")
  case object Jy             extends SurfaceBrightness("Jy", "Jy/arcsec²")
  case object Watts          extends SurfaceBrightness("Watts", "W/m²/µm/arcsec²")
  case object ErgsWavelength extends SurfaceBrightness("ErgsWavelength", "erg/s/cm²/Å/arcsec²")
  case object ErgsFrequency  extends SurfaceBrightness("ErgsFrequency", "erg/s/cm²/Hz/arcsec²")

}

