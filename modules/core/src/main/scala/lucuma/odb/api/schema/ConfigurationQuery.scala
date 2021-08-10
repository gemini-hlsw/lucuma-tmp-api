// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.Parallel
import cats.Monad
import cats.syntax.all._
import lucuma.core.math.Redshift
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.search._
import lucuma.odb.itc._
import sangria.schema._

trait ConfigurationQuery {

  import ConfigurationAlternativesSchema._

  // TBD Add return type and hook it to the basic case algorithm
  def spectroscopy[F[_]: Parallel: Monad: Itc]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "spectroscopy",
      fieldType   = IntType,
      description = None,
      arguments   = List(ArgumentConfigurationAlternativesModelSearch),
      resolve     = c => {
        val arg = c.arg(ArgumentConfigurationAlternativesModelSearch)
        (arg.wavelength.toWavelength("wavelength"),
         arg.simultaneousCoverage.toWavelength("simultaneousCoverage"),
         arg.resolution.validNec,
         arg.spatialProfile.toSpatialProfile,
         arg.spectralDistribution.validNec,
         arg.magnitude.toMagnitude,
         arg.redshift.validNec,
        ).mapN {(wavelength, simultaneousCoverage, resolution, spatialProfile, spectralDistribution, magnitude, redshift) =>
          val constraints = Constraints.Spectroscopy(wavelength, simultaneousCoverage, resolution)
          val targetProfile = TargetProfile(spatialProfile, spectralDistribution, magnitude, Redshift(redshift))
          Search.spectroscopy[F](
            constraints, targetProfile, arg.signalToNoise)
          }
        0
      }
    )

  def allFields[F[_]: Parallel: Monad]: List[Field[OdbRepo[F], Unit]] = {
    implicit val itc: Itc[F] = ???
    List(
      spectroscopy
    )
  }

}

object ConfigurationQuery extends ConfigurationQuery
