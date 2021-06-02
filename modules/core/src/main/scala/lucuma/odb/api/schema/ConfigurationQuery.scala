// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.all._
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

trait ConfigurationQuery {

  import ConfigurationAlternativesSchema._

  // TBD Add return type and hook it to the basic case algorithm
  def spectroscopy[F[_]: Effect]: Field[OdbRepo[F], Unit] =
    Field(
      name        = "spectroscopy",
      fieldType   = IntType,
      description = None,
      arguments   = List(ArgumentConfigurationAlternativesModelSearch),
      // TODO Return a real result
      resolve     = c => c.arg(ArgumentConfigurationAlternativesModelSearch).wavelength.foldMap(_.nanometers.foldMap(_.toInt))
    )

  def allFields[F[_]: Effect]: List[Field[OdbRepo[F], Unit]] =
    List(
      spectroscopy
    )

}

object ConfigurationQuery extends ConfigurationQuery
