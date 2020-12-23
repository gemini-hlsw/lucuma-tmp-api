// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import cats.Applicative
import lucuma.odb.api.model._
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`.ObsStatus
import cats.effect.Sync
import cats.syntax.all._
import io.circe.parser.decode

object Init {

  val targetsJson = List(
"""
{
  "name":  "Rigel",
  "ra":    { "hms":  "05:14:32.272" },
  "dec":   { "dms": "-08:12:05.90"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 1.31 },
    "dec": { "milliarcsecondsPerYear": 0.5  }
  },
  "radialVelocity": { "metersPerSecond": 17687 },
  "parallax":       { "milliarcseconds":  6.55 },
  "magnitudes": [
    {
      "band": "R",
      "value": 0.13,
      "system": "VEGA"
    },
    {
      "band": "V",
      "value": 0.13,
      "system": "VEGA"
    }
  ]
}
""",
"""
{
  "name":  "Betelgeuse",
  "ra":    { "hms": "05:55:10.305" },
  "dec":   { "dms": "07:24:25.43"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 27.54 },
    "dec": { "milliarcsecondsPerYear":  11.3 }
  },
  "radialVelocity": { "metersPerSecond": 21884 },
  "parallax":       { "milliarcseconds":  3.78 },
  "magnitudes": [
    {
      "band": "R",
      "value": -1.17,
      "system": "VEGA"
    },
    {
      "band": "V",
      "value": 0.42,
      "system": "VEGA"
    }
  ]
}
"""
  )

  val targets: Either[Exception, List[TargetModel.CreateSidereal]] =
    targetsJson.traverse(decode[TargetModel.CreateSidereal])

  /**
   * Initializes a (presumably) empty ODB with some demo values.
   */
  def initialize[F[_]: Sync: Applicative](repo: OdbRepo[F]): F[Unit] =
    for {
      p  <- repo.program.insert(
              ProgramModel.Create(
                None,
                Some("Observing Stars in Constellation Orion for No Particular Reason")
              )
            )
      _  <- repo.program.insert(
              ProgramModel.Create(
                None,
                Some("An Empty Placeholder Program")
              )
            )
      cs <- targets.liftTo[F]
      ts <- cs.map(_.copy(programIds = Some(List(p.id)))).traverse(repo.target.insertSidereal)
      a0 <- repo.asterism.insert(
              AsterismModel.CreateDefault(
                None,
                Some("More Constellation Than Asterism"),
                List(p.id),
                None,
                Set.from(ts.take(2).map(_.id))
              )
            )
      _  <- repo.observation.insert(
              ObservationModel.Create(
                None,
                p.id,
                Some("First Observation"),
                Some(a0.id),
                Some(ObsStatus.New)
              )
            )
    } yield ()

}
