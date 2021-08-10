// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.itc

import lucuma.odb.search._
import scala.concurrent.duration.FiniteDuration

trait Itc[F[_]] {

  /**
   * Compute the exposure time and number required to achieve the desired signal-to-noise under
   * average conditions.
   */
  def calculate(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    signalToNoise: Int
  ): F[Itc.Result]

}

object Itc {

  def apply[F[_]](implicit ev: Itc[F]): ev.type = ev

  sealed trait Result
  object Result {

    final case class Success(
      exposureTime:  FiniteDuration,
      exposures:     Int,
      signalToNoise: Int
    ) extends Result

    /** Object is too bright to be observed in the specified mode. */
    final case object SourceTooBright extends Result

  }

}
