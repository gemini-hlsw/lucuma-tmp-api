// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.effect._
import cats.implicits._
// import eu.timepit.refined._
import lucuma.core.math.Angle
import lucuma.core.enum._
import lucuma.odb.itc.Itc
import lucuma.odb.search.TargetProfile
import lucuma.odb.search.ObservingMode
import lucuma.odb.api.model.syntax.instrument._
import lucuma.odb.api.model.syntax.finiteduration._
import io.circe.syntax._
import org.http4s._
import org.http4s.syntax.all._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.asynchttpclient.client.AsyncHttpClient
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware._
import org.http4s.dsl.io._
import scala.concurrent.duration._

/** An ITC implementation that calls the OCS2 ITC server remotely. */
object ItcImpl {

  private val MaxPercentSaturation = 50.0

  def forHeroku[F[_]: Async]: Resource[F, Itc[F]] =
    forUri(uri"https://gemini-itc.herokuapp.com/json")

  def forUri[F[_]: Async](uri: Uri): Resource[F, Itc[F]] =
    AsyncHttpClient.resource[F]()
      .map(RequestLogger(true, true))
      .map(ResponseLogger(true, true))
      .map(forClientAndUri[F](_, uri))

  def forClientAndUri[F[_]: Concurrent](c: Client[F], uri: Uri): Itc[F] =
    new Itc[F] with Http4sClientDsl[F] {

      def calculate(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        signalToNoise: Int
      ): F[Itc.Result] =
        observingMode match {
          case _: ObservingMode.Spectroscopy => spectroscopy(targetProfile, observingMode, signalToNoise)
          // TODO: imaging
        }

      def spectroscopy(
        targetProfile: TargetProfile,
        observingMode: ObservingMode,
        signalToNoise: Int
      ): F[Itc.Result] = {

        // The OCS2 ITC doesn't know how to compute exposure time and exposure count for a
        // requested signal-to-noise so we have to kind of wank around to estimate it here, which
        // can require up to three round-trip calls to ITC. We can push some logic back over there
        // at some point but this is ok for now.

        // Convenience method to compute an OCS2 ITC result for the specified profile/mode.
        def itc(exposureDuration: FiniteDuration, exposures: Int): F[ItcResult] = {
          val json = spectroscopyParams(targetProfile, observingMode, exposureDuration, exposures).asJson
          c.expect(POST(json, uri))(jsonOf[F, ItcResult])
        }

        // Pull our exposure limits out of the observing mode since we'll need them soon.
        // N.B. we can't just import these because they're added to `Instrument` with syntax.
        val minExposureDuration: FiniteDuration = observingMode.instrument.minExposureDuration
        val maxExposureDuration: FiniteDuration = observingMode.instrument.maxExposureDuration
        val integralDurations:   Boolean        = observingMode.instrument.integralDurations

        // Compute a 1-second exposure and use this as a baseline for estimating longer/multiple
        // exposures. All of our estimations are just that: we can't deal with read noise accurately
        // here so the final 9kresult will be approximately the S/N the user requests. We just have to
        // accept this limitation for now. Note that conditions are totally guesswork so this may
        // end up being good enough. Unclear.

        itc(1.second, 1).flatMap { baseline =>

          // First thing we need to check is the saturation for a minimum-length exposure. If it's
          // greater than our limit we simply can't observe in this mode because the source is
          // too bright.

          if (baseline.maxPercentFullWell * minExposureDuration.toDoubleSeconds > MaxPercentSaturation) {

            (Itc.Result.SourceTooBright : Itc.Result).pure[F]

          } else {

            // Ok so we know that it's possible to observe this thing. Let's scale to get an ideal
            // single exposure time. If it's within instrument limits and doesn't saturate
            // the detector then we can do the whole thing in a single exposure.

            val singleExposureDuration: FiniteDuration =
              (signalToNoise * signalToNoise / (baseline.maxTotalSNRatio * baseline.maxTotalSNRatio))
                .seconds
                .secondsCeilIf(integralDurations)

            val singleExposureSaturation: Double =
              baseline.maxPercentFullWell * singleExposureDuration.toDoubleSeconds

            if (
              singleExposureDuration >= minExposureDuration &&
              singleExposureDuration <= maxExposureDuration &&
              singleExposureSaturation <= MaxPercentSaturation
            ) {

              // We can do this in one exposure, but we need to hit ITC again to get an accurate
              // signal-to-noise value.
              itc(singleExposureDuration, 1).map { r =>
                Itc.Result.Success(singleExposureDuration, 1, r.maxTotalSNRatio.toInt)
              }

            } else {

              // For multiple exposures we compute the time it would take to fill the well to 50%,
              // then clip to instrument limits and round up to the nearest second if necessary.

              val multipleExposureSecs: FiniteDuration =
                (MaxPercentSaturation / baseline.maxPercentFullWell)
                  .seconds
                  .min(maxExposureDuration)
                  .max(minExposureDuration)
                  .secondsCeilIf(integralDurations)

              // We can't compute S/N accurately enough to extrapolate the number of exposures we
              // need so we must ask ITC to do it.
              itc(multipleExposureSecs, 1).flatMap { r =>

                // Now estimate the number of exposures. It may be low due to read noise but we
                // don't really have a way to compensate yet.
                val n = ((signalToNoise * signalToNoise) / (r.maxTotalSNRatio * r.maxTotalSNRatio)).ceil.toInt

                // But in any case we can calculate our final answer, which may come in low.
                itc(multipleExposureSecs, n).map { r2 =>
                  Itc.Result.Success(multipleExposureSecs, n, r2.maxTotalSNRatio.toInt)
                }

              }

            }

          }

        }

      }

  }

  /** Convert model types into OCS2 ITC-compatible types for a spectroscopy request. */
  private def spectroscopyParams(
    targetProfile: TargetProfile,
    observingMode: ObservingMode,
    exposureDuration:  FiniteDuration,
    exposures:     Int
  ): ItcParameters =
    ItcParameters(
      source = ItcSourceDefinition.fromTargetProfile(targetProfile),
      observation =
        ItcObservationDetails(
          calculationMethod = ItcObservationDetails.CalculationMethod.SignalToNoise.Spectroscopy(
            exposures        = exposures,
            coadds           = None,
            exposureDuration = exposureDuration,
            sourceFraction   = 1.0,
            ditherOffset     = Angle.Angle0
          ),
          analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(
            skyAperture = 5.0
          )
        ),
      conditions = ItcObservingConditions(
        iq      = ImageQuality.PointEight, // Orginially 0.85
        cc      = CloudExtinction.PointFive, // Originally 0.7
        wv      = WaterVapor.Dry, // Orginally Any
        sb      = SkyBackground.Bright, // Originally 0.5
        airmass = 1.5
      ),
      telescope = ItcTelescopeDetails(
        wfs = ItcWavefrontSensor.OIWFS
      ),
      instrument = ItcInstrumentDetails.fromObservingMode(observingMode)
    )

}
