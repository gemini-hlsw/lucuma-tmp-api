// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit

import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import eu.timepit.refined.types.all.PosDouble
import lucuma.core.enums.{GmosGratingOrder, GmosNorthFilter, GmosNorthFpu, GmosNorthGrating, GmosSouthFilter, GmosSouthFpu, GmosSouthGrating, ImageQuality}
import lucuma.core.math.{Offset, Wavelength}
import lucuma.core.math.syntax.int._
import lucuma.core.model.SourceProfile
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import lucuma.gen.gmos.{GmosNorthInitialDynamicConfig, GmosSouthInitialDynamicConfig}
import lucuma.gen.{SciExposureTime, SequenceState}
import lucuma.odb.api.model.GmosModel.{DynamicOptics, GratingConfig, NorthDynamic, SouthDynamic}
import lucuma.odb.api.model.StepConfig
import lucuma.odb.api.model.gmos.longslit.{DeltaWavelengthCalculator, LongSlit, XBinCalculator}

import scala.collection.immutable.LazyList

/**
 * GMOS long slit science atoms
 *
 * @tparam D dynamic config type
 * @tparam G grating type
 * @tparam F filter type
 * @tparam U FPU type
 */
sealed trait Science[D, G, F, U] extends SequenceState[D] {

  def optics: DynamicOptics[D, G, F, U]

  def compute(
    mode:          LongSlit[G, F, U],
    exposureTime:  SciExposureTime,
    λ:             Wavelength,
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  )(implicit wc: DeltaWavelengthCalculator[G],
             xb: XBinCalculator[U]
  ): LazyList[Science.Atom[D]] = {

    val p0  = Offset.P(0.arcsec)
    val Δλs = mode.λDithers.toList
    val qs  = mode.spatialOffsets.toList

    def nextAtom(index: Int, d: D): Science.Atom[D] = {
      val Δ = Δλs(index % Δλs.length)
      val q = qs(index % qs.length)

      (for {
        _ <- optics.wavelength := GmosLongSlit.wavelengthDither(λ, Δ)
        s <- scienceStep(Offset(p0, q))
        f <- flatStep
      } yield Science.Atom(index, s, f)).runA(d).value
    }

    val init: D =
      (for {
        _ <- optics.exposure      := exposureTime
        _ <- optics.gratingConfig := GratingConfig(mode.grating, GmosGratingOrder.One, λ).some
        _ <- optics.filter        := mode.filter
        _ <- optics.fpu           := mode.fpu.asRight.some

        _ <- optics.xBin          := mode.xBin(sourceProfile, imageQuality, sampling)
        _ <- optics.yBin          := mode.yBin
        _ <- optics.ampReadMode   := mode.ampReadMode
        _ <- optics.ampGain       := mode.ampGain

        _ <- optics.roi           := mode.roi
      } yield ()).runS(initialConfig).value

    LazyList.unfold((0, init)) { case (i, d) =>
      val a = nextAtom(i, d)
      Some((a, (i+1, a.science.instrumentConfig)))
    }
  }

}

object Science {

  /**
   * Science and associated matching flat.
   */
  final case class Atom[D](
    index:   Int,
    science: StepConfig[D],
    flat:    StepConfig[D]
  ) {

    def steps: NonEmptyList[StepConfig[D]] =
      if ((index % 2) === 0) NonEmptyList.of(science, flat)
      else NonEmptyList.of(flat, science)

  }

  object GmosNorth extends GmosNorthInitialDynamicConfig
                      with Science[NorthDynamic, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    def optics: DynamicOptics[NorthDynamic, GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] =
      NorthDynamic
  }

  object GmosSouth extends GmosSouthInitialDynamicConfig
                      with Science[SouthDynamic, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    def optics: DynamicOptics[SouthDynamic, GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] =
      SouthDynamic
  }

}
