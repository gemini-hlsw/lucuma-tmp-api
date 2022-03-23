// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen.gmos.longslit

import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import coulomb.Quantity
import coulomb.refined._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import fs2.Stream
import lucuma.core.`enum`.{GmosDisperserOrder, GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu, GmosRoi, GmosXBinning, GmosYBinning, ImageQuality}
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int._
import lucuma.core.math.units._
import lucuma.core.model.SourceProfile
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import lucuma.gen.gmos.GmosNorthSequenceGenerator
import lucuma.odb.api.model.{AtomModel, GmosModel, ScienceConfigurationModel, StepModel}
import lucuma.odb.api.model.GmosModel.{CustomMask, Grating, NorthDynamic}
import lucuma.gen.gmos.longslit.syntax.all._

import scala.concurrent.duration._
import spire.std.int._

/**
 * Sequence generation for GMOS North Longslit
 */
sealed trait GmosNorthLongSlit {

  def acquisition[F[_]: Sync](
    exposureTime: FiniteDuration
  ): Stream[F, AtomModel[StepModel[NorthDynamic]]]

  def science[F[_]: Sync](
    exposureTime:  FiniteDuration,
    sourceProfile: SourceProfile,
    imageQuality:  ImageQuality,
    sampling:      PosDouble
  ): Stream[F, AtomModel[StepModel[GmosModel.NorthDynamic]]]

}

object GmosNorthLongSlit {

  def apply(
    λ:    Wavelength,
    mode: ScienceConfigurationModel.Modes.GmosNorthLongSlit
  ): GmosNorthLongSlit =

    new GmosNorthLongSlit with GmosNorthSequenceGenerator {

      override def acquisition[F[_]: Sync](
        exposureTime: FiniteDuration
      ): Stream[F, AtomModel[StepModel[NorthDynamic]]] = {

        def filter: GmosNorthFilter = GmosNorthFilter.allAcquisition.minBy { f =>
          (λ.toPicometers.value.value - f.wavelength.toPicometers.value.value).abs
        }

        eval {
          for {
            _  <- NorthDynamic.exposure := exposureTime
            _  <- NorthDynamic.filter   := filter.some
            _  <- NorthDynamic.fpu      := none[Either[CustomMask, GmosNorthFpu]]
            _  <- NorthDynamic.grating  := none[Grating[GmosNorthDisperser]]
            _  <- NorthDynamic.xBin     := GmosXBinning.Two
            _  <- NorthDynamic.yBin     := GmosYBinning.Two
            _  <- NorthDynamic.roi      := GmosRoi.Ccd2
            s0 <- scienceStep(0.arcsec, 0.arcsec)

            _  <- NorthDynamic.exposure := 20.seconds
            _  <- NorthDynamic.fpu      := mode.fpu.asRight.some
            _  <- NorthDynamic.xBin     := GmosXBinning.One
            _  <- NorthDynamic.yBin     := GmosYBinning.One
            _  <- NorthDynamic.roi      := GmosRoi.CentralStamp
            s1 <- scienceStep(10.arcsec, 0.arcsec)

            _  <- NorthDynamic.exposure := exposureTime * 4
            s2 <- scienceStep(0.arcsec, 0.arcsec)

            a0  = atom(s0, s1, s2)
            a1  = atom(s2)
          } yield Stream.eval(a0) ++ Stream.repeatEval(a1)
        }
      }

      override def science[F[_]: Sync](
        exposureTime:  FiniteDuration,
        sourceProfile: SourceProfile,
        imageQuality:  ImageQuality,
        sampling:      PosDouble
      ): Stream[F, AtomModel[StepModel[GmosModel.NorthDynamic]]] = {

        def sum(λ: Wavelength, Δ: Quantity[PosInt, Nanometer]): Wavelength =
          new Wavelength(λ.toPicometers + Δ.to[PosInt, Picometer])

        eval {
          for {
            _  <- NorthDynamic.exposure := exposureTime
            _  <- NorthDynamic.xBin     := mode.fpu.xbin(sourceProfile, imageQuality, sampling)
            _  <- NorthDynamic.yBin     := GmosYBinning.Two
            _  <- NorthDynamic.grating  := Grating(mode.disperser, GmosDisperserOrder.One, λ).some
            _  <- NorthDynamic.filter   := mode.filter
            _  <- NorthDynamic.fpu      := mode.fpu.asRight.some
            s0 <- scienceStep(0.arcsec, 0.arcsec)
            f0 <- flatStep

            _  <- NorthDynamic.wavelength := sum(λ, mode.disperser.Δλ)
            s1 <- scienceStep(0.arcsec, 15.arcsec)
            f1 <- flatStep

            a0  = atom(s0, f0)
            a1  = atom(f1, s1)
            a2  = atom(s1, f1)
            a3  = atom(f0, s0)

          } yield Stream.evals(List(a0, a1, a2, a3).sequence)
        }
      }
    }

}
