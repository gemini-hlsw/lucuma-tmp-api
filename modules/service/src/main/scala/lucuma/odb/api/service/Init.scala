// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import cats.Applicative
import cats.data.State
import lucuma.odb.api.model._
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`._
import lucuma.core.optics.syntax.all._
import lucuma.core.math.syntax.int._
import cats.effect.Sync
import cats.syntax.all._
import io.circe.parser.decode
import lucuma.odb.api.model.OffsetModel.ComponentInput
import monocle.state.all._

import scala.concurrent.duration._

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

  import GmosModel.{CreateCcdReadout, CreateSouthDynamic}
  import StepModel.CreateStep

  import CreateSouthDynamic.{exposure, filter, fpu, grating, readout, roi, step}
  import CreateCcdReadout.{ampRead, xBin, yBin}

  private def edit[A](start: A)(state: State[A, _]): A =
    state.runS(start).value

  val gmosAc: CreateSouthDynamic =
    CreateSouthDynamic(
      FiniteDurationModel.Input(10.seconds),
      CreateCcdReadout(
        GmosXBinning.Two,
        GmosYBinning.Two,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Fast
      ),
      GmosDtax.Zero,
      GmosRoi.Ccd2,
      None,
      Some(GmosSouthFilter.RPrime),
      None
    )

  val ac1: CreateStep[CreateSouthDynamic] =
    CreateStep.science(gmosAc, OffsetModel.Input.Zero)

  val ac2: CreateStep[CreateSouthDynamic] =
    edit(ac1) {
      for {
        _ <- step.p                                         := ComponentInput(10.arcsec)
        _ <- step.exposure                                  := FiniteDurationModel.Input(20.seconds)
        _ <- (step.instrumentConfig ^|-> readout ^|-> xBin) := GmosXBinning.One
        _ <- (step.instrumentConfig ^|-> readout ^|-> yBin) := GmosYBinning.One
        _ <- (step.instrumentConfig ^|-> roi)               := GmosRoi.CentralStamp
        _ <- (step.instrumentConfig ^|-> fpu)               := GmosSouthFpu.LongSlit_1_00.asRight.some
      } yield ()
    }

  val ac3: CreateStep[CreateSouthDynamic] =
    step.exposure.assign_(FiniteDurationModel.Input(30.seconds)).runS(ac2).value

  val acquisitionSequence: List[CreateStep[CreateSouthDynamic]] =
    List(ac1, ac2, ac3, ac3)

  val gcal: GcalModel.Create =
    GcalModel.Create(
      GcalContinuum.QuartzHalogen.some,
      List.empty[GcalArc],
      GcalFilter.Gmos,
      GcalDiffuser.Visible,
      GcalShutter.Closed,
      FiniteDurationModel.Input.fromSeconds(3.0),
      CoAddsModel.Input(1)
    )

  val Q15: OffsetModel.Input =
    OffsetModel.Input.fromArcseconds(0.0, 15.0)

  val gmos520: CreateSouthDynamic =
    edit(gmosAc) {
      for {
        _ <- exposure               := FiniteDurationModel.Input.fromSeconds(950.0)
        _ <- (readout ^|-> ampRead) := GmosAmpReadMode.Slow
        _ <- (readout ^|-> xBin)    := GmosXBinning.Two
        _ <- (readout ^|-> yBin)    := GmosYBinning.Two
        _ <- roi                    := GmosRoi.CentralSpectrum
        _ <- grating                := GmosModel.CreateGrating[GmosSouthDisperser](GmosSouthDisperser.B600_G5323, GmosDisperserOrder.One, WavelengthModel.Input.fromNanometers(520.0)).some
        _ <- filter                 := Option.empty[GmosSouthFilter]
        _ <- fpu                    := GmosSouthFpu.LongSlit_1_00.asRight.some
      } yield ()
    }

  val gmos525: CreateSouthDynamic =
    edit(gmos520)(
      CreateSouthDynamic.instrument.wavelength := WavelengthModel.Input.fromNanometers(525.0)
    )

  val flat_520: CreateStep[CreateSouthDynamic] =
    CreateStep.gcal(gmos520, gcal)

  val flat_525: CreateStep[CreateSouthDynamic] =
    CreateStep.gcal(gmos525, gcal)

  val sci0_520: CreateStep[CreateSouthDynamic] =
    CreateStep.science(gmos520, OffsetModel.Input.Zero)

  val sci15_520: CreateStep[CreateSouthDynamic] =
    CreateStep.science(gmos520, Q15)

  val sci0_525: CreateStep[CreateSouthDynamic] =
    CreateStep.science(gmos525, OffsetModel.Input.Zero)

  val sci15_525: CreateStep[CreateSouthDynamic] =
    CreateStep.science(gmos525, Q15)

  val scienceSequence: List[CreateStep[CreateSouthDynamic]] =
    List(
      flat_520, sci0_520, sci15_520, flat_520, sci15_520, sci0_520, flat_520, sci0_520, sci15_520, flat_520,
      flat_525, sci15_525, sci0_525, flat_525, sci0_525, sci15_525, flat_525
    )

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
                Some(ObsStatus.New),
                Some(
                  ConfigModel.Create.gmosSouth(
                    ConfigModel.CreateGmosSouth(
                      ManualSequence.Create(
                        GmosModel.CreateSouthStatic.Default,
                        acquisitionSequence,
                        scienceSequence
                      )
                    )
                  )
                )
              )
            )
    } yield ()

}
