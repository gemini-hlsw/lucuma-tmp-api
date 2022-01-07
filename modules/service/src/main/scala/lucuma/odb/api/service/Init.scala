// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import lucuma.odb.api.model._
import lucuma.odb.api.model.targetModel._
import lucuma.odb.api.repo.OdbRepo
import lucuma.core.`enum`._
import lucuma.core.optics.syntax.all._
import lucuma.core.math.syntax.int._
import cats.data.State
import cats.effect.Sync
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.parser.decode
import lucuma.core.model.Program
import lucuma.odb.api.model.OffsetModel.ComponentInput

import scala.concurrent.duration._

object Init {

  // 2) NGC 5949
  // 3) NGC 3269
  // 4) NGC 3312

  val targetsJson = List(
"""
{
  "name":  "NGC 5949",
  "ra":    { "hms":  "15:28:00.668" },
  "dec":   { "dms": "64:45:47.4"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 0.0 },
    "dec": { "milliarcsecondsPerYear": 0.0 }
  },
  "radialVelocity": { "metersPerSecond": 423607 },
  "parallax":       { "milliarcseconds":  0.00 },
  "sourceProfile": {
    "point": {
      "bandNormalized": {
        "sed": {
          "galaxy": "SPIRAL"
        },
        "brightnesses": [
          {
            "magnitude": {
              "value": 12.7,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "B"
          },
          {
            "magnitude": {
              "value": 10.279,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "J",
            "error": 0.0009
          },
          {
            "magnitude": {
              "value": 9.649,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "H",
            "error": 0.0120
          },
          {
            "magnitude": {
              "value": 9.425,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "K",
            "error": 0.0170
          },
          {
            "magnitude": {
              "value": 14.147,
              "units": "AB_MAGNITUDE"
            },
            "band": "SLOAN_U",
            "error": 0.0050
          },
          {
            "magnitude": {
              "value": 12.924,
              "units": "AB_MAGNITUDE"
            },
            "band": "SLOAN_G",
            "error": 0.0020
          },
          {
            "magnitude": {
              "value": 12.252,
              "units": "AB_MAGNITUDE"
            },
            "band": "SLOAN_R",
            "error": 0.0020
          },
          {
            "magnitude": {
              "value": 11.888,
              "units": "AB_MAGNITUDE"
            },
            "band": "SLOAN_I",
            "error": 0.0020
          },
          {
            "magnitude": {
              "value": 11.636,
              "units": "AB_MAGNITUDE"
            },
            "band": "SLOAN_Z",
            "error": 0.0020
          }
        ]
      }
    }
  }
}
""",
"""
{
  "name":  "NGC 3269",
  "ra":    { "hms":  "10:29:57.070" },
  "dec":   { "dms": "-35:13:27.8"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 0.0 },
    "dec": { "milliarcsecondsPerYear": 0.0 }
  },
  "radialVelocity": { "metersPerSecond": 3753885 },
  "parallax":       { "milliarcseconds":  0.00 },
  "sourceProfile": {
    "point": {
      "bandNormalized": {
        "sed": {
          "galaxy": "SPIRAL"
        },
        "brightnesses": [
          {
            "magnitude": {
              "value": 13.240,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "B"
          },
          {
            "magnitude": {
              "value": 13.510,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "V"
          },
          {
            "magnitude": {
              "value": 11.730,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "R"
          },
          {
            "magnitude": {
              "value": 9.958,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "J",
            "error": 0.018
          },
          {
            "magnitude": {
              "value": 9.387,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "H",
            "error": 0.024
          },
          {
            "magnitude": {
              "value": 9.055,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "K",
            "error": 0.031
          }
        ]
      }
    }
  }
}
""",
"""
{
  "name":  "NGC 3312",
  "ra":    { "hms": "10:37:02.549" },
  "dec":   { "dms": "-27:33:54.17"  },
  "epoch": "J2000.000",
  "properMotion": {
    "ra":  { "milliarcsecondsPerYear": 0.0 },
    "dec": { "milliarcsecondsPerYear":  0.0 }
  },
  "radialVelocity": { "metersPerSecond": 2826483 },
  "parallax":       { "milliarcseconds":  0.0 },
  "sourceProfile": {
    "point": {
      "bandNormalized": {
        "sed": {
          "galaxy": "SPIRAL"
        },
        "brightnesses": [
          {
            "magnitude": {
              "value": 12.630,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "B"
          },
          {
            "magnitude": {
              "value": 13.960,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "V"
          },
          {
            "magnitude": {
              "value": 9.552,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "J",
            "error": 0.016
          },
          {
            "magnitude": {
              "value": 8.907,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "H",
            "error": 0.017
          },
          {
            "magnitude": {
              "value": 8.665,
              "units": "VEGA_MAGNITUDE"
            },
            "band": "K",
            "error": 0.028
          }
        ]
      }
    }
  }
}
"""
  )


  val targets: Either[Exception, List[CreateSiderealInput]] =
    targetsJson.traverse(decode[CreateSiderealInput])

  import GmosModel.{CreateCcdReadout, CreateSouthDynamic}
  import StepConfig.CreateStepConfig

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

  val ac1: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmosAc, OffsetModel.Input.Zero)

  val ac2: CreateStepConfig[CreateSouthDynamic] =
    edit(ac1) {
      for {
        _ <- step.p                                               := ComponentInput(10.arcsec)
        _ <- step.exposure                                        := FiniteDurationModel.Input(20.seconds)
        _ <- step.instrumentConfig.andThen(readout).andThen(xBin) := GmosXBinning.One
        _ <- step.instrumentConfig.andThen(readout).andThen(yBin) := GmosYBinning.One
        _ <- step.instrumentConfig.andThen(roi)                   := GmosRoi.CentralStamp
        _ <- step.instrumentConfig.andThen(fpu)                   := GmosSouthFpu.LongSlit_1_00.asRight.some
      } yield ()
    }

  val ac3: CreateStepConfig[CreateSouthDynamic] =
    (step.exposure := FiniteDurationModel.Input(30.seconds)).runS(ac2).value

  val acquisitionSequence: SequenceModel.Create[CreateSouthDynamic] =
    SequenceModel.Create(
      List(ac1, ac2, ac3).map(AtomModel.Create.continueTo) ++
        List.fill(10)(AtomModel.Create.stopBefore(ac3))
    )

  val gcal: GcalModel.Create =
    GcalModel.Create(
      GcalContinuum.QuartzHalogen.some,
      List.empty[GcalArc],
      GcalFilter.Gmos,
      GcalDiffuser.Visible,
      GcalShutter.Closed
    )

  val Q15: OffsetModel.Input =
    OffsetModel.Input.fromArcseconds(0.0, 15.0)

  val gmos520: CreateSouthDynamic =
    edit(gmosAc) {
      for {
        _ <- exposure                 := FiniteDurationModel.Input.fromSeconds(5.0)
        _ <- readout.andThen(ampRead) := GmosAmpReadMode.Slow
        _ <- readout.andThen(xBin)    := GmosXBinning.Two
        _ <- readout.andThen(yBin)    := GmosYBinning.Two
        _ <- roi                      := GmosRoi.CentralSpectrum
        _ <- grating                  := GmosModel.CreateGrating[GmosSouthDisperser](GmosSouthDisperser.B600_G5323, GmosDisperserOrder.One, WavelengthModel.Input.fromNanometers(520.0)).some
        _ <- filter                   := Option.empty[GmosSouthFilter]
        _ <- fpu                      := GmosSouthFpu.LongSlit_1_00.asRight.some
      } yield ()
    }

  val gmos525: CreateSouthDynamic =
    edit(gmos520)(
      CreateSouthDynamic.instrument.wavelength := WavelengthModel.Input.fromNanometers(525.0)
    )

  val threeSeconds: FiniteDurationModel.Input =
    FiniteDurationModel.Input.fromSeconds(3.0)

  val flat_520: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.gcal(edit(gmos520)(exposure := threeSeconds), gcal)

  val flat_525: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.gcal(edit(gmos525)(exposure := threeSeconds), gcal)

  val sci0_520: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos520, OffsetModel.Input.Zero)

  val sci15_520: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos520, Q15)

  val sci0_525: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos525, OffsetModel.Input.Zero)

  val sci15_525: CreateStepConfig[CreateSouthDynamic] =
    CreateStepConfig.science(gmos525, Q15)

  val scienceSequence: SequenceModel.Create[CreateSouthDynamic] =
    SequenceModel.Create(
      List(
        flat_520,  sci0_520,
        sci15_520, flat_520,
        flat_520,  sci15_520,
        sci0_520,  flat_520,
        flat_520,  sci0_520,
        sci15_520, flat_520,

        flat_525,  sci15_525,
        sci0_525,  flat_525,
        flat_525,  sci0_525,
        sci15_525, flat_525
      ).map(StepModel.Create.continueTo)
       .grouped(2) // pairs flat and science steps
       .toList
       .map(AtomModel.Create(None, _))
    )

  def obs(
    pid:   Program.Id,
    target: Option[TargetModel]
  ): ObservationModel.Create =
    ObservationModel.Create(
      observationId        = None,
      programId            = pid,
      name                 = target.map(_.name) orElse NonEmptyString.from("Observation").toOption,
      status               = ObsStatus.New.some,
      activeStatus         = ObsActiveStatus.Active.some,
      targets              = target.fold(none[TargetEnvironmentModel.Create]) { sidereal =>
        TargetEnvironmentModel.Create(List(sidereal.id).some, None).some
      },
      constraintSet        = None,
      scienceRequirements  = ScienceRequirementsModel.Create.Default.some,
      scienceConfiguration = None,
      config               =
        InstrumentConfigModel.Create.gmosSouth(
          GmosModel.CreateSouthStatic.Default,
          acquisitionSequence,
          scienceSequence
        ).some
    )

  /**
   * Initializes a (presumably) empty ODB with some demo values.
   */
  def initialize[F[_]: Sync](repo: OdbRepo[F]): F[Unit] =
    for {
      p  <- repo.program.insert(
              ProgramModel.Create(
                None,
                NonEmptyString.from("The real dark matter was the friends we made along the way").toOption
              )
            )
      _  <- repo.program.insert(
              ProgramModel.Create(
                None,
                NonEmptyString.from("An Empty Placeholder Program").toOption
              )
            )
      cs <- targets.liftTo[F]
      ts <- cs.traverse { c =>
        repo.target.insert(TargetModel.Create(None, p.id, Some(c), None))
      }
      _  <- repo.observation.insert(obs(p.id, ts.headOption))
      _  <- repo.observation.insert(obs(p.id, ts.lastOption))
      _  <- repo.observation.insert(obs(p.id, None))
    } yield ()

}
