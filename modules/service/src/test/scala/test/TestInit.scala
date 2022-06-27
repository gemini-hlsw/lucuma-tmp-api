// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test

import cats.data.State
import cats.effect.Sync
import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import eu.timepit.refined._
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal}
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.parser.decode
import lucuma.core.enums.{ScienceMode => _, _}
import lucuma.core.math.syntax.int._
import lucuma.core.model.ZeroTo100
import lucuma.core.model.{Partner, Program}
import lucuma.core.optics.syntax.all._
import lucuma.core.syntax.time._
import lucuma.odb.api.model.OffsetModel.ComponentInput
import lucuma.odb.api.model._
import lucuma.odb.api.model.gmos.longslit.BasicConfigInput
import lucuma.odb.api.model.targetModel.{TargetEnvironmentInput, TargetModel}
import lucuma.odb.api.repo.OdbRepo

object TestInit {

  // 2) NGC 5949
  // 3) NGC 3269
  // 4) NGC 3312
  // 5) NGC 4749

  val targetsJson = List(
"""
{
  "name":  "NGC 5949",
  "sidereal": {
    "ra":    { "hms":  "15:28:00.668" },
    "dec":   { "dms": "64:45:47.4"  },
    "epoch": "J2000.000",
    "properMotion": {
      "ra":  { "milliarcsecondsPerYear": 0.0 },
      "dec": { "milliarcsecondsPerYear": 0.0 }
    },
    "radialVelocity": { "metersPerSecond": 423607 },
    "parallax":       { "milliarcseconds":  0.00 }
  },
  "sourceProfile": {
    "point": {
      "bandNormalized": {
        "sed": {
          "galaxy": "SPIRAL"
        },
        "brightnesses": [
          {
            "band": "B",
            "value": 12.7,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "J",
            "value": 10.279,
            "units": "VEGA_MAGNITUDE",
            "error": 0.0009
          },
          {
            "band": "H",
            "value": 9.649,
            "units": "VEGA_MAGNITUDE",
            "error": 0.0120
          },
          {
            "band": "K",
            "value": 9.425,
            "units": "VEGA_MAGNITUDE",
            "error": 0.0170
          },
          {
            "band": "SLOAN_U",
            "value": 14.147,
            "units": "AB_MAGNITUDE",
            "error": 0.0050
          },
          {
            "band": "SLOAN_G",
            "value": 12.924,
            "units": "AB_MAGNITUDE",
            "error": 0.0020
          },
          {
            "band": "SLOAN_R",
            "value": 12.252,
            "units": "AB_MAGNITUDE",
            "error": 0.0020
          },
          {
            "band": "SLOAN_I",
            "value": 11.888,
            "units": "AB_MAGNITUDE",
            "error": 0.0020
          },
          {
            "band": "SLOAN_Z",
            "value": 11.636,
            "units": "AB_MAGNITUDE",
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
  "sidereal": {
    "ra":    { "hms":  "10:29:57.070" },
    "dec":   { "dms": "-35:13:27.8"  },
    "epoch": "J2000.000",
    "properMotion": {
      "ra":  { "milliarcsecondsPerYear": 0.0 },
      "dec": { "milliarcsecondsPerYear": 0.0 }
    },
    "radialVelocity": { "metersPerSecond": 3753885 },
    "parallax":       { "milliarcseconds":  0.00 }
  },
  "sourceProfile": {
    "point": {
      "bandNormalized": {
        "sed": {
          "galaxy": "SPIRAL"
        },
        "brightnesses": [
          {
            "band": "B",
            "value": 13.240,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "V",
            "value": 13.510,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "R",
            "value": 11.730,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "J",
            "value": 9.958,
            "units": "VEGA_MAGNITUDE",
            "error": 0.018
          },
          {
            "band": "H",
            "value": 9.387,
            "units": "VEGA_MAGNITUDE",
            "error": 0.024
          },
          {
            "band": "K",
            "value": 9.055,
            "units": "VEGA_MAGNITUDE",
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
  "sidereal": {
    "ra":    { "hms": "10:37:02.549" },
    "dec":   { "dms": "-27:33:54.17"  },
    "epoch": "J2000.000",
    "properMotion": {
      "ra":  { "milliarcsecondsPerYear": 0.0 },
      "dec": { "milliarcsecondsPerYear":  0.0 }
    },
    "radialVelocity": { "metersPerSecond": 2826483 },
    "parallax":       { "milliarcseconds":  0.0 }
  },
  "sourceProfile": {
    "point": {
      "bandNormalized": {
        "sed": {
          "galaxy": "SPIRAL"
        },
        "brightnesses": [
          {
            "band": "B",
            "value": 12.630,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "V",
            "value": 13.960,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "J",
            "value": 9.552,
            "units": "VEGA_MAGNITUDE",
            "error": 0.016
          },
          {
            "band": "H",
            "value": 8.907,
            "units": "VEGA_MAGNITUDE",
            "error": 0.017
          },
          {
            "band": "K",
            "value": 8.665,
            "units": "VEGA_MAGNITUDE",
            "error": 0.028
          }
        ]
      }
    }
  }
}
""",
"""
{
  "name":  "NGC 4749",
  "sidereal": {
    "ra":    { "hms":  "12:51:12.001" },
    "dec":   { "dms": "71:38:12.43"  },
    "epoch": "J2000.000",
    "properMotion": {
      "ra":  { "milliarcsecondsPerYear": 0.0 },
      "dec": { "milliarcsecondsPerYear": 0.0 }
    },
    "radialVelocity": { "metersPerSecond": 1728985 },
    "parallax":       { "milliarcseconds":  0.00 }
  },
  "sourceProfile": {
    "point": {
      "bandNormalized": {
        "sed": {
          "galaxy": "SPIRAL"
        },
        "brightnesses": [
          {
            "band": "B",
            "value": 14.2,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "J",
            "value": 10.752,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "H",
            "value": 9.891,
            "units": "VEGA_MAGNITUDE"
          },
          {
            "band": "K",
            "value": 9.467,
            "units": "VEGA_MAGNITUDE"
          }
        ]
      }
    }
  }
}
"""
  )

  def targets(pid: Program.Id): Either[Exception, List[TargetModel.CreateInput]] =
    targetsJson.traverse(decode[TargetModel.PropertiesInput]).map { in =>
      in.map(TargetModel.CreateInput(pid, _))
    }

  import GmosModel.{CreateCcdReadout, CreateSouthDynamic}
  import CreateCcdReadout.{ampRead, xBin, yBin}
  import CreateSouthDynamic.{exposure, filter, fpu, gratingConfig, readout, roi, step}
  import StepConfig.CreateStepConfig

  private def edit[A](start: A)(state: State[A, _]): A =
    state.runS(start).value

  val gmosAc: CreateSouthDynamic =
    CreateSouthDynamic(
      DurationModel.NonNegDurationInput.unsafeFromDuration(10.seconds),
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
        _ <- step.exposure                                        := DurationModel.NonNegDurationInput.unsafeFromDuration(20.seconds)
        _ <- step.instrumentConfig.andThen(readout).andThen(xBin) := GmosXBinning.One
        _ <- step.instrumentConfig.andThen(readout).andThen(yBin) := GmosYBinning.One
        _ <- step.instrumentConfig.andThen(roi)                   := GmosRoi.CentralStamp
        _ <- step.instrumentConfig.andThen(fpu)                   := GmosModel.CreateFpu.builtin[GmosSouthFpu](GmosSouthFpu.LongSlit_1_00).some
      } yield ()
    }

  val ac3: CreateStepConfig[CreateSouthDynamic] =
    (step.exposure := DurationModel.NonNegDurationInput.unsafeFromDuration(30.seconds)).runS(ac2).value

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
        _ <- exposure                 := DurationModel.NonNegDurationInput.fromSeconds(NonNegBigDecimal.unsafeFrom(950.0))
        _ <- readout.andThen(ampRead) := GmosAmpReadMode.Slow
        _ <- readout.andThen(xBin)    := GmosXBinning.Two
        _ <- readout.andThen(yBin)    := GmosYBinning.Two
        _ <- roi                      := GmosRoi.CentralSpectrum
        _ <- gratingConfig            := GmosModel.CreateGratingConfig[GmosSouthGrating](GmosSouthGrating.B600_G5323, GmosGratingOrder.One, WavelengthModel.WavelengthInput.fromNanometers(PosBigDecimal.unsafeFrom(520.0))).some
        _ <- filter                   := Option.empty[GmosSouthFilter]
        _ <- fpu                      := GmosModel.CreateFpu.builtin[GmosSouthFpu](GmosSouthFpu.LongSlit_1_00).some
      } yield ()
    }

  val gmos525: CreateSouthDynamic =
    edit(gmos520)(
      CreateSouthDynamic.instrument.wavelength := WavelengthModel.WavelengthInput.fromNanometers(PosBigDecimal.unsafeFrom(525.0))
    )

  val threeSeconds: DurationModel.NonNegDurationInput =
    DurationModel.NonNegDurationInput.fromSeconds(NonNegBigDecimal.unsafeFrom( 3.0))

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
       .map(AtomModel.Create(_))
    )

  def obs(
    pid:     Program.Id,
    targets: List[TargetModel]
  ): ObservationModel.CreateInput =
    ObservationModel.CreateInput(
      programId            = pid,
      properties           = ObservationModel.PropertiesInput(
        subtitle             = Input.ignore,
        status               = ObsStatus.New.assign,
        activeStatus         = ObsActiveStatus.Active.assign,
        targetEnvironment    = TargetEnvironmentInput.asterism(targets.map(_.id)).assign,
        constraintSet        = Input.ignore,
        scienceRequirements  = ScienceRequirementsInput.Default.assign,
        scienceMode          =
          ScienceModeInput(
            gmosSouthLongSlit = ScienceMode.GmosSouthLongSlitInput(
              BasicConfigInput[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu](
                grating   = GmosSouthGrating.B600_G5323.assign,
                fpu       = GmosSouthFpu.LongSlit_1_00.assign
              ).assign
            ).assign
          ).assign,
        manualConfig               =
          ExecutionModel.Create.gmosSouth(
            GmosModel.CreateSouthStatic.Default,
            acquisitionSequence,
            scienceSequence
          ).assign
      ).some
    )

  val proposal: ProposalInput =
    ProposalInput(
      title         = NonEmptyString.unsafeFrom("Proposal title").assign,
      proposalClass =
        ProposalClassInput(
          classical = ProposalClassInput.ClassicalInput(refineMV[ZeroTo100](80).assign).assign
        ).assign,
      category      = TacCategory.SmallBodies.assign,
      toOActivation = ToOActivation.None.assign,
      abstrakt      = NonEmptyString.unsafeFrom("Totally abstract").assign,
      partnerSplits =
        List(
          ProposalInput.PartnerSplitInput(Partner.Cl.assign, refineMV[ZeroTo100](60).assign),
          ProposalInput.PartnerSplitInput(Partner.Uh.assign, refineMV[ZeroTo100](40).assign)
        ).assign
    )

  /**
   * Initializes a (presumably) empty ODB with some demo values.
   */
  def initialize[F[_]: Sync](repo: OdbRepo[F]): F[Unit] =
    for {
      p  <- repo.program.insert(
              ProgramModel.CreateInput(
                ProgramModel.PropertiesInput(
                  NonEmptyString.unsafeFrom("The real dark matter was the friends we made along the way").assign,
                  proposal.assign
                ).some
              )
            ).map(_.program)

      p3 <- repo.program.insert(
              ProgramModel.CreateInput(
                ProgramModel.PropertiesInput(
                  NonEmptyString.unsafeFrom("An Empty Placeholder Program").assign
                ).some
              )
            ).map(_.program)
      cs <- targets(p.id).liftTo[F]
      ts <- cs.init.traverse(repo.target.insert(_).map(_.target))
      _  <- repo.observation.insert(obs(p.id, ts.headOption.toList)) // 2
      _  <- repo.observation.insert(obs(p.id, ts.lastOption.toList)) // 3
      _  <- repo.observation.insert(obs(p.id, ts.lastOption.toList)) // 4
      o  <- repo.observation.insert(obs(p.id, ts.lastOption.toList)).map(_.observation) // 5

      // Add an explicit base to the last observation's target environment
      _  <- repo.observation.update(
              ObservationModel.UpdateInput(
                ObservationModel.PropertiesInput(
                  targetEnvironment = TargetEnvironmentInput.explicitBase(
                    CoordinatesModel.Input(
                        RightAscensionModel.Input.fromDegrees(159.2583),
                        DeclinationModel.Input.fromDegrees(-27.5650)
                    )
                  ).assign
                ),
                WhereObservationInput.MatchPresent.withIds(List(o.id)).some,
                None
              )
            )

      // Add an unused target (t-5 NGC 4749)
      _  <- repo.target.insert(cs.last)

      _  <- repo.observation.insert(obs(p.id, ts)) // 6
      _  <- repo.observation.insert(obs(p.id, Nil)) // 7

      // Add an unused target for the otherwise empty program. (t-6)
      _  <- repo.target.insert(cs.last.copy(programId = p3.id))

    } yield ()

}
