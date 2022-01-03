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
import lucuma.core.math.BrightnessUnits.{Brightness, Integrated}
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional.TaggedUnit
import lucuma.core.math.units.{ABMagnitude, VegaMagnitude}
import lucuma.core.model.{BandBrightness, Program, SourceProfile, SpectralDefinition, UnnormalizedSED}
import lucuma.odb.api.model.OffsetModel.ComponentInput

import scala.collection.immutable.SortedMap
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
  "parallax":       { "milliarcseconds":  0.00 }
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
  "parallax":       { "milliarcseconds":  0.00 }
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
  "parallax":       { "milliarcseconds":  0.0 }
}
"""
  )

  private def bandBrightness[U](bv: Double, b: Band, e: Option[Double])(
    implicit ev: TaggedUnit[U, Brightness[Integrated]]
  ): BandBrightness[Integrated] =
    BandBrightness[Integrated, U](
      BrightnessValue.fromDouble(bv),
      b,
      e.map(BrightnessValue.fromDouble)
    )

  private def pointSpiral(
    brightnesses: BandBrightness[Integrated]*
  ): SourceProfile =
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral),
          SortedMap.from[Band, BandBrightness[Integrated]](brightnesses.fproductLeft(_.band))
      )
    )

  val ngc5949Profile: SourceProfile =
    pointSpiral(
      bandBrightness[VegaMagnitude](12.700, Band.B,      None       ),
      bandBrightness[VegaMagnitude](10.279, Band.J,      0.0009.some),
      bandBrightness[VegaMagnitude]( 9.649, Band.H,      0.0120.some),
      bandBrightness[VegaMagnitude]( 9.425, Band.K,      0.0170.some),
      bandBrightness[ABMagnitude](  14.147, Band.SloanU, 0.0050.some),
      bandBrightness[ABMagnitude](  12.924, Band.SloanG, 0.0020.some),
      bandBrightness[ABMagnitude](  12.252, Band.SloanR, 0.0020.some),
      bandBrightness[ABMagnitude](  11.888, Band.SloanI, 0.0020.some),
      bandBrightness[ABMagnitude](  11.636, Band.SloanZ, 0.0020.some)
    )

  val ngc3369Profile: SourceProfile =
    pointSpiral(
      bandBrightness[VegaMagnitude](13.240, Band.B,      None       ),
      bandBrightness[VegaMagnitude](13.510, Band.V,      None       ),
      bandBrightness[VegaMagnitude](11.730, Band.R,      None       ),
      bandBrightness[VegaMagnitude]( 9.958, Band.J,      0.018.some),
      bandBrightness[VegaMagnitude]( 9.387, Band.H,      0.024.some),
      bandBrightness[VegaMagnitude]( 9.055, Band.K,      0.031.some)
    )

  val ngc3312Profile: SourceProfile =
    pointSpiral(
      bandBrightness[VegaMagnitude](12.630, Band.B,      None       ),
      bandBrightness[VegaMagnitude](13.960, Band.V,      None       ),
      bandBrightness[VegaMagnitude]( 9.552, Band.J,      0.016.some),
      bandBrightness[VegaMagnitude]( 8.907, Band.H,      0.017.some),
      bandBrightness[VegaMagnitude]( 8.665, Band.K,      0.028.some)
    )

  val targets: Either[Exception, List[(CreateSiderealInput, SourceProfile)]] =
    targetsJson.traverse(decode[CreateSiderealInput]).map(
      _.zip(List(ngc5949Profile, ngc3369Profile, ngc3312Profile))
    )

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
      ts <- cs.traverse { case (c, prof) =>
        repo.target.insert(TargetModel.Create(None, p.id, Some(c), None), prof.some)
      }
      _  <- repo.observation.insert(obs(p.id, ts.headOption))
      _  <- repo.observation.insert(obs(p.id, ts.lastOption))
      _  <- repo.observation.insert(obs(p.id, None))
    } yield ()

}
