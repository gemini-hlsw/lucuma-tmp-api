// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`._
import lucuma.core.math.{Offset, Wavelength}
import lucuma.core.optics.syntax.lens._
import lucuma.odb.api.model.syntax.input._
import cats.Eq
import cats.data.{State, Validated}
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.types.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.generic.extras.Configuration
import monocle.Optional
import monocle.macros.Lenses
import monocle.std.option.some

import scala.concurrent.duration._
import scala.annotation.nowarn


object GmosModel {

  implicit val customConfig: Configuration = Configuration.default.withDefaults

  // --- Static Configuration ---

  @Lenses final case class NodAndShuffle(
    posA:          Offset,
    posB:          Offset,
    eOffset:       GmosEOffsetting,
    shuffleOffset: Int,
    shuffleCycles: Int
  )

  object NodAndShuffle {

    val Default: NodAndShuffle =
      NodAndShuffle(
        Offset.Zero,
        Offset.Zero,
        GmosEOffsetting.Off,
        1,
        1
      )

    implicit val EqNodAndShuffle: Eq[NodAndShuffle] =
      Eq.by { a => (
        a.posA,
        a.posB,
        a.eOffset,
        a.shuffleOffset,
        a.shuffleCycles
      )}

  }

  @Lenses final case class CreateNodAndShuffle(
    posA:          OffsetModel.Input,
    posB:          OffsetModel.Input,
    eOffset:       GmosEOffsetting,
    shuffleOffset: Int,
    shuffleCycles: Int
  ) {

    val create: ValidatedInput[NodAndShuffle] =
      (posA.create,
        posB.create,
        Validated.condNec(shuffleOffset > 0, shuffleOffset, InputError.fromMessage("Shuffle offset must be >= 1")),
        Validated.condNec(shuffleCycles > 0, shuffleCycles, InputError.fromMessage("Shuffle cycles must be >= 1"))
        ).mapN { (a, b, so, sc) => NodAndShuffle(a, b, eOffset, so, sc) }

  }

  object CreateNodAndShuffle {

    implicit val DecoderCreateNodAndShuffle: Decoder[CreateNodAndShuffle] =
      deriveDecoder[CreateNodAndShuffle]

    implicit val EqCreateNodAndShuffle: Eq[CreateNodAndShuffle] =
      Eq.by { a => (
        a.posA,
        a.posB,
        a.eOffset,
        a.shuffleOffset,
        a.shuffleCycles
      )}

  }

  final case class EditNodAndShuffle(
    posA:          Input[OffsetModel.Input] = Input.ignore,
    posB:          Input[OffsetModel.Input] = Input.ignore,
    eOffset:       Input[GmosEOffsetting]   = Input.ignore,
    shuffleOffset: Input[Int]               = Input.ignore,
    shuffleCycles: Input[Int]               = Input.ignore
  ) {

    val editor: ValidatedInput[State[NodAndShuffle, Unit]] =
      (posA.validateNotNullable("posA")(_.create),
        posB.validateNotNullable("posB")(_.create),
        eOffset.validateIsNotNull("eOffset"),
        shuffleOffset.validateNotNullable("shuffleOffset")(so => Validated.condNec(so > 0, so, InputError.fromMessage("Shuffle offset must be >= 1"))),
        shuffleCycles.validateNotNullable("shuffleCycles")(sc => Validated.condNec(sc > 0, sc, InputError.fromMessage("Shuffle cycles must be >= 1")))
        ).mapN { (a, b, e, o, c) =>
        for {
          _ <- NodAndShuffle.posA := a
          _ <- NodAndShuffle.posB := b
          _ <- NodAndShuffle.eOffset := e
          _ <- NodAndShuffle.shuffleOffset := o
          _ <- NodAndShuffle.shuffleCycles := c
        } yield ()
      }

  }

  object EditNodAndShuffle {

    implicit val DecoderEditNodAndShuffle: Decoder[EditNodAndShuffle] =
      deriveConfiguredDecoder[EditNodAndShuffle]

    implicit val EqEditNodAndShuffle: Eq[EditNodAndShuffle] =
      Eq.by { a => (
        a.posA,
        a.posB,
        a.eOffset,
        a.shuffleOffset,
        a.shuffleCycles
      )}

  }



  /*

  Not clear how to do an editor for a hierarchy.  For example if the config has
  no NodAndShuffle to edit then what do you do?

  final case class EditCommonStatic(
    detector:      Input[GmosDetector]                                   = Input.ignore,
    mosPreImaging: Input[MosPreImaging]                                  = Input.ignore,
    nodAndShuffle: Input[Either[CreateNodAndShuffle, EditNodAndShuffle]] = Input.ignore
  ) {

    val editor: ValidatedInput[State[CommonStatic, Unit]] =
      (detector     .validateIsNotNull("detector"),
       mosPreImaging.validateIsNotNull("mosPreImaging"),
       nodAndShuffle.validateNullable {
         _.fold(
           _.create.map(_.asLeft[State[NodAndShuffle, Unit]]), // ValidatedInput[Either[NodAndShuffle, State[NodAndShuffle, Unit]]
           _.editor.map(_.asRight[NodAndShuffle])              // ValidatedInput[Either[NodAndShuffle, State[NodAndShuffle, Unit]]
         )
       }
      ).mapN { (d, p, e) =>
        for {
          _ <- CommonStatic.detector      := d
          _ <- CommonStatic.mosPreImaging := p
          _ <- CommonStatic.nodAndShuffle.mod_ { nso => ??? }
        } yield ()
      }

  }

  object EditCommonStatic {

    implicit val DecoderEditCommonStatic: Decoder[EditCommonStatic] =
      deriveDecoder[EditCommonStatic]

    implicit val EqEditCommonStatic: Eq[EditCommonStatic] =
      Eq.by { e => (
        e.detector,
        e.mosPreImaging,
        e.nodAndShuffle
      )}

  }
   */

  sealed trait Static[S] {
    def detector:      GmosDetector
    def mosPreImaging: MosPreImaging
    def nodAndShuffle: Option[NodAndShuffle]
    // insert customRois here
    def stageMode:     S
  }

  @Lenses final case class NorthStatic(
    detector:      GmosDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[NodAndShuffle],
    // insert customRois here
    stageMode:     GmosNorthStageMode
  ) extends Static[GmosNorthStageMode]

  object NorthStatic { //extends NorthStaticOptics {

    implicit val EqNorthStatic: Eq[NorthStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle,
        a.stageMode
      )}

  }

  @Lenses final case class SouthStatic(
    detector:      GmosDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[NodAndShuffle],
    // insert customRois here
    stageMode:     GmosSouthStageMode
  ) extends Static[GmosSouthStageMode]

  object SouthStatic { //extends SouthStaticOptics {

    implicit val EqSouthStatic: Eq[SouthStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle,
        a.stageMode
      )}

  }

  @Lenses final case class CreateNorthStatic(
    detector:      GmosDetector                = GmosDetector.HAMAMATSU,
    mosPreImaging: MosPreImaging               = MosPreImaging.IsNotMosPreImaging,
    nodAndShuffle: Option[CreateNodAndShuffle] = None,
    stageMode:     GmosNorthStageMode          = GmosNorthStageMode.FollowXy
  ) {

    val create: ValidatedInput[NorthStatic] =
      nodAndShuffle.traverse(_.create).map { ns =>
        NorthStatic(detector, mosPreImaging, ns, stageMode)
      }

  }

  object CreateNorthStatic {

    implicit val DecoderCreateNorthStatic: Decoder[CreateNorthStatic] =
      deriveDecoder[CreateNorthStatic]

    implicit val EqCreateNorthStatic: Eq[CreateNorthStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle,
        a.stageMode
      )}

    implicit val InputValidatorCreateNorthStatic: InputValidator[CreateNorthStatic, NorthStatic] =
      InputValidator.by(_.create)

  }

  @Lenses final case class CreateSouthStatic(
    detector:      GmosDetector                = GmosDetector.HAMAMATSU,
    mosPreImaging: MosPreImaging               = MosPreImaging.IsNotMosPreImaging,
    nodAndShuffle: Option[CreateNodAndShuffle] = None,
    stageMode:     GmosSouthStageMode          = GmosSouthStageMode.FollowXy
  ) {

    val create: ValidatedInput[SouthStatic] =
      nodAndShuffle.traverse(_.create).map { ns =>
        SouthStatic(detector, mosPreImaging, ns, stageMode)
      }

  }

  object CreateSouthStatic {

    val Default: CreateSouthStatic =
      new CreateSouthStatic()

    implicit val DecoderCreateSouthStatic: Decoder[CreateSouthStatic] =
      deriveDecoder[CreateSouthStatic]

    implicit val EqCreateSouthStatic: Eq[CreateSouthStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle,
        a.stageMode
      )}

    implicit val InputValidatorCreateSouthStatic: InputValidator[CreateSouthStatic, SouthStatic] =
      InputValidator.by(_.create)

  }

  // --- Dynamic Configuration ---

  @Lenses final case class CcdReadout(
    xBin:     GmosXBinning,
    yBin:     GmosYBinning,
    ampCount: GmosAmpCount,
    ampGain:  GmosAmpGain,
    ampRead:  GmosAmpReadMode
  )

  object CcdReadout { //extends CcdReadoutOptics {

    val Default: CcdReadout =
      CcdReadout(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Slow
      )

    implicit val EqCcdReadout: Eq[CcdReadout] =
      Eq.by { a => (
        a.xBin,
        a.yBin,
        a.ampCount,
        a.ampGain,
        a.ampRead
      )}

  }

  @Lenses final case class CreateCcdReadout(
    xBin:     GmosXBinning    = GmosXBinning.One,
    yBin:     GmosYBinning    = GmosYBinning.One,
    ampCount: GmosAmpCount    = GmosAmpCount.Twelve,
    ampGain:  GmosAmpGain     = GmosAmpGain.Low,
    ampRead:  GmosAmpReadMode = GmosAmpReadMode.Slow
  ) {

    val create: ValidatedInput[CcdReadout] =
      CcdReadout(xBin, yBin, ampCount, ampGain, ampRead).validNec[InputError]

  }

  object CreateCcdReadout {

    implicit val DecoderCreateCcdReadout: Decoder[CreateCcdReadout] =
      deriveDecoder[CreateCcdReadout]

    implicit val EqCreateCcdReadout: Eq[CreateCcdReadout] =
      Eq.by { a => (
        a.xBin,
        a.yBin,
        a.ampCount,
        a.ampGain,
        a.ampRead
      )}

  }

  @Lenses final case class CustomMask(
    filename: NonEmptyString,
    slitWidth: GmosCustomSlitWidth
  )

  object CustomMask {

    implicit val EqCustomMask: Eq[CustomMask] =
      Eq.by { a => (
        a.filename.value,
        a.slitWidth
      )}

  }

  @Lenses final case class CreateCustomMask(
    filename: String,
    slitWidth: GmosCustomSlitWidth
  ) {

    val create: ValidatedInput[CustomMask] =
      ValidatedInput.nonEmptyString("filename", filename).map(CustomMask(_, slitWidth))

  }

  object CreateCustomMask {

    implicit val DecoderCreateCustomMask: Decoder[CreateCustomMask] =
      deriveDecoder[CreateCustomMask]

    implicit val EqCreateCustomMask: Eq[CreateCustomMask] =
      Eq.by { a => (
        a.filename,
        a.slitWidth
      )}

  }


  @Lenses final case class Grating[D](
    disperser:  D,
    order:      GmosDisperserOrder,
    wavelength: Wavelength
  )

  object Grating {

    implicit def EqGmosGrating[D: Eq]: Eq[Grating[D]] =
      Eq.by { a => (
        a.disperser,
        a.order,
        a.wavelength
      )}

  }

  @Lenses final case class CreateGrating[D](
    disperser:  D,
    order:      GmosDisperserOrder,
    wavelength: WavelengthModel.Input
  ) {

    val create: ValidatedInput[Grating[D]] =
      wavelength.toWavelength("wavelength").map(Grating(disperser, order, _))

  }

  object CreateGrating {

    @nowarn
    implicit def DecoderCreateGrating[D: Decoder]: Decoder[CreateGrating[D]] =
      deriveDecoder[CreateGrating[D]]

    implicit def EqCreateGrating[D: Eq]: Eq[CreateGrating[D]] =
      Eq.by { a => (
        a.disperser,
        a.order,
        a.wavelength
      )}

  }

  sealed trait Dynamic[D, L, U] {
    def exposure: FiniteDuration
    def readout:  CcdReadout
    def dtax:     GmosDtax
    def roi:      GmosRoi
    def grating:  Option[Grating[D]]
    def filter:   Option[L]
    def fpu:      Option[Either[CustomMask, U]]
  }

  @Lenses final case class NorthDynamic (
    exposure: FiniteDuration,
    readout:  CcdReadout,
    dtax:     GmosDtax,
    roi:      GmosRoi,
    grating:  Option[Grating[GmosNorthDisperser]],
    filter:   Option[GmosNorthFilter],
    fpu:      Option[Either[CustomMask, GmosNorthFpu]]
  ) extends Dynamic[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]

  object NorthDynamic { // extends NorthDynamicOptics {

    implicit def EqDynamic: Eq[NorthDynamic] =
      Eq.by { a => (
        a.exposure,
        a.readout,
        a.dtax,
        a.roi,
        a.grating,
        a.filter,
        a.fpu
      )}

  }

  @Lenses final case class SouthDynamic (
    exposure: FiniteDuration,
    readout:  CcdReadout,
    dtax:     GmosDtax,
    roi:      GmosRoi,
    grating:  Option[Grating[GmosSouthDisperser]],
    filter:   Option[GmosSouthFilter],
    fpu:      Option[Either[CustomMask, GmosSouthFpu]]
  ) extends Dynamic[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]

  object SouthDynamic { //extends SouthDynamicOptics {

    implicit def EqDynamic: Eq[SouthDynamic] =
      Eq.by { a => (
        a.exposure,
        a.readout,
        a.dtax,
        a.roi,
        a.grating,
        a.filter,
        a.fpu
      )}

  }


  implicit val DecoderNorthFpu: Decoder[Either[CreateCustomMask, GmosNorthFpu]] =
    Decoder[CreateCustomMask].either(Decoder[GmosNorthFpu])

  @Lenses final case class CreateNorthDynamic(
    exposure: FiniteDurationModel.Input,
    readout:  CreateCcdReadout                               = CreateCcdReadout(),
    dtax:     GmosDtax                                       = GmosDtax.Zero,
    roi:      GmosRoi                                        = GmosRoi.FullFrame,
    grating:  Option[CreateGrating[GmosNorthDisperser]]      = None,
    filter:   Option[GmosNorthFilter]                        = None,
    fpu:      Option[Either[CreateCustomMask, GmosNorthFpu]] = None
  ) {

    val create: ValidatedInput[NorthDynamic] =
      (
        exposure.toFiniteDuration("exposure"),
        readout.create,
        grating.traverse(_.create),
        fpu.traverse(_.fold(_.create.map(_.asLeft[GmosNorthFpu]), _.asRight[CustomMask].validNec[InputError]))
      ).mapN { (e, r, g, u) => NorthDynamic(e, r, dtax, roi, g, filter, u) }

  }

  object CreateNorthDynamic {

    implicit def DecoderCreateNorthDynamic: Decoder[CreateNorthDynamic] =
      deriveDecoder[CreateNorthDynamic]

    implicit def EqCreateNorthDynamic: Eq[CreateNorthDynamic] =
      Eq.by { a => (
        a.exposure,
        a.readout,
        a.dtax,
        a.roi,
        a.grating,
        a.filter,
        a.fpu
      )}

    implicit def ValidatorNorthDynamic: InputValidator[CreateNorthDynamic, NorthDynamic] =
      InputValidator.by(_.create)

  }

  implicit val DecoderSouthFpu: Decoder[Either[CreateCustomMask, GmosSouthFpu]] =
    Decoder[CreateCustomMask].either(Decoder[GmosSouthFpu])

  @Lenses final case class CreateSouthDynamic(
    exposure: FiniteDurationModel.Input,
    readout:  CreateCcdReadout                               = CreateCcdReadout(),
    dtax:     GmosDtax                                       = GmosDtax.Zero,
    roi:      GmosRoi                                        = GmosRoi.FullFrame,
    grating:  Option[CreateGrating[GmosSouthDisperser]]      = None,
    filter:   Option[GmosSouthFilter]                        = None,
    fpu:      Option[Either[CreateCustomMask, GmosSouthFpu]] = None
  ) {

    val create: ValidatedInput[SouthDynamic] =
      (
        exposure.toFiniteDuration("exposure"),
        readout.create,
        grating.traverse(_.create),
        fpu.traverse(_.fold(_.create.map(_.asLeft[GmosSouthFpu]), _.asRight[CustomMask].validNec[InputError]))
      ).mapN { (e, r, g, u) => SouthDynamic(e, r, dtax, roi, g, filter, u) }

  }

  object CreateSouthDynamic {

    implicit def EqCreateSouthDynamic: Eq[CreateSouthDynamic] =
      Eq.by { a => (
        a.exposure,
        a.readout,
        a.dtax,
        a.roi,
        a.grating,
        a.filter,
        a.fpu
      )}

    implicit def DecoderCreateSouthDynamic: Decoder[CreateSouthDynamic] =
      deriveDecoder[CreateSouthDynamic]

    implicit def ValidatorSouthDynamic: InputValidator[CreateSouthDynamic, SouthDynamic] =
      InputValidator.by(_.create)

    object instrument {

      val grating: Optional[CreateSouthDynamic, CreateGrating[GmosSouthDisperser]] =
        CreateSouthDynamic.grating ^<-? some

      val wavelength: Optional[CreateSouthDynamic, WavelengthModel.Input] =
        grating ^|-> CreateGrating.wavelength[GmosSouthDisperser]

    }

    object step {
      val instrumentConfig: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], CreateSouthDynamic] =
        StepConfig.CreateStepConfig.instrumentConfig[CreateSouthDynamic]

      val exposure: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], FiniteDurationModel.Input] =
        instrumentConfig ^|-> CreateSouthDynamic.exposure

      val p: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], OffsetModel.ComponentInput] =
        StepConfig.CreateStepConfig.p[CreateSouthDynamic]

      val q: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], OffsetModel.ComponentInput] =
        StepConfig.CreateStepConfig.q[CreateSouthDynamic]

      val grating: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], CreateGrating[GmosSouthDisperser]] =
        instrumentConfig ^|-? instrument.grating

      val wavelength: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], WavelengthModel.Input] =
        grating ^|-> CreateGrating.wavelength[GmosSouthDisperser]

    }
  }

}
