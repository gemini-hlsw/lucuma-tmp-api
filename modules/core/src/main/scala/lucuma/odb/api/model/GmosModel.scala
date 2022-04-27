// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import monocle.{Focus, Lens, Optional}

import scala.concurrent.duration._
import monocle.macros.GenLens


object GmosModel {

  implicit val customConfig: Configuration = Configuration.default.withDefaults

  // --- Static Configuration ---

  final case class NodAndShuffle(
    posA:          Offset,
    posB:          Offset,
    eOffset:       GmosEOffsetting,
    shuffleOffset: Int,
    shuffleCycles: Int
  )

  object NodAndShuffle {
    val posA: Lens[NodAndShuffle, Offset]             = GenLens[NodAndShuffle](_.posA)
    val posB: Lens[NodAndShuffle, Offset]             = GenLens[NodAndShuffle](_.posB)
    val eOffset: Lens[NodAndShuffle, GmosEOffsetting] = GenLens[NodAndShuffle](_.eOffset)
    val shuffleOffset: Lens[NodAndShuffle, Int]       = GenLens[NodAndShuffle](_.shuffleOffset)
    val shuffleCycles: Lens[NodAndShuffle, Int]       = GenLens[NodAndShuffle](_.shuffleCycles)

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

  final case class CreateNodAndShuffle(
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


  sealed trait Static[S, D] {
    def mosPreImaging: MosPreImaging
    def nodAndShuffle: Option[NodAndShuffle]
    // insert customRois here
    def stageMode:     S
    def detector:      D
  }

  final case class NorthStatic(
    detector:      GmosNorthDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[NodAndShuffle],
    // insert customRois here
    stageMode:     GmosNorthStageMode
  ) extends Static[GmosNorthStageMode, GmosNorthDetector]

  object NorthStatic { //extends NorthStaticOptics {

    implicit val EqNorthStatic: Eq[NorthStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle,
        a.stageMode
      )}

  }

  final case class SouthStatic(
    detector:      GmosSouthDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[NodAndShuffle],
    // insert customRois here
    stageMode:     GmosSouthStageMode
  ) extends Static[GmosSouthStageMode, GmosSouthDetector]

  object SouthStatic { //extends SouthStaticOptics {

    implicit val EqSouthStatic: Eq[SouthStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle,
        a.stageMode
      )}

  }

  final case class CreateNorthStatic(
    detector:      GmosNorthDetector           = GmosNorthDetector.Hamamatsu,
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

  final case class CreateSouthStatic(
    detector:      GmosSouthDetector           = GmosSouthDetector.Hamamatsu,
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

  final case class CcdReadout(
    xBin:     GmosXBinning,
    yBin:     GmosYBinning,
    ampCount: GmosAmpCount,
    ampGain:  GmosAmpGain,
    ampRead:  GmosAmpReadMode
  )

  object CcdReadout extends CcdReadoutOptics {

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

  sealed trait CcdReadoutOptics { self: CcdReadout.type =>

    val xBin: Lens[CcdReadout, GmosXBinning] =
      Focus[CcdReadout](_.xBin)

    val yBin: Lens[CcdReadout, GmosYBinning] =
      Focus[CcdReadout](_.yBin)

    val ampCount: Lens[CcdReadout, GmosAmpCount] =
      Focus[CcdReadout](_.ampCount)

    val ampGain: Lens[CcdReadout, GmosAmpGain] =
      Focus[CcdReadout](_.ampGain)

    val ampRead: Lens[CcdReadout, GmosAmpReadMode] =
      Focus[CcdReadout](_.ampRead)

  }

  final case class CreateCcdReadout(
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
    val ampRead: Lens[CreateCcdReadout, GmosAmpReadMode] = GenLens[CreateCcdReadout](_.ampRead)
    val xBin: Lens[CreateCcdReadout, GmosXBinning]       = GenLens[CreateCcdReadout](_.xBin)
    val yBin: Lens[CreateCcdReadout, GmosYBinning]       = GenLens[CreateCcdReadout](_.yBin)

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

  final case class CustomMask(
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

  final case class CreateCustomMask(
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

  final case class CreateFpu[U](
    customMask: Option[CreateCustomMask],
    builtin:    Option[U]
  ) {

    val create: ValidatedInput[Either[CustomMask, U]] =
      ValidatedInput.requireOne("fpu",
        customMask.map(_.create.map(_.asLeft[U])),
        builtin.map(_.asRight[CustomMask].validNec[InputError])
      )

  }

  object CreateFpu {

    def customMask[U](m: CreateCustomMask): CreateFpu[U] =
      CreateFpu(m.some, None)

    def builtin[U](u: U): CreateFpu[U] =
      CreateFpu(None, u.some)

    implicit def DecoderCreateFpu[U: Decoder]: Decoder[CreateFpu[U]] =
      deriveDecoder[CreateFpu[U]]

    implicit def EqCreateFpu[U: Eq]: Eq[CreateFpu[U]] =
      Eq.by { a => (
        a.customMask,
        a.builtin
      )}

  }


  final case class GratingConfig[D](
    grating:    D,
    order:      GmosGratingOrder,
    wavelength: Wavelength
  )

  object GratingConfig extends GratingConfigOptics {

    implicit def EqGmosGrating[D: Eq]: Eq[GratingConfig[D]] =
      Eq.by { a => (
        a.grating,
        a.order,
        a.wavelength
      )}

  }

  sealed trait GratingConfigOptics { self: GratingConfig.type =>

    def grating[D]: Lens[GratingConfig[D], D] =
      Focus[GratingConfig[D]](_.grating)

    def order[D]: Lens[GratingConfig[D], GmosGratingOrder] =
      Focus[GratingConfig[D]](_.order)

    def wavelength[D]: Lens[GratingConfig[D], Wavelength] =
      Focus[GratingConfig[D]](_.wavelength)

  }

  final case class CreateGratingConfig[D](
    grating:    D,
    order:      GmosGratingOrder,
    wavelength: WavelengthModel.Input
  ) {

    val create: ValidatedInput[GratingConfig[D]] =
      wavelength.toWavelength("wavelength").map(GratingConfig(grating, order, _))

  }

  object CreateGratingConfig {
    def wavelength[D]: Lens[CreateGratingConfig[D], WavelengthModel.Input] =
      GenLens[CreateGratingConfig[D]](_.wavelength)

    implicit def DecoderCreateGrating[D: Decoder]: Decoder[CreateGratingConfig[D]] =
      deriveDecoder[CreateGratingConfig[D]]

    implicit def EqCreateGratingConfig[D: Eq]: Eq[CreateGratingConfig[D]] =
      Eq.by { a => (
        a.grating,
        a.order,
        a.wavelength
      )}

  }

  sealed trait Dynamic[D, L, U] {
    def exposure:      FiniteDuration
    def readout:       CcdReadout
    def dtax:          GmosDtax
    def roi:           GmosRoi
    def gratingConfig: Option[GratingConfig[D]]
    def filter:        Option[L]
    def fpu:           Option[Either[CustomMask, U]]
  }

  final case class NorthDynamic (
    exposure:      FiniteDuration,
    readout:       CcdReadout,
    dtax:          GmosDtax,
    roi:           GmosRoi,
    gratingConfig: Option[GratingConfig[GmosNorthGrating]],
    filter:        Option[GmosNorthFilter],
    fpu:           Option[Either[CustomMask, GmosNorthFpu]]
  ) extends Dynamic[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]

  object NorthDynamic extends NorthDynamicOptics {

    implicit def EqDynamic: Eq[NorthDynamic] =
      Eq.by { a => (
        a.exposure,
        a.readout,
        a.dtax,
        a.roi,
        a.gratingConfig,
        a.filter,
        a.fpu
      )}

  }

  sealed trait NorthDynamicOptics { self: NorthDynamic.type =>

    val exposure: Lens[NorthDynamic, FiniteDuration] =
      Focus[NorthDynamic](_.exposure)

    val readout: Lens[NorthDynamic, CcdReadout] =
      Focus[NorthDynamic](_.readout)

    val xBin: Lens[NorthDynamic, GmosXBinning] =
      readout andThen CcdReadout.xBin

    val yBin: Lens[NorthDynamic, GmosYBinning] =
      readout andThen CcdReadout.yBin

    val dtax: Lens[NorthDynamic, GmosDtax] =
      Focus[NorthDynamic](_.dtax)

    val roi: Lens[NorthDynamic, GmosRoi] =
      Focus[NorthDynamic](_.roi)

    val gratingConfig: Lens[NorthDynamic, Option[GratingConfig[GmosNorthGrating]]] =
      Focus[NorthDynamic](_.gratingConfig)

    val wavelength: Optional[NorthDynamic, Wavelength] =
      Optional[NorthDynamic, Wavelength](
        _.gratingConfig.map(_.wavelength)
      )(
        λ => gratingConfig.modify(_.map(GratingConfig.wavelength.replace(λ)))
      )

    val filter: Lens[NorthDynamic, Option[GmosNorthFilter]] =
      Focus[NorthDynamic](_.filter)

    val fpu: Lens[NorthDynamic, Option[Either[CustomMask, GmosNorthFpu]]] =
      Focus[NorthDynamic](_.fpu)

  }

  final case class SouthDynamic (
    exposure:      FiniteDuration,
    readout:       CcdReadout,
    dtax:          GmosDtax,
    roi:           GmosRoi,
    gratingConfig: Option[GratingConfig[GmosSouthGrating]],
    filter:        Option[GmosSouthFilter],
    fpu:           Option[Either[CustomMask, GmosSouthFpu]]
  ) extends Dynamic[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

  object SouthDynamic extends SouthDynamicOptics {

    implicit def EqDynamic: Eq[SouthDynamic] =
      Eq.by { a => (
        a.exposure,
        a.readout,
        a.dtax,
        a.roi,
        a.gratingConfig,
        a.filter,
        a.fpu
      )}

  }

  sealed trait SouthDynamicOptics { self: SouthDynamic.type =>

    val exposure: Lens[SouthDynamic, FiniteDuration] =
      Focus[SouthDynamic](_.exposure)

    val readout: Lens[SouthDynamic, CcdReadout] =
      Focus[SouthDynamic](_.readout)

    val xBin: Lens[SouthDynamic, GmosXBinning] =
      readout andThen CcdReadout.xBin

    val yBin: Lens[SouthDynamic, GmosYBinning] =
      readout andThen CcdReadout.yBin

    val dtax: Lens[SouthDynamic, GmosDtax] =
      Focus[SouthDynamic](_.dtax)

    val roi: Lens[SouthDynamic, GmosRoi] =
      Focus[SouthDynamic](_.roi)

    val gratingConfig: Lens[SouthDynamic, Option[GratingConfig[GmosSouthGrating]]] =
      Focus[SouthDynamic](_.gratingConfig)

    val wavelength: Optional[SouthDynamic, Wavelength] =
      Optional[SouthDynamic, Wavelength](
        _.gratingConfig.map(_.wavelength)
      )(
        λ => gratingConfig.modify(_.map(GratingConfig.wavelength.replace(λ)))
      )

    val filter: Lens[SouthDynamic, Option[GmosSouthFilter]] =
      Focus[SouthDynamic](_.filter)

    val fpu: Lens[SouthDynamic, Option[Either[CustomMask, GmosSouthFpu]]] =
      Focus[SouthDynamic](_.fpu)

  }

  sealed trait CreateDynamic[D, L, U] {

    def exposure:      FiniteDurationModel.Input
    def readout:       CreateCcdReadout
    def dtax:          GmosDtax
    def roi:           GmosRoi
    def gratingConfig: Option[CreateGratingConfig[D]]
    def filter:        Option[L]
    def fpu:           Option[CreateFpu[U]]

  }

  final case class CreateNorthDynamic(
    exposure:      FiniteDurationModel.Input,
    readout:       CreateCcdReadout                                = CreateCcdReadout(),
    dtax:          GmosDtax                                        = GmosDtax.Zero,
    roi:           GmosRoi                                         = GmosRoi.FullFrame,
    gratingConfig: Option[CreateGratingConfig[GmosNorthGrating]] = None,
    filter:        Option[GmosNorthFilter]                         = None,
    fpu:           Option[CreateFpu[GmosNorthFpu]]                 = None
  ) extends CreateDynamic[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    val create: ValidatedInput[NorthDynamic] =
      (
        exposure.toFiniteDuration("exposure"),
        readout.create,
        gratingConfig.traverse(_.create),
        fpu.traverse(_.create)
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
        a.gratingConfig,
        a.filter,
        a.fpu
      )}

    implicit def ValidatorNorthDynamic: InputValidator[CreateNorthDynamic, NorthDynamic] =
      InputValidator.by(_.create)

  }

  implicit val DecoderSouthFpu: Decoder[Either[CreateCustomMask, GmosSouthFpu]] =
    Decoder[CreateCustomMask].either(Decoder[GmosSouthFpu])

  final case class CreateSouthDynamic(
    exposure:      FiniteDurationModel.Input,
    readout:       CreateCcdReadout                                = CreateCcdReadout(),
    dtax:          GmosDtax                                        = GmosDtax.Zero,
    roi:           GmosRoi                                         = GmosRoi.FullFrame,
    gratingConfig: Option[CreateGratingConfig[GmosSouthGrating]] = None,
    filter:        Option[GmosSouthFilter]                         = None,
    fpu:           Option[CreateFpu[GmosSouthFpu]]                 = None
  ) extends CreateDynamic[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    val create: ValidatedInput[SouthDynamic] =
      (
        exposure.toFiniteDuration("exposure"),
        readout.create,
        gratingConfig.traverse(_.create),
        fpu.traverse(_.create)
      ).mapN { (e, r, g, u) => SouthDynamic(e, r, dtax, roi, g, filter, u) }

  }

  object CreateSouthDynamic {
    val exposure: Lens[CreateSouthDynamic, FiniteDurationModel.Input] =
      GenLens[CreateSouthDynamic](_.exposure)

    val readout: Lens[CreateSouthDynamic, CreateCcdReadout] =
      GenLens[CreateSouthDynamic](_.readout)

    val roi: Lens[CreateSouthDynamic, GmosRoi] =
      GenLens[CreateSouthDynamic](_.roi)

    val gratingConfig: Lens[CreateSouthDynamic, Option[CreateGratingConfig[GmosSouthGrating]]] =
      GenLens[CreateSouthDynamic](_.gratingConfig)

    val filter: Lens[CreateSouthDynamic, Option[GmosSouthFilter]] =
      GenLens[CreateSouthDynamic](_.filter)

    val fpu: Lens[CreateSouthDynamic, Option[CreateFpu[GmosSouthFpu]]] =
      GenLens[CreateSouthDynamic](_.fpu)

    implicit def EqCreateSouthDynamic: Eq[CreateSouthDynamic] =
      Eq.by { a => (
        a.exposure,
        a.readout,
        a.dtax,
        a.roi,
        a.gratingConfig,
        a.filter,
        a.fpu
      )}

    implicit def DecoderCreateSouthDynamic: Decoder[CreateSouthDynamic] =
      deriveDecoder[CreateSouthDynamic]

    implicit def ValidatorSouthDynamic: InputValidator[CreateSouthDynamic, SouthDynamic] =
      InputValidator.by(_.create)

    object instrument {

      val gratingConfig: Optional[CreateSouthDynamic, CreateGratingConfig[GmosSouthGrating]] =
        CreateSouthDynamic.gratingConfig.some

      val wavelength: Optional[CreateSouthDynamic, WavelengthModel.Input] =
        gratingConfig.andThen(CreateGratingConfig.wavelength[GmosSouthGrating])

    }

    object step {
      val instrumentConfig: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], CreateSouthDynamic] =
        StepConfig.CreateStepConfig.instrumentConfig[CreateSouthDynamic]

      val exposure: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], FiniteDurationModel.Input] =
        instrumentConfig.andThen(CreateSouthDynamic.exposure)

      val p: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], OffsetModel.ComponentInput] =
        StepConfig.CreateStepConfig.p[CreateSouthDynamic]

      val q: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], OffsetModel.ComponentInput] =
        StepConfig.CreateStepConfig.q[CreateSouthDynamic]

      val gratingConfig: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], CreateGratingConfig[GmosSouthGrating]] =
        instrumentConfig.andThen(instrument.gratingConfig)

      val wavelength: Optional[StepConfig.CreateStepConfig[CreateSouthDynamic], WavelengthModel.Input] =
        gratingConfig.andThen(CreateGratingConfig.wavelength[GmosSouthGrating])

    }
  }

}
