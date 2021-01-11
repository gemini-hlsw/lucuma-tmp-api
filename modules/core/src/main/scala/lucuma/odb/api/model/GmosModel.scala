// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import monocle.Lens

import scala.concurrent.duration._


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

  object NodAndShuffle extends NodAndShuffleOptics {

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

  sealed trait NodAndShuffleOptics { this: NodAndShuffle.type =>

    val posA: Lens[NodAndShuffle, Offset] =
      Lens[NodAndShuffle, Offset](_.posA)(a => _.copy(posA = a))

    val posB: Lens[NodAndShuffle, Offset] =
      Lens[NodAndShuffle, Offset](_.posB)(a => _.copy(posB = a))

    val eOffset: Lens[NodAndShuffle, GmosEOffsetting] =
      Lens[NodAndShuffle, GmosEOffsetting](_.eOffset)(a => _.copy(eOffset = a))

    val shuffleOffset: Lens[NodAndShuffle, Int] =
      Lens[NodAndShuffle, Int](_.shuffleOffset)(a => _.copy(shuffleOffset = a))

    val shuffleCycles: Lens[NodAndShuffle, Int] =
      Lens[NodAndShuffle, Int](_.shuffleCycles)(a => _.copy(shuffleCycles = a))

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
      (posA         .validateNotNullable("posA")(_.create),
       posB         .validateNotNullable("posB")(_.create),
       eOffset      .validateIsNotNull("eOffset"),
       shuffleOffset.validateNotNullable("shuffleOffset")(so => Validated.condNec(so > 0, so, InputError.fromMessage("Shuffle offset must be >= 1"))),
       shuffleCycles.validateNotNullable("shuffleCycles")(sc => Validated.condNec(sc > 0, sc, InputError.fromMessage("Shuffle cycles must be >= 1")))
      ).mapN {(a, b, e, o, c) =>
        for {
          _ <- NodAndShuffle.posA          := a
          _ <- NodAndShuffle.posB          := b
          _ <- NodAndShuffle.eOffset       := e
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

  final case class CommonStatic(
    detector:      GmosDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[NodAndShuffle]
    // insert customRois here
  )

  object CommonStatic extends CommonStaticLenses {

    implicit val EqCommonStatic: Eq[CommonStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle
      )}

  }

  sealed trait CommonStaticLenses { this: CommonStatic.type =>

    val detector: Lens[CommonStatic, GmosDetector] =
      Lens[CommonStatic, GmosDetector](_.detector)(a => _.copy(detector = a))

    val mosPreImaging: Lens[CommonStatic, MosPreImaging] =
      Lens[CommonStatic, MosPreImaging](_.mosPreImaging)(a => _.copy(mosPreImaging = a))

    val nodAndShuffle: Lens[CommonStatic, Option[NodAndShuffle]] =
      Lens[CommonStatic, Option[NodAndShuffle]](_.nodAndShuffle)(a => _.copy(nodAndShuffle = a))

  }

  final case class CreateCommonStatic(
    detector:      GmosDetector                = GmosDetector.HAMAMATSU,
    mosPreImaging: MosPreImaging               = MosPreImaging.IsNotMosPreImaging,
    nodAndShuffle: Option[CreateNodAndShuffle] = None
  ) {

    def create: ValidatedInput[CommonStatic] =
      nodAndShuffle.traverse(_.create).map { ns =>
        CommonStatic(detector, mosPreImaging, ns)
      }

  }

  object CreateCommonStatic {

    implicit val Default: CreateCommonStatic =
      CreateCommonStatic()

    implicit val DecoderCreateCommonStatic: Decoder[CreateCommonStatic] =
      deriveDecoder[CreateCommonStatic]

    implicit val EqCreateCommonStatic: Eq[CreateCommonStatic] =
      Eq.by { a => (
        a.detector,
        a.mosPreImaging,
        a.nodAndShuffle
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

  final case class Static[S](
    common:    CommonStatic,
    stageMode: S
  )

  object Static extends StaticOptics {

    implicit def EqGmosStatic[S: Eq]: Eq[Static[S]] =
      Eq.by { a => (
        a.common,
        a.stageMode
      )}

  }

  sealed trait StaticOptics { this: Static.type =>

    def common[S]: Lens[Static[S], CommonStatic] =
      Lens[Static[S], CommonStatic](_.common)(a => _.copy(common = a))

    def stageMode[S]: Lens[Static[S], S] =
      Lens[Static[S], S](_.stageMode)(a => _.copy(stageMode = a))

  }

  type NorthStatic = Static[GmosNorthStageMode]
  type SouthStatic = Static[GmosSouthStageMode]

  final case class CreateNorthStatic(
    common:    CreateCommonStatic = CreateCommonStatic.Default,
    stageMode: GmosNorthStageMode = GmosNorthStageMode.FollowXy
  ) {

    val create: ValidatedInput[NorthStatic] =
      common.create.map(Static(_, stageMode))

  }

  object CreateNorthStatic {

    implicit val DecoderCreateNorthStatic: Decoder[CreateNorthStatic] =
      deriveDecoder[CreateNorthStatic]

    implicit val EqCreateNorthStatic: Eq[CreateNorthStatic] =
      Eq.by { a => (
        a.common,
        a.stageMode
      )}

    implicit val InputValidatorCreateNorthStatic: InputValidator[CreateNorthStatic, NorthStatic] =
      InputValidator.by(_.create)

  }

  final case class CreateSouthStatic(
    common:    CreateCommonStatic = CreateCommonStatic.Default,
    stageMode: GmosSouthStageMode = GmosSouthStageMode.FollowXy
  ) {

    val create: ValidatedInput[SouthStatic] =
      common.create.map(Static(_, stageMode))

  }

  object CreateSouthStatic {

    implicit val DecoderCreateSouthStatic: Decoder[CreateSouthStatic] =
      deriveDecoder[CreateSouthStatic]

    implicit val EqCreateSouthStatic: Eq[CreateSouthStatic] =
      Eq.by { a => (
        a.common,
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

  sealed trait CcdReadoutOptics { this: CcdReadout.type =>

    val xBin: Lens[CcdReadout, GmosXBinning] =
      Lens[CcdReadout, GmosXBinning](_.xBin)(a => _.copy(xBin = a))

    val yBin: Lens[CcdReadout, GmosYBinning] =
      Lens[CcdReadout, GmosYBinning](_.yBin)(a => _.copy(yBin = a))

    val ampCount: Lens[CcdReadout, GmosAmpCount] =
      Lens[CcdReadout, GmosAmpCount](_.ampCount)(a => _.copy(ampCount = a))

    val ampGain: Lens[CcdReadout, GmosAmpGain] =
      Lens[CcdReadout, GmosAmpGain](_.ampGain)(a => _.copy(ampGain = a))

    val ampRead: Lens[CcdReadout, GmosAmpReadMode] =
      Lens[CcdReadout, GmosAmpReadMode](_.ampRead)(a => _.copy(ampRead = a))

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


  final case class CommonDynamic(
    readout:  CcdReadout,
    dtax:     GmosDtax,
    exposure: FiniteDuration,
    roi:      GmosRoi
  )

  object CommonDynamic extends CommonDynamicOptics {

    val Default: CommonDynamic =
      CommonDynamic(
        CcdReadout.Default,
        GmosDtax.Zero,
        300.seconds,
        GmosRoi.FullFrame
      )

    implicit val EqCommonDynamic: Eq[CommonDynamic] =
      Eq.by { a => (
        a.readout,
        a.dtax,
        a.exposure,
        a.roi
      )}

  }

  sealed trait CommonDynamicOptics { this: CommonDynamic.type =>

    val readout: Lens[CommonDynamic, CcdReadout] =
      Lens[CommonDynamic, CcdReadout](_.readout)(a => _.copy(readout = a))

    val dtax: Lens[CommonDynamic, GmosDtax] =
      Lens[CommonDynamic, GmosDtax](_.dtax)(a => _.copy(dtax = a))

    val exposure: Lens[CommonDynamic, FiniteDuration] =
      Lens[CommonDynamic, FiniteDuration](_.exposure)(a => _.copy(exposure = a))

    val roi: Lens[CommonDynamic, GmosRoi] =
      Lens[CommonDynamic, GmosRoi](_.roi)(a => _.copy(roi = a))

  }

  final case class CreateCommonDynamic(
    readout:  CreateCcdReadout          = CreateCcdReadout(),
    dtax:     GmosDtax                  = GmosDtax.Zero,
    exposure: FiniteDurationModel.Input = FiniteDurationModel.Input.fromSeconds(BigDecimal(300)),
    roi:      GmosRoi                   = GmosRoi.FullFrame
  ) {

    val create: ValidatedInput[CommonDynamic] = {
      (
        readout.create,
        exposure.toFiniteDuration("exposure")
      ).mapN { (r, e) => CommonDynamic(r, dtax, e, roi) }
    }

  }

  object CreateCommonDynamic {

    implicit val DecoderCreateCommonDynamic: Decoder[CreateCommonDynamic] =
      deriveDecoder[CreateCommonDynamic]

    implicit val EqCreateCommonDynamic: Eq[CreateCommonDynamic] =
      Eq.by { a => (
        a.readout,
        a.dtax,
        a.exposure,
        a.roi
      )}

  }

  final case class CustomMask(
    filename:  NonEmptyString,
    slitWidth: GmosCustomSlitWidth
  )

  object CustomMask extends CustomMaskOptics {

    implicit val EqCustomMask: Eq[CustomMask] =
      Eq.by { a => (
        a.filename.value,
        a.slitWidth
      )}

  }

  sealed trait CustomMaskOptics { this: CustomMask.type =>

    val filename: Lens[CustomMask, NonEmptyString] =
      Lens[CustomMask, NonEmptyString](_.filename)(a => _.copy(filename = a))

    val slitWidth: Lens[CustomMask, GmosCustomSlitWidth] =
      Lens[CustomMask, GmosCustomSlitWidth](_.slitWidth)(a => _.copy(slitWidth = a))

  }

  final case class CreateCustomMask(
    filename:  String,
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


  final case class Grating[D](
    disperser:  D,
    order:      GmosDisperserOrder,
    wavelength: Wavelength
  )

  object Grating extends GratingOptics {

    implicit def EqGmosGrating[D: Eq]: Eq[Grating[D]] =
      Eq.by { a => (
        a.disperser,
        a.order,
        a.wavelength
      )}

  }

  sealed trait GratingOptics { this: Grating.type =>

    def disperser[D]: Lens[Grating[D], D] =
      Lens[Grating[D], D](_.disperser)(a => _.copy(disperser = a))

    def order[D]: Lens[Grating[D], GmosDisperserOrder] =
      Lens[Grating[D], GmosDisperserOrder](_.order)(a => _.copy(order = a))

    def wavelength[D]: Lens[Grating[D], Wavelength] =
      Lens[Grating[D], Wavelength](_.wavelength)(a => _.copy(wavelength = a))

  }


  final case class CreateGrating[D](
    disperser:  D,
    order:      GmosDisperserOrder,
    wavelength: WavelengthModel.Input
  ) {

    val create: ValidatedInput[Grating[D]] =
      wavelength.toWavelength("wavelength").map(Grating(disperser, order, _))

  }

  object CreateGrating {

    implicit def DecoderCreateGrating[D: Decoder]: Decoder[CreateGrating[D]] =
      deriveDecoder[CreateGrating[D]]

    implicit def EqCreateGrating[D: Eq]: Eq[CreateGrating[D]] =
      Eq.by { a => (
        a.disperser,
        a.order,
        a.wavelength
      )}

  }

  final case class Dynamic[D, L, U](
    common:  CommonDynamic,
    grating: Option[Grating[D]],
    filter:  Option[L],
    fpu:     Option[Either[CustomMask, U]]
  )

  object Dynamic extends DynamicOptics {

    implicit def EqDynamic[D: Eq, L: Eq, U: Eq]: Eq[Dynamic[D, L, U]] =
      Eq.by { a => (
        a.common,
        a.grating,
        a.filter,
        a.fpu
      )}

  }

  sealed trait DynamicOptics { this: Dynamic.type =>

    def common[D, L, U]: Lens[Dynamic[D, L, U], CommonDynamic] =
      Lens[Dynamic[D, L, U], CommonDynamic](_.common)(a => _.copy(common = a))

    def grating[D, L, U]: Lens[Dynamic[D, L, U], Option[Grating[D]]] =
      Lens[Dynamic[D, L, U], Option[Grating[D]]](_.grating)(a => _.copy(grating = a))

    def filter[D, L, U]: Lens[Dynamic[D, L, U], Option[L]] =
      Lens[Dynamic[D, L, U], Option[L]](_.filter)(a => _.copy(filter = a))

    def fpu[D, L, U]: Lens[Dynamic[D, L, U], Option[Either[CustomMask, U]]] =
      Lens[Dynamic[D, L, U], Option[Either[CustomMask, U]]](_.fpu)(a => _.copy(fpu = a))

  }

  type NorthDynamic = Dynamic[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]
  type SouthDynamic = Dynamic[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]


  implicit val DecoderNorthFpu: Decoder[Either[CreateCustomMask, GmosNorthFpu]] =
    Decoder[CreateCustomMask].either(Decoder[GmosNorthFpu])

  final case class CreateNorthDynamic(
    common:  CreateCommonDynamic,
    grating: Option[CreateGrating[GmosNorthDisperser]],
    filter:  Option[GmosNorthFilter],
    fpu:     Option[Either[CreateCustomMask, GmosNorthFpu]]
  ) {

    val create: ValidatedInput[NorthDynamic] =
      (
        common.create,
        grating.traverse(_.create),
        fpu.traverse(_.fold(_.create.map(_.asLeft[GmosNorthFpu]), _.asRight[CustomMask].validNec[InputError]))
      ).mapN { (c, g, u) => Dynamic(c, g, filter, u) }

  }

  object CreateNorthDynamic {

    implicit def DecoderCreateNorthDynamic: Decoder[CreateNorthDynamic] =
      deriveDecoder[CreateNorthDynamic]

    implicit def EqCreateNorthDynamic: Eq[CreateNorthDynamic] =
      Eq.by { a => (
        a.common,
        a.grating,
        a.filter,
        a.fpu
      )}

    implicit def ValidatorNorthDynamic: InputValidator[CreateNorthDynamic, NorthDynamic] =
      InputValidator.by(_.create)

  }

  implicit val DecoderSouthFpu: Decoder[Either[CreateCustomMask, GmosSouthFpu]] =
    Decoder[CreateCustomMask].either(Decoder[GmosSouthFpu])

  final case class CreateSouthDynamic(
    common:  CreateCommonDynamic,
    grating: Option[CreateGrating[GmosSouthDisperser]],
    filter:  Option[GmosSouthFilter],
    fpu:     Option[Either[CreateCustomMask, GmosSouthFpu]]
  ) {

    val create: ValidatedInput[SouthDynamic] =
      (
        common.create,
        grating.traverse(_.create),
        fpu.traverse(_.fold(_.create.map(_.asLeft[GmosSouthFpu]), _.asRight[CustomMask].validNec[InputError]))
      ).mapN { (c, g, u) => Dynamic(c, g, filter, u) }

  }

  object CreateSouthDynamic {

    implicit def EqCreateSouthDynamic: Eq[CreateSouthDynamic] =
      Eq.by { a => (
        a.common,
        a.grating,
        a.filter,
        a.fpu
      )}

    implicit def DecoderCreateSouthDynamic: Decoder[CreateSouthDynamic] =
      deriveDecoder[CreateSouthDynamic]

    implicit def ValidatorSouthDynamic: InputValidator[CreateSouthDynamic, SouthDynamic] =
      InputValidator.by(_.create)

  }

}
