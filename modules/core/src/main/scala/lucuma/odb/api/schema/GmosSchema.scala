// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.syntax.option._
import lucuma.core.`enum`._
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.GmosModel
import sangria.marshalling.circe._
import sangria.schema._

import scala.reflect.ClassTag

object GmosSchema {

  import TimeSchema._
  import InstrumentSchema._
  import OffsetSchema._
  import WavelengthSchema._
  import syntax.`enum`._

  implicit val EnumTypeGmosAmpCount: EnumType[GmosAmpCount] =
    EnumType.fromEnumerated(
      "GmosAmpCount",
      "GMOS amp count"
    )

  implicit val EnumTypeGmosAmpGain: EnumType[GmosAmpGain] =
    EnumType.fromEnumerated(
      "GmosAmpGain",
      "GMOS amp gain"
    )

  implicit val EnumTypeGmosAmpReadMode: EnumType[GmosAmpReadMode] =
    EnumType.fromEnumerated(
      "GmosAmpReadMode",
      "GMOS amp read mode"
    )

  implicit val EnumTypeGmosCustomSlitWidth: EnumType[GmosCustomSlitWidth] =
    EnumType.fromEnumerated(
      "GmosCustomSlitWidth",
      "GMOS Custom Slit Width"
    )

  implicit val EnumTypeGmosNorthDetector: EnumType[GmosNorthDetector] =
    EnumType.fromEnumerated(
      "GmosNorthDetector",
      "GmosNorth Detector type"
    )

  implicit val EnumTypeGmosSouthDetector: EnumType[GmosSouthDetector] =
    EnumType.fromEnumerated(
      "GmosSouthDetector",
      "GmosSouth Detector type"
    )

  implicit val EnumTypeGmosDisperserOrder: EnumType[GmosDisperserOrder] =
    EnumType.fromEnumerated(
      "GmosDisperserOrder",
      "GMOS disperser order"
    )

  implicit val EnumTypeGmosDtax: EnumType[GmosDtax] =
    EnumType.fromEnumerated(
      "GmosDtax",
      "GMOS Detector Translation X Offset"
    )

  implicit val EnumTypeGmosEOffsetting: EnumType[GmosEOffsetting] =
    EnumType.fromEnumerated(
      "GmosEOffsetting",
      "Electronic offsetting"
    )

  implicit val EnumTypeGmosNorthDisperser: EnumType[GmosNorthDisperser] =
    EnumType.fromEnumerated(
      "GmosNorthDisperser",
      "GMOS North Disperser"
    )

  implicit val EnumTypeGmosNorthFilter: EnumType[GmosNorthFilter] =
    EnumType.fromEnumerated(
      "GmosNorthFilter",
      "GMOS North Filter"
    )

  implicit val EnumTypeGmosNorthFpu: EnumType[GmosNorthFpu] =
    EnumType.fromEnumerated(
      "GmosNorthBuiltinFpu",
      "GMOS North FPU"
    )

  implicit val EnumTypeGmosNorthStageMode: EnumType[GmosNorthStageMode] =
    EnumType.fromEnumerated(
      "GmosNorthStageMode",
      "GMOS North stage mode"
    )

  implicit val EnumTypeGmosRoi: EnumType[GmosRoi] =
    EnumType.fromEnumerated[GmosRoi](
      "GmosRoi",
      "GMOS Region Of Interest"
    )

  implicit val EnumTypeGmosSouthDisperser: EnumType[GmosSouthDisperser] =
    // GmosSouthDisperser.all is haunted.  B600_G5323 is inexplicably null in the list.
    // Fixed with marking it `lazy` in core but for now ....
    EnumType.fromEnumerated[GmosSouthDisperser](
      "GmosSouthDisperser",
      "GMOS South Disperser"
    )(Enumerated.of(
      GmosSouthDisperser.B1200_G5321,
      GmosSouthDisperser.R831_G5322,
      GmosSouthDisperser.B600_G5323,
      GmosSouthDisperser.R600_G5324,
      GmosSouthDisperser.B480_G5327,
      GmosSouthDisperser.R400_G5325,
      GmosSouthDisperser.R150_G5326
    ))

  implicit val EnumTypeGmosSouthFilter: EnumType[GmosSouthFilter] =
    EnumType.fromEnumerated(
      "GmosSouthFilter",
      "GMOS South Filter"
    )

  implicit val EnumTypeGmosSouthFpu: EnumType[GmosSouthFpu] =
    EnumType.fromEnumerated(
      "GmosSouthBuiltinFpu",
      "GMOS South FPU"
    )

  implicit val EnumTypeGmosSouthStageMode: EnumType[GmosSouthStageMode] =
    EnumType.fromEnumerated(
      "GmosSouthStageMode",
      "GMOS South stage mode"
    )

  implicit val EnumTypeGmosXBinning: EnumType[GmosXBinning] =
    EnumType.fromEnumerated(
      "GmosXBinning",
      "GMOS X Binning"
    )

  implicit val EnumTypeGmosYBinning: EnumType[GmosYBinning] =
    EnumType.fromEnumerated(
      "GmosYBinning",
      "GMOS Y Binning"
    )

  def GmosNodAndShuffleType: ObjectType[Any, GmosModel.NodAndShuffle] =
    ObjectType(
      name     =  "GmosNodAndShuffle",
      fieldsFn = () => fields(

        Field(
          name        = "posA",
          fieldType   = OffsetType,
          description = Some("Offset position A"),
          resolve     = _.value.posA
        ),

        Field(
          name        = "posB",
          fieldType   = OffsetType,
          description = Some("Offset position B"),
          resolve     = _.value.posB
        ),

        Field(
          name        = "eOffset",
          fieldType   = EnumTypeGmosEOffsetting,
          description = Some("Whether to use electronic offsetting"),
          resolve     = _.value.eOffset
        ),

        Field(
          name        = "shuffleOffset",
          fieldType   = IntType,
          description = Some("Shuffle offset"),
          resolve     = _.value.shuffleOffset
        ),

        Field(
          name        = "shuffleCycles",
          fieldType   = IntType,
          description = Some("Shuffle cycles"),
          resolve     = _.value.shuffleCycles
        )
      )
    )

  val InputObjectTypeGmosNodAndShuffleInput: InputObjectType[GmosModel.CreateNodAndShuffle] =

    InputObjectType[GmosModel.CreateNodAndShuffle](
      "GmosNodAndShuffleInput",
      "Creation input parameters for GMOS nod and shuffle",
      List(
        InputField("posA", InputObjectTypeOffsetInput, "Offset position A"),
        InputField("posB", InputObjectTypeOffsetInput, "Offset position B"),
        InputField("eOffset", EnumTypeGmosEOffsetting, "Electronic offsetting"),
        InputField("shuffleOffset", IntType, "Shuffle offset"),
        InputField("shuffleCycles", IntType, "Suffle cycles")
      )
    )

  private def gmos(s: Site): Instrument =
    s match {
      case Site.GN => Instrument.GmosNorth
      case Site.GS => Instrument.GmosSouth
    }

  def GmosStaticConfig[S: EnumType, D: EnumType, G <: GmosModel.Static[S, D]: ClassTag](
    site: Site
  ): ObjectType[Any, G] =
    ObjectType(
      name        = s"${gmos(site).tag}Static",
      description = "Unchanging (over the course of the sequence) configuration values",
      fieldsFn    = () => fields(

        Field(
          name        = "stageMode",
          fieldType   = implicitly[EnumType[S]],
          description = Some("Stage mode"),
          resolve     = _.value.stageMode
        ),

        Field(
          name        = "detector",
          fieldType   = implicitly[EnumType[D]],
          description = Some("Detector in use (always HAMAMATSU for recent and new observations)"),
          resolve     = _.value.detector
        ),

        Field(
          name        = "mosPreImaging",
          fieldType   = EnumTypeMosPreImaging,
          description = Some("Is MOS Pre-Imaging Observation"),
          resolve     = _.value.mosPreImaging
        ),

        Field(
          name        = "nodAndShuffle",
          fieldType   = OptionType(GmosNodAndShuffleType),
          description = Some("Nod-and-shuffle configuration"),
          resolve     = _.value.nodAndShuffle
        )

      )
    )

  val GmosNorthStaticConfigType: ObjectType[Any, GmosModel.NorthStatic] =
    GmosStaticConfig[GmosNorthStageMode, GmosNorthDetector, GmosModel.NorthStatic](Site.GN)

  val GmosSouthStaticConfigType: ObjectType[Any, GmosModel.SouthStatic] =
    GmosStaticConfig[GmosSouthStageMode, GmosSouthDetector, GmosModel.SouthStatic](Site.GS)


  val InputObjectGmosNorthStaticInput: InputObjectType[GmosModel.CreateNorthStatic] =
    InputObjectType[GmosModel.CreateNorthStatic](
      "GmosNorthStaticInput",
      "GMOS North static configuration input parameters",
      List(
        InputField("detector", EnumTypeGmosNorthDetector, "GMOS North Detector option", GmosNorthDetector.Hamamatsu: GmosNorthDetector),
        InputField("mosPreImaging", EnumTypeMosPreImaging, "Whether this is a MOS pre-imaging observation", MosPreImaging.IsNotMosPreImaging: MosPreImaging),
        InputField("nodAndShuffle", OptionInputType(InputObjectTypeGmosNodAndShuffleInput), "GMOS Nod And Shuffle configuration"),
        InputField("stageMode", EnumTypeGmosNorthStageMode, "GMOS North Stage Mode", GmosNorthStageMode.FollowXy: GmosNorthStageMode)
      )
    )

  val InputObjectGmosSouthStaticInput: InputObjectType[GmosModel.CreateSouthStatic] =
    InputObjectType[GmosModel.CreateSouthStatic](
      "GmosSouthStaticInput",
      "GMOS South static configuration input parameters",
      List(
        InputField("detector", EnumTypeGmosNorthDetector, "GMOS North Detector option", GmosSouthDetector.Hamamatsu: GmosSouthDetector),
        InputField("mosPreImaging", EnumTypeMosPreImaging, "Whether this is a MOS pre-imaging observation", MosPreImaging.IsNotMosPreImaging: MosPreImaging),
        InputField("nodAndShuffle", OptionInputType(InputObjectTypeGmosNodAndShuffleInput), "GMOS Nod And Shuffle configuration"),
        InputField("stageMode", EnumTypeGmosSouthStageMode, "GMOS North Stage Mode", GmosSouthStageMode.FollowXy: GmosSouthStageMode)
      )
    )

  val GmosCcdReadoutType: ObjectType[Any, GmosModel.CcdReadout] =
    ObjectType(
      name        = "GmosCcdMode",
      description = "CCD Readout Configuration",
      fieldsFn    = () => fields(

        Field(
          name        = "xBin",
          fieldType   = EnumTypeGmosXBinning,
          description = Some("GMOS X-binning"),
          resolve     = _.value.xBin
        ),

        Field(
          name        = "yBin",
          fieldType   = EnumTypeGmosYBinning,
          description = Some("GMOS Y-binning"),
          resolve     = _.value.yBin
        ),

        Field(
          name        = "ampCount",
          fieldType   = EnumTypeGmosAmpCount,
          description = Some("GMOS Amp Count"),
          resolve     = _.value.ampCount
        ),

        Field(
          name        = "ampGain",
          fieldType   = EnumTypeGmosAmpGain,
          description = Some("GMOS Amp Gain"),
          resolve     = _.value.ampGain
        ),

        Field(
          name        = "ampReadMode",
          fieldType   = EnumTypeGmosAmpReadMode,
          description = Some("GMOS Amp Read Mode"),
          resolve     = _.value.ampRead
        )
      )
    )

  val InputObjectTypeGmosCcdReadoutInput: InputObjectType[GmosModel.CreateCcdReadout] =

    InputObjectType[GmosModel.CreateCcdReadout](
      "GmosCcdReadoutInput",
      "GMOS CCD readout input parameters",
      List(
        InputField("xBin", EnumTypeGmosXBinning, "X Binning", GmosXBinning.One: GmosXBinning),
        InputField("yBin", EnumTypeGmosYBinning, "Y Binning", GmosYBinning.One: GmosYBinning),
        InputField("ampCount", EnumTypeGmosAmpCount, "Amp Count", GmosAmpCount.Twelve: GmosAmpCount),
        InputField("ampGain",  EnumTypeGmosAmpGain,  "Amp Gain",  GmosAmpGain.Low: GmosAmpGain),
        InputField("ampRead",  EnumTypeGmosAmpReadMode, "Amp Read Mode", GmosAmpReadMode.Slow: GmosAmpReadMode)
      )
    )

  def GmosGratingType[D: EnumType](
    site: Site
  ): ObjectType[Any, GmosModel.Grating[D]] =
    ObjectType(
      name        = s"${gmos(site).tag}Grating",
      description = s"${gmos(site).longName} Grating",
      fieldsFn    = () => fields(

        Field(
          name        = "disperser",
          fieldType   = implicitly[EnumType[D]],
          description = Some(s"${gmos(site).longName} Disperser"),
          resolve     = _.value.disperser
        ),

        Field(
          name        = "order",
          fieldType   = EnumTypeGmosDisperserOrder,
          description = Some(s"GMOS disperser order"),
          resolve     = _.value.order
        ),

        Field(
          name        = "wavelength",
          fieldType   = WavelengthType,
          description = Some(s"Grating wavelength"),
          resolve     = _.value.wavelength
        )

      )
    )

  def InputObjectTypeGratingInput[D: EnumType](
    site: Site,
  ): InputObjectType[GmosModel.CreateGrating[D]] =

    InputObjectType[GmosModel.CreateGrating[D]](
      s"Gmos${gmos(site).tag}GratingInput",
      s"GMOS ${gmos(site).longName} grating input parameters",
      List(
        InputField("disperser", implicitly[EnumType[D]], s"Gmos${gmos(site).tag} disperser"),
        InputField("order", EnumTypeGmosDisperserOrder, "GMOS disperser order"),
        InputField("wavelength", InputWavelength, "Grating wavelength")
      )
    )

  val GmosCustomMaskType: ObjectType[Any, GmosModel.CustomMask] =
    ObjectType(
      name        = "GmosCustomMask",
      description = "GMOS Custom Mask",
      fieldsFn    = () => fields(

        // TODO: I think these are supposed to be constrained to a pattern?
        Field(
          name        = "filename",
          fieldType   = StringType,
          description = Some("Custom Mask Filename"),
          resolve     = _.value.filename.value
        ),

        Field(
          name        = "slitWidth",
          fieldType   = EnumTypeGmosCustomSlitWidth,
          description = Some("Custom Slit Width"),
          resolve     = _.value.slitWidth
        )

      )
    )

  val InputObjectTypeGmosCustomMask: InputObjectType[GmosModel.CreateCustomMask] =

    InputObjectType[GmosModel.CreateCustomMask](
      "GmosCustomMaskInput",
      "GMOS custom mask input parameters",
      List(
        InputField("filename", StringType, "Custom mask file name"),
        InputField("slitWidth", EnumTypeGmosCustomSlitWidth, "Custom mask slit width")
      )
    )

  def GmosFpuType[U: EnumType](
    site: Site
  ): ObjectType[Any, Either[GmosModel.CustomMask, U]] =
    ObjectType(
      name        = s"${gmos(site).tag}Fpu",
      description = s"${gmos(site).longName} FPU option, either builtin or custom mask",
      fieldsFn    = () => fields(

        Field(
          name        = "customMask",
          fieldType   = OptionType(GmosCustomMaskType),
          description = "The custom mask, if in use".some,
          resolve     = _.value.swap.toOption
        ),

        Field(
          name        = "builtin",
          fieldType   = OptionType(implicitly[EnumType[U]]),
          description = s"${gmos(site).longName} builtin FPU, if in use".some,
          resolve     = _.value.toOption
        )
      )

    )

  def InputObjectFpuInput[U: EnumType](
    site: Site,
  ): InputObjectType[GmosModel.CreateFpu[U]] =

    InputObjectType[GmosModel.CreateFpu[U]](
      s"Gmos${gmos(site).tag}FpuInput",
      s"GMOS ${gmos(site).longName} FPU input parameters (choose custom or builtin).",
      List(
        InputField("customMask", InputObjectTypeGmosCustomMask, "Custom mask FPU option"),
        InputField("builtin", implicitly[EnumType[U]], "Builtin FPU option")
      )
    )

  def GmosDynamicType[D: EnumType, L: EnumType, U: EnumType, G <: GmosModel.Dynamic[D, L, U] : ClassTag](
    site: Site
  ): ObjectType[Any, G] =
    ObjectType(
      name        = s"${gmos(site).tag}Dynamic",
      description = s"${gmos(site).longName} dynamic step configuration",
      fieldsFn    = () => fields(

        Field(
          name        = "exposure",
          fieldType   = DurationType,
          description = Some("GMOS exposure time"),
          resolve     = _.value.exposure
        ),

        Field(
          name        = "readout",
          fieldType   = GmosCcdReadoutType,
          description = Some("GMOS CCD Readout"),
          resolve     = _.value.readout
        ),

        Field(
          name        = "dtax",
          fieldType   = EnumTypeGmosDtax,
          description = Some("GMOS detector x offset"),
          resolve     = _.value.dtax
        ),

        Field(
          name        = "roi",
          fieldType   = EnumTypeGmosRoi,
          description = Some("GMOS region of interest"),
          resolve     = _.value.roi
        ),

        Field(
          name        = "grating",
          fieldType   = OptionType(GmosGratingType[D](site)),
          description = Some(s"${gmos(site).longName} grating"),
          resolve     = _.value.grating
        ),

        Field(
          name        = "filter",
          fieldType   = OptionType(implicitly[EnumType[L]]),
          description = Some(s"${gmos(site).longName} filter"),
          resolve     = _.value.filter
        ),

        Field(
          name        = "fpu",
          fieldType   = OptionType(GmosFpuType[U](site)),
          description = Some(s"${gmos(site).longName} FPU"),
          resolve     = _.value.fpu
        )
      )
    )

  val GmosNorthDynamicType: ObjectType[Any, GmosModel.NorthDynamic] =
    GmosDynamicType[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu, GmosModel.NorthDynamic](Site.GN)

  val GmosSouthDynamicType: ObjectType[Any, GmosModel.SouthDynamic] =
    GmosDynamicType[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu, GmosModel.SouthDynamic](Site.GS)

  def InputObjectTypeGmosDynamicInput[D: EnumType, L: EnumType, U: EnumType, G <: GmosModel.CreateDynamic[D, L, U]](
    site: Site
  ): InputObjectType[G] =

    InputObjectType[G](
      s"${gmos(site).tag.capitalize}DynamicInput",
      s"${gmos(site).longName} instrument configuration input",
      List(
        InputField("exposure", InputObjectTypeDuration,            "Exposure time"),
        InputField("readout",  InputObjectTypeGmosCcdReadoutInput, "GMOS CCD readout"),
        InputField("dtax",     EnumTypeGmosDtax,                   "GMOS detector x offset"),
        InputField("roi",      EnumTypeGmosRoi,                    "GMOS region of interest"),
        InputField("grating",  OptionInputType(InputObjectTypeGratingInput[D](site)), s"${gmos(site).longName} grating"),
        InputField("filter",   OptionInputType(implicitly[EnumType[L]]),              s"${gmos(site).longName} filter"),
        InputField("fpu",      OptionInputType(InputObjectFpuInput[U](site)),         s"${gmos(site).longName} FPU")
      )
    )

  val InputObjectTypeGmosNorthDynamic: InputObjectType[GmosModel.CreateNorthDynamic] =
    InputObjectTypeGmosDynamicInput[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu, GmosModel.CreateNorthDynamic](Site.GN)

  val InputObjectTypeGmosSouthDynamic: InputObjectType[GmosModel.CreateSouthDynamic] =
    InputObjectTypeGmosDynamicInput[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu, GmosModel.CreateSouthDynamic](Site.GS)



}
