// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`._
import lucuma.odb.api.model.GmosModel
import lucuma.odb.api.repo.OdbRepo
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
      "GmosAmpCount",
      "GMOS amp count"
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
      "GmosNorthFpu",
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
    EnumType.fromEnumerated(
      "GmosSouthDisperser",
      "GMOS South Disperser"
    )

  implicit val EnumTypeGmosSouthFilter: EnumType[GmosSouthFilter] =
    EnumType.fromEnumerated(
      "GmosSouthFilter",
      "GMOS South Filter"
    )

  implicit val EnumTypeGmosSouthFpu: EnumType[GmosSouthFpu] =
    EnumType.fromEnumerated(
      "GmosSouthFpu",
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

  def GmosNodAndShuffleType[F[_]]: ObjectType[OdbRepo[F], GmosModel.NodAndShuffle] =
    ObjectType(
      name     =  "GmosNodAndShuffle",
      fieldsFn = () => fields(

        Field(
          name        = "posA",
          fieldType   = OffsetType[F],
          description = Some("Offset position A"),
          resolve     = _.value.posA
        ),

        Field(
          name        = "posB",
          fieldType   = OffsetType[F],
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

  private def gmos(s: Site): Instrument =
    s match {
      case Site.GN => Instrument.GmosNorth
      case Site.GS => Instrument.GmosSouth
    }

  def GmosStaticConfig[F[_], S: EnumType, D: EnumType, G <: GmosModel.Static[S, D]: ClassTag](
    site: Site
  ): ObjectType[OdbRepo[F], G] =
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
          fieldType   = OptionType(GmosNodAndShuffleType[F]),
          description = Some("Nod-and-shuffle configuration"),
          resolve     = _.value.nodAndShuffle
        )

      )
    )

  def GmosNorthStaticConfigType[F[_]]: ObjectType[OdbRepo[F], GmosModel.NorthStatic] =
    GmosStaticConfig[F, GmosNorthStageMode, GmosNorthDetector, GmosModel.NorthStatic](Site.GN)

  def GmosSouthStaticConfigType[F[_]]: ObjectType[OdbRepo[F], GmosModel.SouthStatic] =
    GmosStaticConfig[F, GmosSouthStageMode, GmosSouthDetector, GmosModel.SouthStatic](Site.GS)

  def GmosCcdReadoutType[F[_]]: ObjectType[OdbRepo[F], GmosModel.CcdReadout] =
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

  def GmosCustomMaskType[F[_]]: ObjectType[OdbRepo[F], GmosModel.CustomMask] =
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

  def GmosGratingType[F[_], D: EnumType](
    site: Site
  ): ObjectType[OdbRepo[F], GmosModel.Grating[D]] =
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
          fieldType   = WavelengthType[F],
          description = Some(s"Grating wavelength"),
          resolve     = _.value.wavelength
        )

      )
    )

  def GmosBuiltinFpuType[F[_], U: EnumType: ClassTag](
    site: Site
  ): ObjectType[OdbRepo[F], U] =
    ObjectType(
      name        = s"${gmos(site).tag}BuiltinFpu",
      description = s"${gmos(site).longName} builtin-in FPU",
      fieldsFn    = () => fields(

        Field(
          name        = "builtin",
          fieldType   = implicitly[EnumType[U]],
          description = Some(s"${gmos(site).longName} builtin-fpu"),
          resolve     = _.value
        )
      )
    )

  def GmosFpuUnionType[F[_], U: EnumType: ClassTag](
    site: Site
  ): OutputType[Either[GmosModel.CustomMask, U]] =
    UnionType(
      name        = s"${gmos(site).tag}FpuUnion",
      description = Some("Either custom mask or builtin-FPU"),
      types       = List(GmosCustomMaskType[F], GmosBuiltinFpuType[F, U](site))
    ).mapValue[Either[GmosModel.CustomMask, U]](
      _.fold(
        cm => cm: Any,
        u  =>  u: Any
      )
    )

  def GmosDynamicType[F[_], D: EnumType, L: EnumType, U: EnumType: ClassTag, G <: GmosModel.Dynamic[D, L, U] : ClassTag](
    site: Site
  ): ObjectType[OdbRepo[F], G] =
    ObjectType(
      name        = s"${gmos(site).tag}Dynamic",
      description = s"${gmos(site).longName} dynamic step configuration",
      fieldsFn    = () => fields(

        Field(
          name        = "exposure",
          fieldType   = DurationType[F],
          description = Some("GMOS exposure time"),
          resolve     = _.value.exposure
        ),

        Field(
          name        = "readout",
          fieldType   = GmosCcdReadoutType[F],
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
          fieldType   = OptionType(GmosGratingType[F,D](site)),
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
          fieldType   = OptionType(GmosFpuUnionType[F, U](site)),
          description = Some(s"${gmos(site).longName} FPU"),
          resolve     = _.value.fpu
        )
      )
    )

  def GmosNorthDynamicType[F[_]]: ObjectType[OdbRepo[F], GmosModel.NorthDynamic] =
    GmosDynamicType[F, GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu, GmosModel.NorthDynamic](Site.GN)

  def GmosSouthDynamicType[F[_]]: ObjectType[OdbRepo[F], GmosModel.SouthDynamic] =
    GmosDynamicType[F, GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu, GmosModel.SouthDynamic](Site.GS)

}
