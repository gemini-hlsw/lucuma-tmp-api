// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.`enum`._
import lucuma.odb.api.model.GmosModel
import lucuma.odb.api.repo.OdbRepo
import cats.effect.Effect
import sangria.schema._

import scala.reflect.ClassTag

object GmosSchema {

  import FiniteDurationSchema._
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

  implicit val EnumTypeGmosDetector: EnumType[GmosDetector] =
    EnumType.fromEnumerated(
      "GmosDetector",
      "Detector type"
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

  def GmosNodAndShuffleType[F[_]: Effect]: ObjectType[OdbRepo[F], GmosModel.NodAndShuffle] =
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

  private def location(s: Site): String =
    s match {
      case Site.GN => "North"
      case Site.GS => "South"
    }

  def GmosStaticConfig[F[_]: Effect, S: EnumType](
    site: Site
  ): ObjectType[OdbRepo[F], GmosModel.Static[S]] =
    ObjectType(
      name        = s"Gmos${location(site)}StaticConfig",
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
          fieldType   = EnumTypeGmosDetector,
          description = Some("Detector in use (always HAMAMATSU for recent and new observations)"),
          resolve     = _.value.common.detector
        ),

        Field(
          name        = "mosPreImaging",
          fieldType   = EnumTypeMosPreImaging,
          description = Some("Is MOS Pre-Imaging Observation"),
          resolve     = _.value.common.mosPreImaging
        ),

        Field(
          name        = "nodAndShuffle",
          fieldType   = OptionType(GmosNodAndShuffleType[F]),
          description = Some("Nod-and-shuffle configuration"),
          resolve     = _.value.common.nodAndShuffle
        )

      )
    )

  type GmosNorthStatic = GmosModel.Static[GmosNorthStageMode]

  def GmosNorthStaticConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], GmosNorthStatic] =
    GmosStaticConfig[F, GmosNorthStageMode](Site.GN)

  type GmosSouthStatic = GmosModel.Static[GmosSouthStageMode]

  def GmosSouthStaticConfigType[F[_]: Effect]: ObjectType[OdbRepo[F], GmosSouthStatic] =
    GmosStaticConfig[F, GmosSouthStageMode](Site.GS)

  def GmosCcdReadoutType[F[_]: Effect]: ObjectType[OdbRepo[F], GmosModel.CcdReadout] =
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

  def GmosCustomMaskType[F[_]: Effect]: ObjectType[OdbRepo[F], GmosModel.CustomMask] =
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

  def GmosGratingType[F[_]: Effect, D: EnumType](
    site: Site
  ): ObjectType[OdbRepo[F], GmosModel.Grating[D]] =
    ObjectType(
      name        = s"Gmos${location(site)}Grating",
      description = s"GMOS ${location(site)} Grating",
      fieldsFn    = () => fields(

        Field(
          name        = "disperser",
          fieldType   = implicitly[EnumType[D]],
          description = Some(s"GMOS ${location(site)} Disperser"),
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

  def GmosBuiltinFpuType[F[_]: Effect, U: EnumType: ClassTag](
    site: Site
  ): ObjectType[OdbRepo[F], U] =
    ObjectType(
      name        = s"Gmos${location(site)}BuiltinFpu",
      description = s"GMOS ${location(site)} builtin-in FPU",
      fieldsFn    = () => fields(

        Field(
          name        = "builtin",
          fieldType   = implicitly[EnumType[U]],
          description = Some(s"GMOS ${location(site)} builtin-fpu"),
          resolve     = _.value
        )
      )
    )

  def GmosFpuType[F[_]: Effect, U: EnumType: ClassTag](
    site: Site
  ): OutputType[Either[GmosModel.CustomMask, U]] =
    UnionType(
      name        = "GmosFpu",
      description = Some("Either custom mask or builtin-FPU"),
      types       = List(GmosCustomMaskType[F], GmosBuiltinFpuType[F, U](site))
    ).mapValue[Either[GmosModel.CustomMask, U]](
      _.fold(
        cm => cm: Any,
        u  =>  u: Any
      )
    )

  def GmosDynamicType[F[_]: Effect, D: EnumType, L: EnumType, U: EnumType: ClassTag](
    site: Site
  ): ObjectType[OdbRepo[F], GmosModel.Dynamic[D, L, U]] =
    ObjectType(
      name        = s"Gmos${location(site)}Dynamic",
      description = s"GMOS ${location(site)} dynamic step configuration",
      fieldsFn    = () => fields(

        Field(
          name        = "readout",
          fieldType   = GmosCcdReadoutType[F],
          description = Some("GMOS CCD Readout"),
          resolve     = _.value.common.readout
        ),

        Field(
          name        = "dtax",
          fieldType   = EnumTypeGmosDtax,
          description = Some("GMOS detector x offset"),
          resolve     = _.value.common.dtax
        ),

        Field(
          name        = "exposure",
          fieldType   = DurationType[F],
          description = Some("GMOS exposure time"),
          resolve     = _.value.common.exposure
        ),

        Field(
          name        = "roi",
          fieldType   = EnumTypeGmosRoi,
          description = Some("GMOS region of interest"),
          resolve     = _.value.common.roi
        ),

        Field(
          name        = "grating",
          fieldType   = OptionType(GmosGratingType[F,D](site)),
          description = Some(s"GMOS ${location(site)} grating"),
          resolve     = _.value.grating
        ),

        Field(
          name        = "filter",
          fieldType   = OptionType(implicitly[EnumType[L]]),
          description = Some(s"GMOS ${location(site)} filter"),
          resolve     = _.value.filter
        ),

        Field(
          name        = "fpu",
          fieldType   = OptionType(GmosFpuType[F, U](site)),
          description = Some(s"GMOS ${location(site)} FPU"),
          resolve     = _.value.fpu
        )
      )
    )

  type GmosNorthDynamic = GmosModel.Dynamic[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]

  def GmosNorthDynamicType[F[_]: Effect]: ObjectType[OdbRepo[F], GmosNorthDynamic] =
    GmosDynamicType[F, GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu](Site.GN)

  type GmosSouthDynamic = GmosModel.Dynamic[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]

  def GmosSouthDynamicType[F[_]: Effect]: ObjectType[OdbRepo[F], GmosSouthDynamic] =
    GmosDynamicType[F, GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu](Site.GN)

}
