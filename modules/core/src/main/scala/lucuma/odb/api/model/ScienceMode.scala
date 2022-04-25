// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.apply._
import clue.data.Input
import coulomb.Quantity
import coulomb.cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.{NonEmptyString, PosDouble}
import io.circe.Decoder
import lucuma.core.`enum`.{GmosAmpGain, GmosAmpReadMode, GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu, GmosRoi, GmosXBinning, GmosYBinning, ImageQuality, Instrument}
import lucuma.core.math.{Angle, Offset}
import lucuma.core.math.units.Nanometer
import lucuma.core.model.SourceProfile
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.{Focus, Lens}

sealed trait ScienceMode extends Product with Serializable {

  def instrument: Instrument

}

object ScienceMode {

  object GmosLongSlit {

    sealed trait BasicConfig[G, F, U] {
      def grating: G
      def filter:  Option[F]
      def fpu:     U
    }

    sealed trait AdvancedConfig[G, F, U] {
      def name: Option[NonEmptyString]

      def overrideBasic: BasicConfig[G, F, U]

      def grating: G =
        overrideBasic.grating

      def filter: Option[F] =
        overrideBasic.filter

      def fpu: U =
        overrideBasic.fpu

      def explicitXBin: Option[GmosXBinning]

      def explicitYBin: Option[GmosYBinning]

      def yBin: GmosYBinning =
        explicitYBin.getOrElse(AdvancedConfig.DefaultYBinning)

      def explicitAmpReadMode: Option[GmosAmpReadMode]

      def ampReadMode: GmosAmpReadMode =
        explicitAmpReadMode.getOrElse(AdvancedConfig.DefaultAmpReadMode)

      def explicitAmpGain: Option[GmosAmpGain]

      def ampGain: GmosAmpGain =
        explicitAmpGain.getOrElse(AdvancedConfig.DefaultAmpGain)

      def explicitRoi: Option[GmosRoi]

      def roi: GmosRoi =
        explicitRoi.getOrElse(AdvancedConfig.DefaultRoi)

      def explicitλDithers: Option[List[Quantity[Int, Nanometer]]]

      def explicitSpatialOffsets: Option[List[Offset.Q]]

      def spatialOffsets: List[Offset.Q] =
        explicitSpatialOffsets.getOrElse(AdvancedConfig.DefaultSpatialOffsets)

    }

    object AdvancedConfig {
      val DefaultYBinning: GmosYBinning =
        GmosYBinning.Two

      val DefaultAmpReadMode: GmosAmpReadMode =
        GmosAmpReadMode.Slow

      val DefaultAmpGain: GmosAmpGain =
        GmosAmpGain.Low

      val DefaultRoi: GmosRoi =
        GmosRoi.FullFrame

      val DefaultSpatialOffsets: List[Offset.Q] =
        List(Offset.Q.Zero, Offset.Q(Angle.arcseconds.reverseGet(15)))
    }

  }

  /**
   * GmosNorthLongSlit mode. BasicConfig options can be overridden or expanded
   * upon in AdvancedConfig if desired.  The AdvancedConfig serves as the input
   * to sequence generation.
   */
  final case class GmosNorthLongSlit(
    basic:    GmosNorthLongSlit.BasicConfig,
    advanced: Option[GmosNorthLongSlit.AdvancedConfig]
  ) extends ScienceMode {

    override def instrument: Instrument =
      Instrument.GmosNorth

  }

  object GmosNorthLongSlit {

    /**
     * BasicConfig options that match the science requirements are listed in a
     * table in Explore.  The user selects one, but may subsequently choose to
     * override (or refine details) in an AdvancedConfig without losing any
     * information in the original BasicConfig.
     */
    final case class BasicConfig(
      grating: GmosNorthDisperser,
      filter:  Option[GmosNorthFilter],
      fpu:     GmosNorthFpu
    ) extends GmosLongSlit.BasicConfig[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu] {

      def toAdvanced: AdvancedConfig =
        AdvancedConfig(
          name          = None,
          overrideBasic = this
        )

    }

    object BasicConfig extends BasicConfigOptics {

      implicit val EqBasicConfig: Eq[BasicConfig] =
        Eq.by { a => (
          a.grating,
          a.filter,
          a.fpu
        )}

    }

    sealed trait BasicConfigOptics { self: BasicConfig.type =>

      val grating: Lens[BasicConfig, GmosNorthDisperser] =
        Focus[BasicConfig](_.grating)

      val filter: Lens[BasicConfig, Option[GmosNorthFilter]] =
        Focus[BasicConfig](_.filter)

      val fpu: Lens[BasicConfig, GmosNorthFpu] =
        Focus[BasicConfig](_.fpu)

    }

    final case class BasicConfigInput(
      grating:   Input[GmosNorthDisperser] = Input.ignore,
      filter:    Input[GmosNorthFilter]    = Input.ignore,
      fpu:       Input[GmosNorthFpu]       = Input.ignore
    ) extends EditorInput[BasicConfig] {

      override val create: ValidatedInput[BasicConfig] =
        (grating.notMissing("grating"),
          fpu.notMissing("fpu")
        ).mapN { case (d, u) =>
          BasicConfig(d, filter.toOption, u)
        }

      override val edit: StateT[EitherInput, BasicConfig, Unit] = {

        val validArgs =
          (grating.validateIsNotNull("grating"),
            fpu.validateIsNotNull("fpu")
          ).tupled

        for {
          args <- validArgs.liftState
          (grating, fpu) = args
          _    <- BasicConfig.grating := grating
          _    <- BasicConfig.filter  := filter.toOptionOption
          _    <- BasicConfig.fpu     := fpu
        } yield ()

      }
    }

    object BasicConfigInput {

      import io.circe.generic.extras.semiauto._
      import io.circe.generic.extras.Configuration
      implicit val customConfig: Configuration = Configuration.default.withDefaults

      implicit val DecoderBasicConfigInput: Decoder[BasicConfig] =
        deriveConfiguredDecoder[BasicConfig]

      implicit val EqBasicConfigInput: Eq[BasicConfigInput] =
        Eq.by { a => (
          a.grating,
          a.filter,
          a.fpu
        )}

    }

    /**
     * AdvancedConfig options provide more control over the sequence that is
     * generated without resorting to a manual sequence.
     */
    final case class AdvancedConfig(
      name:                   Option[NonEmptyString],
      overrideBasic:          BasicConfig,
      explicitXBin:           Option[GmosXBinning]                   = None,  // calculated from effective slit and sampling by default
      explicitYBin:           Option[GmosYBinning]                   = None,
      explicitAmpReadMode:    Option[GmosAmpReadMode]                = None,
      explicitAmpGain:        Option[GmosAmpGain]                    = None,
      explicitRoi:            Option[GmosRoi]                        = None,
      explicitλDithers:       Option[List[Quantity[Int, Nanometer]]] = None,
      explicitSpatialOffsets: Option[List[Offset.Q]]                 = None
    ) extends GmosLongSlit.AdvancedConfig[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu] {

      import lucuma.gen.gmos.longslit.syntax.gmosNorthGrating._
      import lucuma.gen.gmos.longslit.syntax.gmosNorthFpu._

      def xBin(sourceProfile: SourceProfile, imageQuality: ImageQuality, sampling: PosDouble): GmosXBinning =
        explicitXBin.getOrElse(fpu.xbin(sourceProfile, imageQuality, sampling))

      def λDithers: List[Quantity[Int, Nanometer]] =
        explicitλDithers.getOrElse {
          List(
            Quantity[Int, Nanometer](0),
            overrideBasic.grating.Δλ
          )
        }

    }

    object AdvancedConfig extends AdvancedConfigOptics {

      implicit val EqAdvancedConfig: Eq[AdvancedConfig] =
        Eq.by { a => (
          a.name,
          a.overrideBasic,
          a.explicitXBin,
          a.explicitYBin,
          a.explicitAmpReadMode,
          a.explicitAmpGain,
          a.explicitRoi,
          a.explicitλDithers,
          a.explicitSpatialOffsets
        )}

    }

    sealed trait AdvancedConfigOptics { self: AdvancedConfig.type =>

      val name: Lens[AdvancedConfig, Option[NonEmptyString]] =
        Focus[AdvancedConfig](_.name)

      val overrideBasic: Lens[AdvancedConfig, BasicConfig] =
        Focus[AdvancedConfig](_.overrideBasic)

      val grating: Lens[AdvancedConfig, GmosNorthDisperser] =
        overrideBasic.andThen(BasicConfig.grating)

      val filter: Lens[AdvancedConfig, Option[GmosNorthFilter]] =
        overrideBasic.andThen(BasicConfig.filter)

      val fpu: Lens[AdvancedConfig, GmosNorthFpu] =
        overrideBasic.andThen(BasicConfig.fpu)

      val explicitXBin: Lens[AdvancedConfig, Option[GmosXBinning]] =
        Focus[AdvancedConfig](_.explicitXBin)

      val explicitYBin: Lens[AdvancedConfig, Option[GmosYBinning]] =
        Focus[AdvancedConfig](_.explicitYBin)

      val explicitAmpReadMode: Lens[AdvancedConfig, Option[GmosAmpReadMode]] =
        Focus[AdvancedConfig](_.explicitAmpReadMode)

      val explicitAmpGain: Lens[AdvancedConfig, Option[GmosAmpGain]] =
        Focus[AdvancedConfig](_.explicitAmpGain)

      val explicitRoi: Lens[AdvancedConfig, Option[GmosRoi]] =
        Focus[AdvancedConfig](_.explicitRoi)

      val explicitλDithers: Lens[AdvancedConfig, Option[List[Quantity[Int, Nanometer]]]] =
        Focus[AdvancedConfig](_.explicitλDithers)

      val explicitSpatialOffsets: Lens[AdvancedConfig, Option[List[Offset.Q]]] =
        Focus[AdvancedConfig](_.explicitSpatialOffsets)

    }

  }

}
