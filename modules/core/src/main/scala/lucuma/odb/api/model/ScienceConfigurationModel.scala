// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.Instrument
import cats.Eq
import cats.syntax.all._
import cats.data.StateT
import clue.data.Input
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import lucuma.core.util.Display
import lucuma.odb.api.model.ScienceConfigurationModel.Modes.{GmosNorthLongSlit, GmosNorthLongSlitInput, GmosSouthLongSlit, GmosSouthLongSlitInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.{Focus, Lens, Prism}
import monocle.macros.GenPrism

sealed trait ScienceConfigurationModel extends Product with Serializable {
  import ScienceConfigurationModel.Modes

  def instrument: Instrument

  def mode: ConfigurationMode

  def fold[A](
    gnls: Modes.GmosNorthLongSlit => A,
    gsls: Modes.GmosSouthLongSlit => A
  ): A =
    this match {
      case g: Modes.GmosNorthLongSlit => gnls(g)
      case g: Modes.GmosSouthLongSlit => gsls(g)
    }
}

object ScienceConfigurationModel extends ScienceConfigurationModelOptics {

  object Modes {

    final case class GmosNorthLongSlit(
      filter:    Option[GmosNorthFilter],
      disperser: GmosNorthDisperser,
      slitWidth: Angle
    ) extends ScienceConfigurationModel {
      val mode: ConfigurationMode =
        ConfigurationMode.GmosNorthLongSlit

      val instrument: Instrument =
        Instrument.GmosNorth
    }

    object GmosNorthLongSlit {
      val filter: Lens[GmosNorthLongSlit, Option[GmosNorthFilter]] =
        Focus[GmosNorthLongSlit](_.filter)

      val disperser: Lens[GmosNorthLongSlit, GmosNorthDisperser] =
        Focus[GmosNorthLongSlit](_.disperser)

      val slitWidth: Lens[GmosNorthLongSlit, Angle] =
        Focus[GmosNorthLongSlit](_.slitWidth)

      implicit val EqGmosNorth: Eq[GmosNorthLongSlit] =
        Eq.by(a => (a.filter, a.disperser, a.slitWidth))
    }

    final case class GmosNorthLongSlitInput(
      filter:    Input[GmosNorthFilter]    = Input.ignore,
      disperser: Input[GmosNorthDisperser] = Input.ignore,
      slitWidth: Input[SlitWidthInput]     = Input.ignore
    ) extends EditorInput[GmosNorthLongSlit] {

      override val create: ValidatedInput[GmosNorthLongSlit] =
        (disperser.notMissing("filter"),
         slitWidth.notMissingAndThen("slitWidth")(_.toAngle)
        ).mapN { case (d, s) =>
          GmosNorthLongSlit(filter.toOption, d, s)
        }

      override val edit: StateT[EitherInput, GmosNorthLongSlit, Unit] = {
        val validArgs =
          (disperser.validateIsNotNull("disperser"),
           slitWidth.validateNotNullable("slitWidth")(_.toAngle)
          ).tupled

        for {
          args <- validArgs.liftState
          (disperser, slitWidth) = args
          _ <- GmosNorthLongSlit.filter    := filter.toOptionOption
          _ <- GmosNorthLongSlit.disperser := disperser
          _ <- GmosNorthLongSlit.slitWidth := slitWidth
        } yield ()
      }
    }

    object GmosNorthLongSlitInput {

      import io.circe.generic.extras.semiauto._
      import io.circe.generic.extras.Configuration
      implicit val customConfig: Configuration = Configuration.default.withDefaults

      implicit val DecoderGmosNorthLongSlitInput: Decoder[GmosNorthLongSlitInput] =
        deriveConfiguredDecoder[GmosNorthLongSlitInput]

      implicit val EqEditGmosNorthLongSlit: Eq[GmosNorthLongSlitInput] =
        Eq.by { a => (
          a.filter,
          a.disperser,
          a.slitWidth
        )}

    }

    final case class GmosSouthLongSlit(
      filter:    Option[GmosSouthFilter],
      disperser: GmosSouthDisperser,
      slitWidth: Angle
    ) extends ScienceConfigurationModel {
      val mode: ConfigurationMode =
        ConfigurationMode.GmosSouthLongSlit

      val instrument: Instrument =
        Instrument.GmosSouth
    }

    object GmosSouthLongSlit {
      val filter: Lens[GmosSouthLongSlit, Option[GmosSouthFilter]] =
        Focus[GmosSouthLongSlit](_.filter)

      val disperser: Lens[GmosSouthLongSlit, GmosSouthDisperser] =
        Focus[GmosSouthLongSlit](_.disperser)

      val slitWidth: Lens[GmosSouthLongSlit, Angle] =
        Focus[GmosSouthLongSlit](_.slitWidth)

      implicit val EqGmosSouth: Eq[GmosSouthLongSlit] =
        Eq.by(a => (a.filter, a.disperser, a.slitWidth))

    }

    final case class GmosSouthLongSlitInput(
      filter:    Input[GmosSouthFilter]    = Input.ignore,
      disperser: Input[GmosSouthDisperser] = Input.ignore,
      slitWidth: Input[SlitWidthInput]     = Input.ignore
    ) extends EditorInput[GmosSouthLongSlit] {

      override val create: ValidatedInput[GmosSouthLongSlit] =
        (disperser.notMissing("filter"),
         slitWidth.notMissingAndThen("slitWidth")(_.toAngle)
        ).mapN { case (d, s) =>
          GmosSouthLongSlit(filter.toOption, d, s)
        }

      def edit: StateT[EitherInput, GmosSouthLongSlit, Unit] = {
        val validArgs =
          (disperser.validateIsNotNull("disperser"),
           slitWidth.validateNotNullable("slitWidth")(_.toAngle)
          ).tupled

        for {
          args <- validArgs.liftState
          (disperser, slitWidth) = args
          _ <- GmosSouthLongSlit.filter    := filter.toOptionOption
          _ <- GmosSouthLongSlit.disperser := disperser
          _ <- GmosSouthLongSlit.slitWidth := slitWidth
        } yield ()
      }

    }

    object GmosSouthLongSlitInput {
      import io.circe.generic.extras.semiauto._
      import io.circe.generic.extras.Configuration
      implicit val customConfig: Configuration = Configuration.default.withDefaults

      implicit val DecoderGmosSouthLongSlitInput: Decoder[GmosSouthLongSlitInput] =
        deriveConfiguredDecoder[GmosSouthLongSlitInput]

      implicit val EqEditGmosSouthLongSlit: Eq[GmosSouthLongSlitInput] =
        Eq.by { a => (
          a.filter,
          a.disperser,
          a.slitWidth
        )}

    }
  }

  final case class SlitWidthInput(
    microarcseconds: Option[Long],
    milliarcseconds: Option[BigDecimal],
    arcseconds:      Option[BigDecimal]
  ) {
    import Units._

    val toAngle: ValidatedInput[Angle] =
      ValidatedInput.requireOne("slitWidthInput",
        microarcseconds.map(Microarcseconds.readLong),
        milliarcseconds.map(Milliarcseconds.readDecimal),
        arcseconds     .map(Arcseconds.readDecimal)
      )
  }

  object SlitWidthInput {
    implicit def DecoderFocalPlaneAngleInput: Decoder[SlitWidthInput] =
      deriveDecoder[SlitWidthInput]

    implicit val EqFocalPlaneAngleInput: Eq[SlitWidthInput] =
      Eq.by { a => (
        a.microarcseconds,
        a.milliarcseconds,
        a.arcseconds
      )}

  }

  sealed abstract class Units(
    val angleUnit: AngleModel.Units
  ) extends Product with Serializable {

    def readLong(value: Long): ValidatedInput[Angle] =
      angleUnit.readSignedLong(value)

    def readDecimal(value: BigDecimal): ValidatedInput[Angle] =
      angleUnit.readSignedDecimal(value)
  }

  object Units {

    case object Microarcseconds extends Units(AngleModel.Units.Microarcseconds)
    case object Milliarcseconds extends Units(AngleModel.Units.Milliarcseconds)
    case object Arcseconds      extends Units(AngleModel.Units.Arcseconds)

    val microarcseconds: Units = Microarcseconds
    val milliarcseconds: Units = Milliarcseconds
    val arcseconds: Units      = Arcseconds

    implicit val EnumeratedUnits: Enumerated[Units] =
      Enumerated.of(
        Microarcseconds,
        Milliarcseconds,
        Arcseconds
      )

    implicit def DisplayUnits: Display[Units] =
      Display.byShortName(_.angleUnit.abbreviation)

  }

  import Modes._

  implicit val EqScienceConfigurationModel: Eq[ScienceConfigurationModel] =
    Eq.instance {
      case (a: GmosNorthLongSlit, b: GmosNorthLongSlit) => a === b
      case (_, _)                                       => false
    }

}

trait ScienceConfigurationModelOptics {
  import ScienceConfigurationModel.Modes

  val gmosNorthLongSlit: Prism[ScienceConfigurationModel, Modes.GmosNorthLongSlit] =
    GenPrism[ScienceConfigurationModel, Modes.GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[ScienceConfigurationModel, Modes.GmosSouthLongSlit] =
    GenPrism[ScienceConfigurationModel, Modes.GmosSouthLongSlit]
}

final case class ScienceConfigurationInput(
  gmosNorthLongSlit: Input[GmosNorthLongSlitInput] = Input.ignore,
  gmosSouthLongSlit: Input[GmosSouthLongSlitInput] = Input.ignore
) extends EditorInput[ScienceConfigurationModel] {

  override val create: ValidatedInput[ScienceConfigurationModel] =
    ValidatedInput.requireOne[ScienceConfigurationModel](
      "mode",
      gmosNorthLongSlit.toOption.map(_.create.widen[ScienceConfigurationModel]),
      gmosSouthLongSlit.toOption.map(_.create.widen[ScienceConfigurationModel])
    )

  override val edit: StateT[EitherInput, ScienceConfigurationModel, Unit] =
    EditorInput.editOneOf[ScienceConfigurationModel, GmosNorthLongSlit, GmosSouthLongSlit](
      ("gmosNorthLongSlit", gmosNorthLongSlit, ScienceConfigurationModel.gmosNorthLongSlit),
      ("gmosSouthLongSlit", gmosSouthLongSlit, ScienceConfigurationModel.gmosSouthLongSlit)
    )

}

object ScienceConfigurationInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderScienceConfigurationInput: Decoder[ScienceConfigurationInput] =
      deriveConfiguredDecoder[ScienceConfigurationInput]

    implicit val EqScienceConfigurationInput: Eq[ScienceConfigurationInput] =
      Eq.by { a => (
        a.gmosNorthLongSlit,
        a.gmosSouthLongSlit
      )}

}
