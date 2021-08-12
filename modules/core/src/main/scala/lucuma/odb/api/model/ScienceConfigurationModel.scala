// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.Instrument
import cats.Eq
import cats.syntax.all._
import cats.data.State
import clue.data.Input
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthDisperser
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthDisperser
import lucuma.core.math.Angle
import lucuma.core.optics.syntax.lens._
import lucuma.core.util.Enumerated
import lucuma.core.util.Display
import lucuma.odb.api.model.syntax.input._
import monocle.{Focus, Lens, Prism}
import monocle.macros.GenPrism

sealed trait ScienceConfigurationModel extends Product with Serializable {
  import ScienceConfigurationModel.Modes

  def instrument: Instrument

  def mode: ConfigurationMode

  def fold[A](gnls: Modes.GmosNorthLongSlit => A, gsls: Modes.GmosSouthLongSlit => A): A = this match {
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

    final case class CreateGmosNorthLongSlit(
      filter:    Option[GmosNorthFilter],
      disperser: GmosNorthDisperser,
      slitWidth: SlitWidthInput
    ) {
      def create: ValidatedInput[ScienceConfigurationModel] =
        slitWidth.toAngle.map(GmosNorthLongSlit(filter, disperser, _))
    }

    object CreateGmosNorthLongSlit {
      implicit val DecoderCreateGmosNorthLongSlit: Decoder[CreateGmosNorthLongSlit] =
        deriveDecoder[CreateGmosNorthLongSlit]

      implicit val EqCreateGmosNorthLongSlit: Eq[CreateGmosNorthLongSlit] =
        Eq.by { a => (
          a.filter,
          a.disperser,
          a.slitWidth
        )}
    }

    final case class EditGmosNorthLongSlit(
      filter:    Input[GmosNorthFilter]    = Input.ignore,
      disperser: Input[GmosNorthDisperser] = Input.ignore,
      slitWidth: Input[SlitWidthInput]     = Input.ignore
    ) {

      def edit: ValidatedInput[State[GmosNorthLongSlit, Unit]] =
        (disperser.validateIsNotNull("disperser"),
         slitWidth.validateNotNullable("slitWidth")(_.toAngle)).mapN {
          (disperser, slitWidth) =>
            for {
              _ <- GmosNorthLongSlit.filter    := filter.toOptionOption
              _ <- GmosNorthLongSlit.disperser := disperser
              _ <- GmosNorthLongSlit.slitWidth := slitWidth
            } yield ()
        }
    }

    object EditGmosNorthLongSlit {
      import io.circe.generic.extras.semiauto._
      import io.circe.generic.extras.Configuration
      implicit val customConfig: Configuration = Configuration.default.withDefaults

      implicit val DecoderEditGmosNorthLongSlit: Decoder[EditGmosNorthLongSlit] =
        deriveConfiguredDecoder[EditGmosNorthLongSlit]
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

    final case class CreateGmosSouthLongSlit(
      filter:    Option[GmosSouthFilter],
      disperser: GmosSouthDisperser,
      slitWidth: SlitWidthInput
    ) {
      def create: ValidatedInput[GmosSouthLongSlit] =
        slitWidth.toAngle.map(GmosSouthLongSlit(filter, disperser, _))
    }

    object CreateGmosSouthLongSlit {
      implicit val DecoderCreateGmosSouthLongSlit: Decoder[CreateGmosSouthLongSlit] =
        deriveDecoder[CreateGmosSouthLongSlit]

      implicit val EqCreateGmosSouthLongSlit: Eq[CreateGmosSouthLongSlit] =
        Eq.by { a => (
          a.filter,
          a.disperser,
          a.slitWidth
        )}
    }

    final case class EditGmosSouthLongSlit(
      filter:    Input[GmosSouthFilter]    = Input.ignore,
      disperser: Input[GmosSouthDisperser] = Input.ignore,
      slitWidth: Input[SlitWidthInput]     = Input.ignore
    ) {

      def edit: ValidatedInput[State[GmosSouthLongSlit, Unit]] =
        (disperser.validateIsNotNull("disperser"),
         slitWidth.validateNotNullable("slitWidth")(_.toAngle)).mapN {
          (disperser, slitWidth) =>
            for {
              _ <- GmosSouthLongSlit.filter    := filter.toOptionOption
              _ <- GmosSouthLongSlit.disperser := disperser
              _ <- GmosSouthLongSlit.slitWidth := slitWidth
            } yield ()
        }
    }

    object EditGmosSouthLongSlit {
      import io.circe.generic.extras.semiauto._
      import io.circe.generic.extras.Configuration
      implicit val customConfig: Configuration = Configuration.default.withDefaults

      implicit val DecoderEditGmosSouthLongSlit: Decoder[EditGmosSouthLongSlit] =
        deriveConfiguredDecoder[EditGmosSouthLongSlit]
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

  implicit val EqObservationConfigModel: Eq[ScienceConfigurationModel] =
    Eq.instance {
      case (a: GmosNorthLongSlit, b: GmosNorthLongSlit) => a === b
      case (_, _)                                       => false
    }

  final case class Create(
    gmosNorthLongSlit: Option[CreateGmosNorthLongSlit],
    gmosSouthLongSlit: Option[CreateGmosSouthLongSlit],
  ) {

    def create: ValidatedInput[ScienceConfigurationModel] =
      ValidatedInput.requireOne[ScienceConfigurationModel](
        "mode",
        gmosNorthLongSlit.map(_.create),
        gmosSouthLongSlit.map(_.create)
      )

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.gmosNorthLongSlit,
        a.gmosSouthLongSlit
      )}
  }

  final case class ScienceConfigurationModelEdit(
    set: Option[Create] = None,
    edit: Option[Edit] = None
  ) {
    def setInput: ValidatedInput[Option[ScienceConfigurationModel]] =
      set.traverse(_.create)

    def editInput: ValidatedInput[Option[State[ScienceConfigurationModel, Unit]]] =
      edit.traverse(_.editor)

    val editor: ValidatedInput[Option[Either[ScienceConfigurationModel, State[ScienceConfigurationModel, Unit]]]] =
      ValidatedInput.optionEither(
        "set",
        "edit",
        setInput,
        editInput
      )
  }

  object ScienceConfigurationModelEdit {
    implicit val DecoderScienceConfigurationModelEdit: Decoder[ScienceConfigurationModelEdit] =
      deriveDecoder[ScienceConfigurationModelEdit]
  }

  final case class Edit(
    gmosNorthLongSlit: Option[EditGmosNorthLongSlit],
    gmosSouthLongSlit: Option[EditGmosSouthLongSlit]
  ) {

    val gsLongSlit: Option[ValidatedInput[State[ScienceConfigurationModel, Unit]]] =
      gmosSouthLongSlit.map { ls =>
        ls.edit.map { ed =>
          State.modify[ScienceConfigurationModel] {
            case m: GmosSouthLongSlit => ed.runS(m).value
            case m                    => m
          }
        }
      }

    val gnLongSlit: Option[ValidatedInput[State[ScienceConfigurationModel, Unit]]] =
      gmosNorthLongSlit.map { ls =>
        ls.edit.map { ed =>
          State.modify[ScienceConfigurationModel] {
            case m: GmosNorthLongSlit => ed.runS(m).value
            case m                    => m
          }
        }
      }

    def editor: ValidatedInput[State[ScienceConfigurationModel, Unit]] = {
      ValidatedInput.requireOne(
        "mode",
        gnLongSlit,
        gsLongSlit
      )
    }

  }

  object Edit {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] =
      deriveConfiguredDecoder[Edit]
  }
}

trait ScienceConfigurationModelOptics {
  import ScienceConfigurationModel.Modes

  val gmosNorthLongSlit: Prism[ScienceConfigurationModel, Modes.GmosNorthLongSlit] =
    GenPrism[ScienceConfigurationModel, Modes.GmosNorthLongSlit]
}
