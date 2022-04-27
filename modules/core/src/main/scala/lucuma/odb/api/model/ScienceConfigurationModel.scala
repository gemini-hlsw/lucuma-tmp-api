// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.{GmosNorthFpu, GmosSouthFpu, Instrument}
import cats.Eq
import cats.syntax.all._
import cats.data.StateT
import clue.data.Input
import io.circe.Decoder
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosNorthGrating
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.enum.GmosSouthGrating
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
      grating:   GmosNorthGrating,
      fpu:       GmosNorthFpu
    ) extends ScienceConfigurationModel {
      val mode: ConfigurationMode =
        ConfigurationMode.GmosNorthLongSlit

      val instrument: Instrument =
        Instrument.GmosNorth
    }

    object GmosNorthLongSlit {
      val filter: Lens[GmosNorthLongSlit, Option[GmosNorthFilter]] =
        Focus[GmosNorthLongSlit](_.filter)

      val grating: Lens[GmosNorthLongSlit, GmosNorthGrating] =
        Focus[GmosNorthLongSlit](_.grating)

      val fpu: Lens[GmosNorthLongSlit, GmosNorthFpu] =
        Focus[GmosNorthLongSlit](_.fpu)

      implicit val EqGmosNorth: Eq[GmosNorthLongSlit] =
        Eq.by(a => (a.filter, a.grating, a.fpu))
    }

    final case class GmosNorthLongSlitInput(
      filter:    Input[GmosNorthFilter]    = Input.ignore,
      grating:   Input[GmosNorthGrating] = Input.ignore,
      fpu:       Input[GmosNorthFpu]       = Input.ignore
    ) extends EditorInput[GmosNorthLongSlit] {

      override val create: ValidatedInput[GmosNorthLongSlit] =
        (grating.notMissing("grating"),
         fpu.notMissing("fpu")
        ).mapN { case (d, u) =>
          GmosNorthLongSlit(filter.toOption, d, u)
        }

      override val edit: StateT[EitherInput, GmosNorthLongSlit, Unit] = {
        val validArgs =
          (grating.validateIsNotNull("grating"),
           fpu.validateIsNotNull("fpu")
          ).tupled

        for {
          args <- validArgs.liftState
          (grating, fpu) = args
          _ <- GmosNorthLongSlit.filter    := filter.toOptionOption
          _ <- GmosNorthLongSlit.grating   := grating
          _ <- GmosNorthLongSlit.fpu       := fpu
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
          a.grating,
          a.fpu
        )}

    }

    final case class GmosSouthLongSlit(
      filter:    Option[GmosSouthFilter],
      grating:   GmosSouthGrating,
      fpu:       GmosSouthFpu
    ) extends ScienceConfigurationModel {
      val mode: ConfigurationMode =
        ConfigurationMode.GmosSouthLongSlit

      val instrument: Instrument =
        Instrument.GmosSouth
    }

    object GmosSouthLongSlit {
      val filter: Lens[GmosSouthLongSlit, Option[GmosSouthFilter]] =
        Focus[GmosSouthLongSlit](_.filter)

      val grating: Lens[GmosSouthLongSlit, GmosSouthGrating] =
        Focus[GmosSouthLongSlit](_.grating)

      val fpu: Lens[GmosSouthLongSlit, GmosSouthFpu] =
        Focus[GmosSouthLongSlit](_.fpu)

      implicit val EqGmosSouth: Eq[GmosSouthLongSlit] =
        Eq.by(a => (a.filter, a.grating, a.fpu))

    }

    final case class GmosSouthLongSlitInput(
      filter:    Input[GmosSouthFilter]    = Input.ignore,
      grating:   Input[GmosSouthGrating] = Input.ignore,
      fpu:       Input[GmosSouthFpu]       = Input.ignore
    ) extends EditorInput[GmosSouthLongSlit] {

      override val create: ValidatedInput[GmosSouthLongSlit] =
        (grating.notMissing("grating"),
         fpu.notMissing("fpu")
        ).mapN { case (d, u) =>
          GmosSouthLongSlit(filter.toOption, d, u)
        }

      def edit: StateT[EitherInput, GmosSouthLongSlit, Unit] = {
        val validArgs =
          (grating.validateIsNotNull("grating"),
           fpu.validateIsNotNull("fpu")
          ).tupled

        for {
          args <- validArgs.liftState
          (grating, fpu) = args
          _ <- GmosSouthLongSlit.filter    := filter.toOptionOption
          _ <- GmosSouthLongSlit.grating   := grating
          _ <- GmosSouthLongSlit.fpu       := fpu
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
          a.grating,
          a.fpu
        )}

    }
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
