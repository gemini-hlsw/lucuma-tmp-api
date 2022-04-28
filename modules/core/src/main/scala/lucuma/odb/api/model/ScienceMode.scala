// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.traverse._
import clue.data.Input
import io.circe.Decoder
import lucuma.core.`enum`.{GmosNorthFilter, GmosNorthFpu, GmosNorthGrating, GmosSouthFilter, GmosSouthFpu, GmosSouthGrating, Instrument}
import lucuma.odb.api.model.ScienceMode.{GmosNorthLongSlit, GmosNorthLongSlitInput, GmosSouthLongSlit, GmosSouthLongSlitInput}
import lucuma.odb.api.model.gmos.longslit.{AdvancedConfig, AdvancedConfigInput, BasicConfig, BasicConfigInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import monocle.macros.GenPrism
import monocle.{Focus, Lens, Prism}

sealed trait ScienceMode extends Product with Serializable {

  def mode:       ConfigurationMode
  def instrument: Instrument

  def fold[A](
    gnls: ScienceMode.GmosNorthLongSlit => A,
    gsls: ScienceMode.GmosSouthLongSlit => A
  ): A =
    this match {
      case m @ ScienceMode.GmosNorthLongSlit(_, _) => gnls(m)
      case m @ ScienceMode.GmosSouthLongSlit(_, _) => gsls(m)
    }

}

object ScienceMode {

  /**
   * GmosNorthLongSlit mode. BasicConfig options can be overridden or expanded
   * upon in AdvancedConfig if desired.  The AdvancedConfig serves as the input
   * to sequence generation.
   */
  final case class GmosNorthLongSlit(
    basic:    BasicConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu],
    advanced: Option[AdvancedConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]]
  ) extends ScienceMode with gmos.longslit.LongSlit[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu] {

    override def mode: ConfigurationMode =
      ConfigurationMode.GmosNorthLongSlit

    override def instrument: Instrument =
      Instrument.GmosNorth

  }

  object GmosNorthLongSlit extends GmosNorthLongSlitOptics {

    implicit val EqGmosNorthLongSlit: Eq[GmosNorthLongSlit] =
      Eq.by { a => (
        a.basic,
        a.advanced
      )}

  }

  sealed trait GmosNorthLongSlitOptics { self: GmosNorthLongSlit.type =>

    val basic: Lens[GmosNorthLongSlit, BasicConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]] =
      Focus[GmosNorthLongSlit](_.basic)

    val advanced: Lens[GmosNorthLongSlit, Option[AdvancedConfig[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]]] =
      Focus[GmosNorthLongSlit](_.advanced)

  }

  final case class GmosNorthLongSlitInput(
    basic:    Input[BasicConfigInput[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]]    = Input.ignore,
    advanced: Input[AdvancedConfigInput[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]] = Input.ignore
  ) extends EditorInput[GmosNorthLongSlit] {

    override val create: ValidatedInput[GmosNorthLongSlit] =
      (basic.notMissingAndThen("basicConfig")(_.create),
        advanced.toOption.traverse(_.create)
      ).mapN { case (b, a) => GmosNorthLongSlit(b, a) }

    override def edit: StateT[EitherInput, GmosNorthLongSlit, Unit] =
      for {
        _ <- GmosNorthLongSlit.basic    :! basic
        _ <- GmosNorthLongSlit.advanced :? advanced
      } yield ()

  }

  object GmosNorthLongSlitInput {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderGmosNorthLongSlitInput: Decoder[GmosNorthLongSlitInput] =
      deriveConfiguredDecoder[GmosNorthLongSlitInput]

    implicit def EqGmosNorthLongSlitInput: Eq[GmosNorthLongSlitInput] =
      Eq.by { a => (
        a.basic,
        a.advanced
      )}
  }

  final case class GmosSouthLongSlit(
    basic:    BasicConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu],
    advanced: Option[AdvancedConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]]
  ) extends ScienceMode with gmos.longslit.LongSlit[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu] {

    override def mode: ConfigurationMode =
      ConfigurationMode.GmosSouthLongSlit

    override def instrument: Instrument =
      Instrument.GmosSouth

  }

  object GmosSouthLongSlit extends GmosSouthLongSlitOptics {

    implicit val EqGmosSouthLongSlit: Eq[GmosSouthLongSlit] =
      Eq.by { a => (
        a.basic,
        a.advanced
      )}

  }

  sealed trait GmosSouthLongSlitOptics { self: GmosSouthLongSlit.type =>

    val basic: Lens[GmosSouthLongSlit, BasicConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]] =
      Focus[GmosSouthLongSlit](_.basic)

    val advanced: Lens[GmosSouthLongSlit, Option[AdvancedConfig[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]]] =
      Focus[GmosSouthLongSlit](_.advanced)

  }

  final case class GmosSouthLongSlitInput(
    basic:    Input[BasicConfigInput[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]]    = Input.ignore,
    advanced: Input[AdvancedConfigInput[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]] = Input.ignore
  ) extends EditorInput[GmosSouthLongSlit] {

    override val create: ValidatedInput[GmosSouthLongSlit] =
      (basic.notMissingAndThen("basicConfig")(_.create),
        advanced.toOption.traverse(_.create)
      ).mapN { case (b, a) => GmosSouthLongSlit(b, a) }

    override def edit: StateT[EitherInput, GmosSouthLongSlit, Unit] =
      for {
        _ <- GmosSouthLongSlit.basic    :! basic
        _ <- GmosSouthLongSlit.advanced :? advanced
      } yield ()

  }

  object GmosSouthLongSlitInput {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderGmosSouthLongSlitInput: Decoder[GmosSouthLongSlitInput] =
      deriveConfiguredDecoder[GmosSouthLongSlitInput]

    implicit def EqGmosSouthLongSlitInput: Eq[GmosSouthLongSlitInput] =
      Eq.by { a => (
        a.basic,
        a.advanced
      )}
  }

  implicit val EqScienceMode: Eq[ScienceMode] =
    Eq.instance {
      case (a @ GmosNorthLongSlit(_, _), b @ GmosNorthLongSlit(_, _)) => a === b
      case (a @ GmosSouthLongSlit(_, _), b @ GmosSouthLongSlit(_, _)) => a === b
      case _                                                          => false
    }

  val gmosNorthLongSlit: Prism[ScienceMode, GmosNorthLongSlit] =
    GenPrism[ScienceMode, GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[ScienceMode, GmosSouthLongSlit] =
    GenPrism[ScienceMode, GmosSouthLongSlit]

}

final case class ScienceModeInput(
  gmosNorthLongSlit: Input[GmosNorthLongSlitInput] = Input.ignore,
  gmosSouthLongSlit: Input[GmosSouthLongSlitInput] = Input.ignore
) extends EditorInput[ScienceMode] {

  override val create: ValidatedInput[ScienceMode] =
    ValidatedInput.requireOne[ScienceMode](
      "mode",
      gmosNorthLongSlit.toOption.map(_.create.widen[ScienceMode]),
      gmosSouthLongSlit.toOption.map(_.create.widen[ScienceMode])
    )

  override val edit: StateT[EitherInput, ScienceMode, Unit] =
    EditorInput.editOneOf[ScienceMode, GmosNorthLongSlit, GmosSouthLongSlit](
      ("gmosNorthLongSlit", gmosNorthLongSlit, ScienceMode.gmosNorthLongSlit),
      ("gmosSouthLongSlit", gmosSouthLongSlit, ScienceMode.gmosSouthLongSlit)
    )

}

object ScienceModeInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderScienceModeInput: Decoder[ScienceModeInput] =
    deriveConfiguredDecoder[ScienceModeInput]

  implicit val EqScienceModeInput: Eq[ScienceModeInput] =
    Eq.by { a => (
      a.gmosNorthLongSlit,
      a.gmosSouthLongSlit
    )}

}
