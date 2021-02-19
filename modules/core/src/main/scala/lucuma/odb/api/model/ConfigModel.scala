// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.Instrument
import lucuma.odb.api.model.SequenceModel.Sequence

import cats.Eq
import cats.syntax.all._

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.Lens

/**
 * Instrument specific configuration.  Instances for each instrument.
 */
sealed abstract class ConfigModel(val instrument: Instrument) extends Product with Serializable {

  def gmosNorth: Option[ConfigModel.GmosNorth] =
    this match {
      case gn: ConfigModel.GmosNorth => Some(gn)
      case _                         => None
    }

  def gmosSouth: Option[ConfigModel.GmosSouth] =
    this match {
      case gs: ConfigModel.GmosSouth => Some(gs)
      case _                         => None
    }

}


object ConfigModel {

  implicit val EqConfigModel: Eq[ConfigModel] =
    Eq.instance[ConfigModel] {
      case (a: GmosNorth, b: GmosNorth) => a === b
      case (a: GmosSouth, b: GmosSouth) => a === b
      case (_, _)                       => false
    }


  // Here I was anticipating that each instrument will have other specific
  // configuration besides the manual sequence but I'm not sure.  At the very
  // least it gives a GmosNorth-specific ManualSequence type.  Perhaps there
  // are better ideas?

  type GmosNorthSequence = Sequence[GmosModel.NorthStatic, GmosModel.NorthDynamic]

  final case class GmosNorth(
    manual: GmosNorthSequence
  ) extends ConfigModel(Instrument.GmosN)

  object GmosNorth extends GmosNorthOptics {

    implicit val EqGmosNorth: Eq[GmosNorth] =
      Eq.by(_.manual)

  }

  sealed trait GmosNorthOptics { this: GmosNorth.type =>

    val manual: Lens[GmosNorth, GmosNorthSequence] =
      Lens[GmosNorth, GmosNorthSequence](_.manual)(a => _.copy(manual = a))

  }

  type GmosSouthSequence = Sequence[GmosModel.SouthStatic, GmosModel.SouthDynamic]

  final case class GmosSouth(
    manual: GmosSouthSequence
  ) extends ConfigModel(Instrument.GmosS)

  object GmosSouth extends GmosSouthOptics {

    implicit val EqGmosSouth: Eq[GmosSouth] =
      Eq.by(_.manual)

  }

  sealed trait GmosSouthOptics { this: GmosSouth.type =>

    val manual: Lens[GmosSouth, GmosSouthSequence] =
      Lens[GmosSouth, GmosSouthSequence](_.manual)(a => _.copy(manual = a))

  }


  /**
   * Input parameter used to create a configuration.  All optional but
   * validation will check that exactly one is defined.
   */
  final case class Create(
    gmosNorth: Option[CreateGmosNorth],
    gmosSouth: Option[CreateGmosSouth]
  ) {

    def create: ValidatedInput[ConfigModel] =
      ValidatedInput.requireOne("config",
        gmosNorth.map(_.create),
        gmosSouth.map(_.create)
      )

  }

  object Create {

    val Empty: Create = Create(
      gmosNorth = None,
      gmosSouth = None
    )

    def gmosNorth(gn: CreateGmosNorth): Create =
      Empty.copy(gmosNorth = Some(gn))

    def gmosSouth(gs: CreateGmosSouth): Create =
      Empty.copy(gmosSouth = Some(gs))

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.gmosNorth,
        a.gmosSouth
      )}

    implicit val InputValidatorCreate: InputValidator[Create, ConfigModel] =
      InputValidator.by(_.create)

  }

  final case class CreateGmosNorth(
    manual: Sequence.Create[GmosModel.CreateNorthStatic, GmosModel.CreateNorthDynamic]
  ) {

    def create: ValidatedInput[GmosNorth] =
      manual.create.map(GmosNorth(_))
  }

  object CreateGmosNorth {

    implicit val DecoderCreateGmosNorth: Decoder[CreateGmosNorth] =
      deriveDecoder[CreateGmosNorth]

    implicit val EqCreateGmosNorth: Eq[CreateGmosNorth] =
      Eq.by(_.manual)

    implicit val ValidatorCreateGmosNorth: InputValidator[CreateGmosNorth, GmosNorth] =
      InputValidator.by[CreateGmosNorth, GmosNorth](_.create)

  }


  final case class CreateGmosSouth(
    manual: Sequence.Create[GmosModel.CreateSouthStatic, GmosModel.CreateSouthDynamic]
  ) {

    def create: ValidatedInput[GmosSouth] =
      manual.create.map(GmosSouth(_))
  }

  object CreateGmosSouth {

    implicit val DecoderCreateGmosSouth: Decoder[CreateGmosSouth] =
      deriveDecoder[CreateGmosSouth]

    implicit val EqCreateGmosSouth: Eq[CreateGmosSouth] =
      Eq.by(_.manual)

    implicit val ValidatorCreateGmosSouth: InputValidator[CreateGmosSouth, GmosSouth] =
      InputValidator.by[CreateGmosSouth, GmosSouth](_.create)

  }

}
