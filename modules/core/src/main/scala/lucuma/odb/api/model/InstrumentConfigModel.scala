// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.Instrument
import cats.Eq
import cats.data.State
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.macros.Lenses


sealed trait InstrumentConfigModel extends Product with Serializable {

  def instrument: Instrument

  def gmosNorth: Option[InstrumentConfigModel.GmosNorth] =
    this match {
      case gn: InstrumentConfigModel.GmosNorth => gn.some
      case _                                   => none
    }

  def gmosSouth: Option[InstrumentConfigModel.GmosSouth] =
    this match {
      case gs: InstrumentConfigModel.GmosSouth => gs.some
      case _                                   => none
    }
}

object InstrumentConfigModel {

  @Lenses final case class GmosNorth(
    static:      GmosModel.NorthStatic,
    acquisition: SequenceModel[GmosModel.NorthDynamic],
    science:     SequenceModel[GmosModel.NorthDynamic]
  ) extends InstrumentConfigModel {

    def instrument: Instrument =
      Instrument.GmosN

  }

  object GmosNorth {

    implicit val EqGmosNorth: Eq[GmosNorth] =
      Eq.by { a => (a.static, a.acquisition, a.science) }

  }

  final case class CreateGmosNorth(
    static:      GmosModel.CreateNorthStatic,
    acquisition: SequenceModel.Create[GmosModel.CreateNorthDynamic],
    science:     SequenceModel.Create[GmosModel.CreateNorthDynamic]
  ) {

    def create[T](db: Database[T]): State[T, ValidatedInput[GmosNorth]] =
      for {
        aq <- acquisition.create[T, GmosModel.NorthDynamic](db)
        sc <- science.create[T, GmosModel.NorthDynamic](db)
        gn  = (static.create, aq, sc).mapN { (stʹ, aqʹ, scʹ) =>
          GmosNorth(stʹ, aqʹ, scʹ)
        }
      } yield gn

  }

  object CreateGmosNorth {

    implicit val EqCreateGmosNorth: Eq[CreateGmosNorth] =
      Eq.by { a => (a.static, a.acquisition, a.science) }

    implicit val DecoderCreateGmosNorth: Decoder[CreateGmosNorth] =
      deriveDecoder[CreateGmosNorth]

  }

  @Lenses final case class GmosSouth(
    static:      GmosModel.SouthStatic,
    acquisition: SequenceModel[GmosModel.SouthDynamic],
    science:     SequenceModel[GmosModel.SouthDynamic]
  ) extends InstrumentConfigModel {

    def instrument: Instrument =
      Instrument.GmosS

  }

  object GmosSouth {

    implicit val EqGmosSouth: Eq[GmosSouth] =
      Eq.by { a => (a.static, a.acquisition, a.science) }

  }

  final case class CreateGmosSouth(
    static:      GmosModel.CreateSouthStatic,
    acquisition: SequenceModel.Create[GmosModel.CreateSouthDynamic],
    science:     SequenceModel.Create[GmosModel.CreateSouthDynamic]
  ) {

    def create[T](db: Database[T]): State[T, ValidatedInput[GmosSouth]] =
      for {
        aq <- acquisition.create[T, GmosModel.SouthDynamic](db)
        sc <- science.create[T, GmosModel.SouthDynamic](db)
        gs  = (static.create, aq, sc).mapN { (stʹ, aqʹ, scʹ) =>
          GmosSouth(stʹ, aqʹ, scʹ)
        }
      } yield gs

  }

  object CreateGmosSouth {

    implicit val EqCreateGmosSouth: Eq[CreateGmosSouth] =
      Eq.by { a => (a.static, a.acquisition, a.science) }

    implicit val DecoderCreateGmosSouth: Decoder[CreateGmosSouth] =
      deriveDecoder[CreateGmosSouth]

  }

  implicit val EqInstrumentModel: Eq[InstrumentConfigModel] =
    Eq.instance {
      case (a: GmosNorth, b: GmosNorth) => a === b
      case (a: GmosSouth, b: GmosSouth) => a === b
      case _                            => false
    }

  final case class Create(
    gmosNorth: Option[CreateGmosNorth],
    gmosSouth: Option[CreateGmosSouth]
  ) {

    def create[T](db: Database[T]): State[T, ValidatedInput[InstrumentConfigModel]] =
      for {
        gn <- gmosNorth.traverse(_.create(db))
        gs <- gmosSouth.traverse(_.create(db))
      } yield ValidatedInput.requireOne[InstrumentConfigModel](
        "instrument",
        gn,
        gs
      )

  }

  object Create {

    val Empty: Create = Create(
      gmosNorth = None,
      gmosSouth = None
    )

    def gmosNorth(gn: CreateGmosNorth): Create =
      Empty.copy(gmosNorth = gn.some)

    def gmosNorth(
      s:  GmosModel.CreateNorthStatic,
      ac: SequenceModel.Create[GmosModel.CreateNorthDynamic],
      sc: SequenceModel.Create[GmosModel.CreateNorthDynamic]
    ): Create =
      gmosNorth(CreateGmosNorth(s, ac, sc))

    def gmosSouth(gs: CreateGmosSouth): Create =
      Empty.copy(gmosSouth = gs.some)

    def gmosSouth(
      s:  GmosModel.CreateSouthStatic,
      ac: SequenceModel.Create[GmosModel.CreateSouthDynamic],
      sc: SequenceModel.Create[GmosModel.CreateSouthDynamic]
    ): Create =
      gmosSouth(CreateGmosSouth(s, ac, sc))

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.gmosNorth,
        a.gmosSouth
      )}

  }

}
