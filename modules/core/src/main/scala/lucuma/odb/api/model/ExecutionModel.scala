// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.Instrument
import cats.Eq
import cats.effect.Sync
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.Prism


sealed trait ExecutionModel {
  def instrument:  Instrument
}


object ExecutionModel {

  final case class Config[S, D](
    static:      S,
    acquisition: Sequence[D],
    science:     Sequence[D]
  )

  object Config {
    implicit def EqConfig[S: Eq, D: Eq]: Eq[Config[S, D]] =
      Eq.by { a => (
        a.static,
        a.acquisition,
        a.science
      )}
  }

  final case class GmosNorth(
    config: Config[GmosModel.NorthStatic, GmosModel.NorthDynamic]
  ) extends ExecutionModel {

    def instrument: Instrument =
      Instrument.GmosNorth

  }

  object GmosNorth {

    implicit val EqGmosNorth: Eq[GmosNorth] =
      Eq.by(_.config)

  }

  final case class CreateGmosNorth(
    static:      GmosModel.CreateNorthStatic,
    acquisition: SequenceModel.Create[GmosModel.CreateNorthDynamic],
    science:     SequenceModel.Create[GmosModel.CreateNorthDynamic]
  ) {

    def create[F[_]: Sync]: F[ValidatedInput[GmosNorth]] =
      for {
        st <- Sync[F].pure(static.create)
        aq <- acquisition.create[F, GmosModel.NorthDynamic]
        sc <- science.create[F, GmosModel.NorthDynamic]
      } yield (st, aq, sc).mapN((st, aq, sc) => GmosNorth(Config(st, aq, sc)))
  }

  object CreateGmosNorth {

    implicit val EqCreateGmosNorth: Eq[CreateGmosNorth] =
      Eq.by { a => (a.static, a.acquisition, a.science) }

    implicit val DecoderCreateGmosNorth: Decoder[CreateGmosNorth] =
      deriveDecoder[CreateGmosNorth]

  }

  final case class GmosSouth(
    config: Config[GmosModel.SouthStatic, GmosModel.SouthDynamic]
  ) extends ExecutionModel {

    def instrument: Instrument =
      Instrument.GmosSouth

  }

  object GmosSouth {

    implicit val EqGmosSouth: Eq[GmosSouth] =
      Eq.by(_.config)

  }

  final case class CreateGmosSouth(
    static:      GmosModel.CreateSouthStatic,
    acquisition: SequenceModel.Create[GmosModel.CreateSouthDynamic],
    science:     SequenceModel.Create[GmosModel.CreateSouthDynamic]
  ) {

    def create[F[_]: Sync]: F[ValidatedInput[GmosSouth]] =
      for {
        st <- Sync[F].pure(static.create)
        aq <- acquisition.create[F, GmosModel.SouthDynamic]
        sc <- science.create[F,GmosModel.SouthDynamic]
      } yield (st, aq, sc).mapN((st, aq, sc) => GmosSouth(Config(st, aq, sc)))

  }

  object CreateGmosSouth {

    implicit val EqCreateGmosSouth: Eq[CreateGmosSouth] =
      Eq.by { a => (a.static, a.acquisition, a.science) }

    implicit val DecoderCreateGmosSouth: Decoder[CreateGmosSouth] =
      deriveDecoder[CreateGmosSouth]

  }

  implicit val EqExecutionConfigModel: Eq[ExecutionModel] =
    Eq.instance {
      case (a: GmosNorth, b: GmosNorth) => a === b
      case (a: GmosSouth, b: GmosSouth) => a === b
      case _                            => false
    }

  val AsGmosNorth: Prism[ExecutionModel, GmosNorth] =
    Prism.apply[ExecutionModel, GmosNorth] {
      case gn: GmosNorth => gn.some
      case _             => None
    }(identity)

  val AsGmosSouth: Prism[ExecutionModel, GmosSouth] =
    Prism.apply[ExecutionModel, GmosSouth] {
      case gs: GmosSouth => gs.some
      case _             => None
    }(identity)


  final case class Create(
    gmosNorth: Option[CreateGmosNorth],
    gmosSouth: Option[CreateGmosSouth]
  ) {

    def create[F[_]: Sync]: F[ValidatedInput[ExecutionModel]] =
      for {
        gn <- gmosNorth.traverse(_.create)
        gs <- gmosSouth.traverse(_.create)
      } yield (gn, gs) match {
          case (Some(n), None) => n
          case (None, Some(s)) => s
          case (None,    None) => InputError.fromMessage("At least one of `gmosNorth` or `gmosSouth` required").invalidNec[ExecutionModel]
          case _               => InputError.fromMessage("Only one of `gmosNorth` or `gmosSouth` may be specified").invalidNec[ExecutionModel]
        }
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
