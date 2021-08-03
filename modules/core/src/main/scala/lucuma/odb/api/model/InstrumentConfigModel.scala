// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.Instrument
import lucuma.core.model.Atom

import cats.{Eq, Monad}
import cats.mtl.Stateful
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

sealed trait InstrumentConfigModel extends Product with Serializable {

  def instrument:  Instrument

  def toReference: InstrumentConfigModel.Reference

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

  sealed abstract class Reference(val instrument: Instrument) extends Product with Serializable {

    def acquisition: SequenceModel[Atom.Id]

    def science:     SequenceModel[Atom.Id]

    protected def dereferenceSequences[F[_]: Monad, T, D, R](
      db: DatabaseReader[T],
      f: StepConfig[_] => Option[D],
      g: (DereferencedSequence[D], DereferencedSequence[D]) => R
    )(
      implicit S: Stateful[F, T]
    ): F[Option[R]] =
      for {
        a <- acquisition.dereference(db)(f)
        s <- science.dereference(db)(f)
      } yield (a, s).mapN { case (a, s) => g(a, s) }

    def dereference[F[_]: Monad, T](db: DatabaseReader[T])(implicit S: Stateful[F, T]): F[Option[InstrumentConfigModel]] =
      this match {
        case r: Reference.GmosNorthReference => r.dereferenceGmosNorth(db).map(_.widen[InstrumentConfigModel])
        case r: Reference.GmosSouthReference => r.dereferenceGmosSouth(db).map(_.widen[InstrumentConfigModel])
        case _                               => Monad[F].pure[Option[InstrumentConfigModel]](None)
      }
  }

  object Reference {

    final case class GmosNorthReference(
      static:      GmosModel.NorthStatic,
      acquisition: SequenceModel[Atom.Id],
      science:     SequenceModel[Atom.Id]
    ) extends Reference(Instrument.GmosNorth) {

      def dereferenceGmosNorth[F[_]: Monad, T](db: DatabaseReader[T])(implicit S: Stateful[F, T]): F[Option[InstrumentConfigModel.GmosNorth]] =
        dereferenceSequences(db, _.gmosNorth, (acq, sci) => GmosNorth(static, acq, sci))

    }

    object GmosNorthReference {
      implicit val EqGmosNorthReference: Eq[GmosNorthReference] =
        Eq.by { a => (a.static, a.acquisition, a.science) }
    }

    final case class GmosSouthReference(
      static:      GmosModel.SouthStatic,
      acquisition: SequenceModel[Atom.Id],
      science:     SequenceModel[Atom.Id]
    ) extends Reference(Instrument.GmosSouth) {

      def dereferenceGmosSouth[F[_]: Monad, T](db: DatabaseReader[T])(implicit S: Stateful[F, T]): F[Option[InstrumentConfigModel.GmosSouth]] =
        dereferenceSequences(db, _.gmosSouth, (acq, sci) => GmosSouth(static, acq, sci))

    }

    object GmosSouthReference {
      implicit val EqGmosSouthReference: Eq[GmosSouthReference] =
        Eq.by { a => (a.static, a.acquisition, a.science) }
    }


    implicit val EqReference: Eq[Reference] =
      Eq.instance {
        case (a: GmosNorthReference, b: GmosNorthReference) => a === b
        case (a: GmosSouthReference, b: GmosSouthReference) => a === b
        case (_, _)                                         => false
      }

  }

  final case class GmosNorth(
    static:      GmosModel.NorthStatic,
    acquisition: SequenceModel[AtomModel[StepModel[GmosModel.NorthDynamic]]],
    science:     SequenceModel[AtomModel[StepModel[GmosModel.NorthDynamic]]]
  ) extends InstrumentConfigModel {

    def instrument: Instrument =
      Instrument.GmosNorth

    override def toReference: Reference.GmosNorthReference =
      Reference.GmosNorthReference(
        static,
        acquisition.map(_.id),
        science.map(_.id)
      )

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

    def create[F[_]: Monad, T](db: DatabaseState[T])(implicit S: Stateful[F, T]): F[ValidatedInput[GmosNorth]] =
      for {
        aq <- acquisition.create[F, T, GmosModel.NorthDynamic](db)
        sc <- science.create[F, T, GmosModel.NorthDynamic](db)
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

  final case class GmosSouth(
    static:      GmosModel.SouthStatic,
    acquisition: SequenceModel[AtomModel[StepModel[GmosModel.SouthDynamic]]],
    science:     SequenceModel[AtomModel[StepModel[GmosModel.SouthDynamic]]]
  ) extends InstrumentConfigModel {

    def instrument: Instrument =
      Instrument.GmosSouth

    override def toReference: Reference.GmosSouthReference =
      Reference.GmosSouthReference(
        static,
        acquisition.map(_.id),
        science.map(_.id)
      )

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

    def create[F[_]: Monad, T](db: DatabaseState[T])(implicit S: Stateful[F, T]): F[ValidatedInput[GmosSouth]] =
      for {
        aq <- acquisition.create[F, T, GmosModel.SouthDynamic](db)
        sc <- science.create[F, T, GmosModel.SouthDynamic](db)
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

    def create[F[_]: Monad, T](db: DatabaseState[T])(implicit S: Stateful[F, T]): F[ValidatedInput[InstrumentConfigModel]] =
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
