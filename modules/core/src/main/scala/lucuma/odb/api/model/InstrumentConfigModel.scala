// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import lucuma.core.`enum`.Instrument
import lucuma.core.model.Atom
import cats.Eq
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

    protected def dereferenceSequences[D, R](
      f: StepConfig[_] => Option[D],
      g: (DereferencedSequence[D], DereferencedSequence[D]) => R
    ): StateT[EitherInput, Database, Option[R]] =
      for {
        a <- acquisition.dereference(f)
        s <- science.dereference(f)
      } yield (a, s).mapN { case (a, s) => g(a, s) }

    lazy val dereference: StateT[EitherInput, Database, Option[InstrumentConfigModel]] =
      this match {
        case r: Reference.GmosNorthReference => r.dereferenceGmosNorth.map(_.widen[InstrumentConfigModel])
        case r: Reference.GmosSouthReference => r.dereferenceGmosSouth.map(_.widen[InstrumentConfigModel])
        case _                               => StateT.pure(None)
      }
  }

  object Reference {

    final case class GmosNorthReference(
      static:      GmosModel.NorthStatic,
      acquisition: SequenceModel[Atom.Id],
      science:     SequenceModel[Atom.Id]
    ) extends Reference(Instrument.GmosNorth) {

      val dereferenceGmosNorth: StateT[EitherInput, Database, Option[InstrumentConfigModel.GmosNorth]] =
        dereferenceSequences(_.gmosNorth, (acq, sci) => GmosNorth(static, acq, sci))

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

      val dereferenceGmosSouth: StateT[EitherInput, Database, Option[InstrumentConfigModel.GmosSouth]] =
        dereferenceSequences(_.gmosSouth, (acq, sci) => GmosSouth(static, acq, sci))

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

    val create: StateT[EitherInput, Database, GmosNorth] =
      for {
        st <- StateT.liftF(static.create.toEither)
        aq <- acquisition.create[GmosModel.NorthDynamic]
        sc <- science.create[GmosModel.NorthDynamic]
      } yield GmosNorth(st, aq, sc)
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

    val create: StateT[EitherInput, Database, GmosSouth] =
      for {
        st <- StateT.liftF(static.create.toEither)
        aq <- acquisition.create[GmosModel.SouthDynamic]
        sc <- science.create[GmosModel.SouthDynamic]
      } yield GmosSouth(st, aq, sc)

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

    val create: StateT[EitherInput, Database, InstrumentConfigModel] =
      for {
        gn <- gmosNorth.traverse(_.create)
        gs <- gmosSouth.traverse(_.create)
        g  <- (gn, gs) match {
          case (Some(n), None) => StateT.pure[EitherInput, Database, InstrumentConfigModel](n)
          case (None, Some(s)) => StateT.pure[EitherInput, Database, InstrumentConfigModel](s)
          case (None,    None) => StateT.liftF[EitherInput, Database, InstrumentConfigModel](InputError.fromMessage("").leftNec[InstrumentConfigModel])
          case _               => StateT.liftF[EitherInput, Database, InstrumentConfigModel](InputError.fromMessage("").leftNec[InstrumentConfigModel])
        }
      } yield g
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
