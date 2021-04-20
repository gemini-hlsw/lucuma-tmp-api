// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.syntax.inputvalidator._
import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.`enum`.Instrument
import monocle.Lens

object SequenceModel {

  /**
   * Sequence representation.
   *
   * @param atoms atoms that make up the sequence
   *
   * @tparam D dynamic (step) configuration type
   */
  final case class Sequence[D](
    atoms: List[AtomModel[D]]
  )

  object Sequence extends SequenceOptics {

    implicit def EqSequence[D: Eq]: Eq[Sequence[D]] =
      Eq.by { _.atoms }

    final case class Create[CD](
      atoms: List[AtomModel.Create[CD]]
    ) {

      def create[D](implicit ev: InputValidator[CD, D]): ValidatedInput[Sequence[D]] =
        atoms.traverse(_.create).map { as => Sequence(as) }

    }

    object Create {

      implicit def EdCreate[D: Eq]: Eq[Create[D]] =
        Eq.by { _.atoms }

      implicit def DecoderCreate[D: Decoder]:Decoder[Create[D]] =
        deriveDecoder[Create[D]]

      implicit def ValidatorCreate[CD, D](
        implicit ev: InputValidator[CD, D]
      ): InputValidator[Create[CD], Sequence[D]] =
        InputValidator.by[Create[CD], Sequence[D]](_.create)

    }

  }

  sealed trait SequenceOptics { this: Sequence.type =>

    def steps[D]: Lens[Sequence[D], List[AtomModel[D]]] =
      Lens[Sequence[D], List[AtomModel[D]]](_.atoms)(a => _.copy(atoms = a))

  }

  // ---------------------------------------------------------------

  final case class Config[S, D](
    static:      S,
    acquisition: Sequence[D],
    science:     Sequence[D]
  )

  object Config extends ConfigOptics {

    implicit def EqConfig[S: Eq, D: Eq]: Eq[Config[S, D]] =
      Eq.by { a => (
        a.static,
        a.acquisition,
        a.science
      )}

    final case class Create[CS, CD](
      static:      CS,
      acquisition: Sequence.Create[CD],
      science:     Sequence.Create[CD]
    ) {

      def create[S, D](
         implicit ev1: InputValidator[CS, S], ev2: InputValidator[CD, D]
       ): ValidatedInput[Config[S, D]] =
         (
           static.validateAndCreate[S],
           acquisition.create[D],
           science.create[D]
         ).mapN { (st, aq, sc) => Config(st, aq, sc) }

    }

    object Create {
      implicit def EqCreate[S: Eq, D: Eq]: Eq[Create[S, D]] =
        Eq.by { a => (
          a.static,
          a.acquisition,
          a.science
        )}

      implicit def DecoderCreate[S: Decoder, D: Decoder]:Decoder[Create[S, D]] =
        deriveDecoder[Create[S, D]]

      implicit def ValidatorCreate[CS, S, CD, D](
        implicit ev1: InputValidator[CS, S], ev2: InputValidator[CD, D]
      ): InputValidator[Create[CS, CD], Config[S, D]] =
        InputValidator.by[Create[CS, CD], Config[S, D]](_.create)

    }

  }

  sealed trait ConfigOptics { this: Config.type =>

    def static[S, D]: Lens[Config[S, D], S] =
      Lens[Config[S, D], S](_.static)(a => _.copy(static = a))

    def acquisition[S, D]: Lens[Config[S, D], Sequence[D]] =
      Lens[Config[S, D], Sequence[D]](_.acquisition)(a => _.copy(acquisition = a))

    def science[S, D]: Lens[Config[S, D], Sequence[D]] =
      Lens[Config[S, D], Sequence[D]](_.science)(a => _.copy(science = a))

  }

  sealed trait InstrumentConfig extends Product with Serializable {
    def instrument: Instrument

    def gmosNorth: Option[InstrumentConfig.GmosNorth] =
      this match {
        case gn: InstrumentConfig.GmosNorth => gn.some
        case _                              => Option.empty
      }

    def gmosSouth: Option[InstrumentConfig.GmosSouth] =
      this match {
        case gs: InstrumentConfig.GmosSouth => gs.some
        case _                              => Option.empty
      }
  }

  object InstrumentConfig {

    implicit val EqInstrumentConfig: Eq[InstrumentConfig] =
      Eq.instance {
        case (a: GmosNorth, b: GmosNorth) => a === b
        case (a: GmosSouth, b: GmosSouth) => a === b
        case _                            => false
      }

    final case class GmosNorth(config: Config[GmosModel.NorthStatic, GmosModel.NorthDynamic]) extends InstrumentConfig {
      override def instrument: Instrument =
        Instrument.GmosN
    }

    object GmosNorth {

      implicit val EqGmosNorth: Eq[GmosNorth] =
        Eq.by(_.config)

    }

    final case class GmosSouth(config: Config[GmosModel.SouthStatic, GmosModel.SouthDynamic]) extends InstrumentConfig {
      override def instrument: Instrument =
        Instrument.GmosS
    }

    object GmosSouth {

      implicit val EqGmosSouth: Eq[GmosSouth] =
        Eq.by(_.config)

    }

    final case class CreateGmosNorth(
      config: Config.Create[GmosModel.CreateNorthStatic, GmosModel.CreateNorthDynamic]
    ) {

      def create: ValidatedInput[GmosNorth] =
        config.create.map(GmosNorth.apply)
    }

    object CreateGmosNorth {

      implicit val DecoderCreateGmosNorth: Decoder[CreateGmosNorth] =
        deriveDecoder[CreateGmosNorth]

      implicit val EqCreateGmosNorth: Eq[CreateGmosNorth] =
        Eq.by(_.config)

      implicit val ValidatorCreateGmosNorth: InputValidator[CreateGmosNorth, GmosNorth] =
        InputValidator.by[CreateGmosNorth, GmosNorth](_.create)

    }

    final case class CreateGmosSouth(
      config: Config.Create[GmosModel.CreateSouthStatic, GmosModel.CreateSouthDynamic]
    ) {

      def create: ValidatedInput[GmosSouth] =
        config.create.map(GmosSouth.apply)

    }

    object CreateGmosSouth {

      implicit val DecoderCreateGmosSouth: Decoder[CreateGmosSouth] =
        deriveDecoder[CreateGmosSouth]

      implicit val EqCreateGmosSouth: Eq[CreateGmosSouth] =
        Eq.by(_.config)

      implicit val ValidatorCreateGmosSouth: InputValidator[CreateGmosSouth, GmosSouth] =
        InputValidator.by[CreateGmosSouth, GmosSouth](_.create)

    }

    /**
     * Input parameter used to create a configuration.  All optional but
     * validation will check that exactly one is defined.
     */
    final case class Create(
      gmosNorth: Option[InstrumentConfig.CreateGmosNorth],
      gmosSouth: Option[InstrumentConfig.CreateGmosSouth]
    ) {

      def create: ValidatedInput[InstrumentConfig] =
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

      def gmosNorth(gn: InstrumentConfig.CreateGmosNorth): Create =
        Empty.copy(gmosNorth = gn.some)

      def gmosNorth(
        s:  GmosModel.CreateNorthStatic,
        ac: SequenceModel.Sequence.Create[GmosModel.CreateNorthDynamic],
        sc: SequenceModel.Sequence.Create[GmosModel.CreateNorthDynamic]
      ): Create =
        gmosNorth(CreateGmosNorth(Config.Create(s, ac, sc)))

      def gmosSouth(gs: InstrumentConfig.CreateGmosSouth): Create =
        Empty.copy(gmosSouth = gs.some)

      def gmosSouth(
        s:  GmosModel.CreateSouthStatic,
        ac: SequenceModel.Sequence.Create[GmosModel.CreateSouthDynamic],
        sc: SequenceModel.Sequence.Create[GmosModel.CreateSouthDynamic]
      ): Create =
        gmosSouth(CreateGmosSouth(Config.Create(s, ac, sc)))

      implicit val DecoderCreate: Decoder[Create] =
        deriveDecoder[Create]

      implicit val EqCreate: Eq[Create] =
        Eq.by { a => (
          a.gmosNorth,
          a.gmosSouth
        )}

      implicit val InputValidatorCreate: InputValidator[Create, InstrumentConfig] =
        InputValidator.by(_.create)

    }


  }

}
