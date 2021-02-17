// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.syntax.inputvalidator._

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.Lens

/**
 * Manual or explicit sequence representation.
 * @param static static configuration
 * @param acquisition acquisition steps
 * @param science science steps
 * @tparam S static configuration type
 * @tparam D dynamic (step) configuration type
 */
final case class ManualSequence[S, D](
  static:      S,
  acquisition: List[SequenceModel.Atom[D]],
  science:     List[SequenceModel.Atom[D]]
)

object ManualSequence extends ManualSequenceOptics {

  implicit def EqManualSequence[S: Eq, D: Eq]: Eq[ManualSequence[S, D]] =
    Eq.by { a => (
      a.static,
      a.acquisition,
      a.science
    )}

  /**
   * Input for manual sequence creation.
   */
  final case class Create[CS, CD](
    static:      CS,
    acquisition: List[SequenceModel.CreateAtom[CD]],
    science:     List[SequenceModel.CreateAtom[CD]]
  ) {

    def create[S, D](
      implicit ev1: InputValidator[CS, S], ev2: InputValidator[CD, D]
    ): ValidatedInput[ManualSequence[S, D]] =
      (
        static.validateAndCreate[S],
        acquisition.traverse(_.create),
        science.traverse(_.create)
      ).mapN { (st, aq, sc) => ManualSequence(st, aq, sc) }

  }

  object Create {

    implicit def EdCreate[S: Eq, D: Eq]: Eq[Create[S, D]] =
      Eq.by { a => (
        a.static,
        a.acquisition,
        a.science
      )}

    implicit def DecoderCreate[S: Decoder, D: Decoder]:Decoder[Create[S, D]] =
      deriveDecoder[Create[S, D]]

    implicit def ValidatorCreate[CS, S, CD, D](
      implicit ev1: InputValidator[CS, S], ev2: InputValidator[CD, D]
    ): InputValidator[Create[CS, CD], ManualSequence[S, D]] =
      InputValidator.by[Create[CS, CD], ManualSequence[S, D]](_.create)

  }

}

sealed trait ManualSequenceOptics { this: ManualSequence.type =>

  def static[S, D]: Lens[ManualSequence[S, D], S] =
    Lens[ManualSequence[S, D], S](_.static)(a => _.copy(static = a))

  def acquisition[S, D]: Lens[ManualSequence[S, D], List[SequenceModel.Atom[D]]] =
    Lens[ManualSequence[S, D], List[SequenceModel.Atom[D]]](_.acquisition)(a => _.copy(acquisition = a))

  def science[S, D]: Lens[ManualSequence[S, D], List[SequenceModel.Atom[D]]] =
    Lens[ManualSequence[S, D], List[SequenceModel.Atom[D]]](_.science)(a => _.copy(science = a))

}