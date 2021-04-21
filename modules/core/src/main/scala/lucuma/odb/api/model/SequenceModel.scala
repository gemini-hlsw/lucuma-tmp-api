// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.macros.Lenses

/**
 * Sequence representation.
 *
 * @param atoms atoms that make up the sequence
 *
 * @tparam D dynamic (step) configuration type
 */
@Lenses final case class SequenceModel[D](
  atoms: List[AtomModel[D]]
)


object SequenceModel {

  implicit def EqSequenceModel[D: Eq]: Eq[SequenceModel[D]] =
    Eq.by { _.atoms }

  final case class Create[CD](
    atoms: List[AtomModel.Create[CD]]
  ) {

    def create[D](implicit ev: InputValidator[CD, D]): ValidatedInput[SequenceModel[D]] =
      atoms.traverse(_.create).map { as => SequenceModel(as) }

  }

  object Create {

    implicit def EdCreate[D: Eq]: Eq[Create[D]] =
      Eq.by { _.atoms }

    implicit def DecoderCreate[D: Decoder]:Decoder[Create[D]] =
      deriveDecoder[Create[D]]

    implicit def ValidatorCreate[CD, D](
      implicit ev: InputValidator[CD, D]
    ): InputValidator[Create[CD], SequenceModel[D]] =
      InputValidator.by[Create[CD], SequenceModel[D]](_.create)

  }

}
