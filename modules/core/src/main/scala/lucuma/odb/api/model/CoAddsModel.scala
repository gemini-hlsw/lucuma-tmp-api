// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosShort
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.Prism

final case class CoAddsModel(
  toPosShort: PosShort
)

object CoAddsModel {

  final val One: CoAddsModel =
    fromShort.getOption(1).get

  def fromShort: Prism[Short, CoAddsModel] =
    Prism((n: Short) => PosShort.from(n).toOption.map(CoAddsModel(_)))(_.toPosShort.value)

  implicit val EqCoAddsModel: Eq[CoAddsModel] =
    Eq.by(_.toPosShort.value)

  final case class Input(
    coadds: Int
  ) {

    val create: ValidatedInput[CoAddsModel] =
      fromShort
        .getOption(coadds.toShort)
        .toValidNec(InputError.fromMessage("CoAdds must be in the range (1, 32767)"))

  }

  object Input {

    implicit val DecoderCreate: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqCreate: Eq[Input] =
      Eq.by(_.coadds)

  }


}
