// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.util.Enumerated

object SpectralDistributionModel {

  sealed trait Input extends Product with Serializable

  case object BlackBody  extends Input
  case object PowerLaw   extends Input
  case object Stellar    extends Input
  case object NonStellar extends Input

  object Input {
    implicit val EnumeratedInput: Enumerated[Input] =
      Enumerated.of(
        BlackBody,
        PowerLaw,
        Stellar,
        NonStellar
      )

    implicit val DecoderSpectralDistributionInput: Decoder[SpectralDistributionModel.Input] =
      deriveDecoder[SpectralDistributionModel.Input]

  }
}

