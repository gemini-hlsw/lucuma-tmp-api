// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package json

import lucuma.core.math.{Declination, Epoch, RightAscension}
import io.circe.Decoder
import lucuma.core.model.EphemerisKey

trait TargetJson {

  import format.target._

  implicit val DecoderDeclination: Decoder[Declination] =
    FormatDeclination.decoder

  implicit val DecoderEpoch: Decoder[Epoch] =
    FormatEpoch.decoder

  implicit val DecoderRightAscension: Decoder[RightAscension] =
    FormatRightAscension.decoder

  implicit val DecoderEphemerisKey: Decoder[EphemerisKey] =
    FormatEphemerisKey.decoder

}

object target extends TargetJson
