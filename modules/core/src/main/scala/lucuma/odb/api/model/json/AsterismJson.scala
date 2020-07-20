// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package json

import io.circe.Decoder

trait AsterismJson {

  import targetmath._

  implicit val DecoderAsterismCreateDefault: Decoder[Asterism.CreateDefault] =
    Decoder.forProduct3(
      "programs",
      "explicitBase",
      "targets"
    )(Asterism.CreateDefault.apply)

  implicit val DecoderAsterismEditDefault: Decoder[Asterism.EditDefault] =
    Decoder.forProduct4(
      "id",
      "existence",
      "explicitBase",
      "targets"
    )(Asterism.EditDefault.apply)

}

object asterism extends AsterismJson