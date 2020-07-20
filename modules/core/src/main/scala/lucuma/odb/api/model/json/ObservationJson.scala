// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package json

import io.circe.Decoder

trait ObservationJson {

  implicit val DecoderObservationCreate: Decoder[Observation.Create] =
    Decoder.forProduct3(
      "pid",
      "name",
      "asterism"
    )(Observation.Create.apply)

  implicit val DecoderObservationEdit: Decoder[Observation.Edit] =
    Decoder.forProduct4(
      "id",
      "existence",
      "name",
      "asterism"
    )(Observation.Edit.apply)

}

object observation extends ObservationJson
