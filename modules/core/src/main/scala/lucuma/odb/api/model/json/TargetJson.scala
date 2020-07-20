// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package json

import io.circe.Decoder


trait TargetJson {

  import targetmath._

  implicit val DecoderCreateNonsidereal: Decoder[Target.CreateNonsidereal] =
    Decoder.forProduct4(
      "pids",
      "name",
      "key",
      "des"
    )(Target.CreateNonsidereal.apply)

  implicit val DecoderCreateSidereal: Decoder[Target.CreateSidereal] =
    Decoder.forProduct7(
      "pids",
      "name",
      "ra",
      "dec",
      "epoch",
      "properVelocity",
      "radialVelocity"
    )(Target.CreateSidereal.apply)

  implicit val DecoderEditSidereal: Decoder[Target.EditSidereal] =
    Decoder.forProduct8(
      "tid",
      "existence",
      "name",
      "ra",
      "dec",
      "epoch",
      "properVelocity",
      "radialVelocity"
    )(Target.EditSidereal.apply)

}

object target extends TargetJson
