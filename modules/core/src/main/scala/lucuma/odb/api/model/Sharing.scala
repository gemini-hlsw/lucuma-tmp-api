// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/**
 * Input objects for sharing.
 */
object Sharing {

  final case class TargetObservationLinks(
    targets:      List[TargetModel.Id],
    observations: List[ObservationModel.Id]
  )

  object TargetObservationLinks {
    implicit val DecoderTargetObservationLinks: Decoder[TargetObservationLinks] =
      deriveDecoder[TargetObservationLinks]
  }

}
