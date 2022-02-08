// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import io.circe.{Decoder, HCursor}


final case class ItcSpectroscopyResult(
  // mode: ObservingModeSpectroscopy
  itc: ItcResult
)

object ItcSpectroscopyResult {

  implicit val DecoderItcSpectroscopyResult: Decoder[ItcSpectroscopyResult] =
    (c: HCursor) =>
      for {
//        m <- c.downField("mode").as[ObservingModeSpectroscopy]
        i <- c.downField("itc").as[ItcResult]
      } yield ItcSpectroscopyResult(i)

  implicit val EqItcSpectroscopyResult: Eq[ItcSpectroscopyResult] =
    Eq.by { a => (
//    a.mode,
      a.itc
    )}

}
