// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.syntax.all._
import clue.GraphQLOperation
import io.circe.{Decoder, Encoder, HCursor, Json}

object ItcQuery extends GraphQLOperation[Unit] {
  type Data      = List[ItcSpectroscopyResult]
  type Variables = ItcSpectroscopyInput

  override val document: String =
    """
      query Spectroscopy($spec: SpectroscopyModeInput!) {
        spectroscopy(input: $spec) {
          results {
            itc {
              ... on ItcSuccess {
                exposures
                exposureTime {
                  microseconds
                }
                signalToNoise
                resultType
              }
              ... on ItcError {
                msg
                resultType
              }
            }
          }
        }
      }
    """

  override val varEncoder: Encoder[Variables] =
    Encoder.instance[ItcSpectroscopyInput] { inp =>
      Json.obj(
        "spec" -> Encoder[ItcSpectroscopyInput].apply(inp)
      )
    }

  override val dataDecoder: Decoder[List[ItcSpectroscopyResult]] =
    (c: HCursor) =>
      for {
        lst <- c.downField("spectroscopy").as[List[Json]]
        spc <- lst.flatTraverse(_.hcursor.downField("results").as[List[ItcSpectroscopyResult]])
      } yield spc

}
