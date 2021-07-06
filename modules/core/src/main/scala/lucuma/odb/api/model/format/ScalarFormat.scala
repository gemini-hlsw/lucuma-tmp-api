// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.format

import lucuma.core.optics.Format

import io.circe.Decoder

/**
 * Combines a `Format[String, A]` with a hint about the expected `String` format.
 * Used to create circe `Decoder`s and sangria `ScalarType`s.
 */
final case class ScalarFormat[A](format: Format[String, A], hint: String) {

  /**
   * Creates an `io.circe.Decoder` based on the format.
   */
  def decoder: Decoder[A] =
    Decoder.decodeString.emap { s =>
      format.getOption(s).toRight(s"Expected a string matching format `$hint`")
    }

}
