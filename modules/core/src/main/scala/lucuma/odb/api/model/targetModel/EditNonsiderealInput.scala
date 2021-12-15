// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.State
import clue.data.Input
import cats.syntax.functor._
import io.circe.Decoder
import lucuma.core.model.{EphemerisKey, Target}
import lucuma.core.optics.syntax.optional._
import lucuma.odb.api.model.ValidatedInput
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.syntax.input._



final case class EditNonsiderealInput(
  key:    Input[EphemerisKey]   = Input.ignore,
) {

  val editor: ValidatedInput[State[Target, Unit]] =
    key.validateIsNotNull("key").map { k =>
      (Target.ephemerisKey := k).void
    }

}

object EditNonsiderealInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderEditNonSidereal: Decoder[EditNonsiderealInput] =
    deriveConfiguredDecoder[EditNonsiderealInput]

  implicit val EqEditNonsidereal: Eq[EditNonsiderealInput] =
    Eq.by(_.key)

}
