// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.State
import clue.data.Input
import cats.syntax.apply._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import lucuma.core.model.{EphemerisKey, Target}
import lucuma.core.optics.syntax.optional._
import lucuma.odb.api.model.ValidatedInput
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.syntax.input._



final case class EditNonsiderealInput(
  select: SelectTargetInput,
  name:   Input[NonEmptyString] = Input.ignore,
  key:    Input[EphemerisKey]   = Input.ignore,
) extends TargetEditor {

  override val editor: ValidatedInput[State[Target, Unit]] =
    (name.validateIsNotNull("name"),
     key.validateIsNotNull("key")
    ).mapN { case (n, k) =>
      for {
        _ <- Target.name         := n
        _ <- Target.ephemerisKey := k
      } yield ()
    }

}

object EditNonsiderealInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderEditNonSidereal: Decoder[EditNonsiderealInput] =
    deriveConfiguredDecoder[EditNonsiderealInput]

  implicit val EqEditNonsidereal: Eq[EditNonsiderealInput] =
    Eq.by(en => (
      en.select,
      en.name,
      en.key
    ))

}
