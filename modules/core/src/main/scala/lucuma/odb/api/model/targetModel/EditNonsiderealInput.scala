// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.{EitherNec, StateT}
import clue.data.Input
import io.circe.Decoder
import lucuma.core.model.{EphemerisKey, Target}
import lucuma.odb.api.model.InputError
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.optional._



final case class EditNonsiderealInput(
  key:    Input[EphemerisKey]   = Input.ignore,
) {

  val editor: StateT[EitherNec[InputError, *], Target, Unit] =
    for {
      k <- StateT.liftF(key.validateIsNotNull("key").toEither)
      _ <- Target.ephemerisKey := k
    } yield ()

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
