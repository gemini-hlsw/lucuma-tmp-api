// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.gmos.longslit

import cats.Eq
import cats.data.StateT
import cats.syntax.apply._
import clue.data.Input
import io.circe.Decoder
import lucuma.odb.api.model.{EditorInput, EitherInput, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._


final case class BasicConfigInput[G, F, U](
  grating: Input[G] = Input.ignore,
  filter:  Input[F] = Input.ignore,
  fpu:     Input[U] = Input.ignore
) extends EditorInput[BasicConfig[G, F, U]] {

  override val create: ValidatedInput[BasicConfig[G, F, U]] =
    (grating.notMissing("grating"),
      fpu.notMissing("fpu")
    ).mapN { case (d, u) =>
      BasicConfig(d, filter.toOption, u)
    }

  override val edit: StateT[EitherInput, BasicConfig[G, F, U], Unit] = {

    val validArgs =
      (grating.validateIsNotNull("grating"),
        fpu.validateIsNotNull("fpu")
      ).tupled

    for {
      args <- validArgs.liftState
      (grating, fpu) = args
      _    <- BasicConfig.grating[G, F, U] := grating
      _    <- BasicConfig.filter[G, F, U]  := filter.toOption
      _    <- BasicConfig.fpu[G, F, U]     := fpu
    } yield ()
  }

}

object BasicConfigInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit def DecoderBasicConfigInput[G: Decoder, F: Decoder, U: Decoder]: Decoder[BasicConfigInput[G, F, U]] =
    deriveConfiguredDecoder[BasicConfigInput[G, F, U]]

  implicit def EqBasicConfigInput[G: Eq, F: Eq, U: Eq]: Eq[BasicConfigInput[G, F, U]] =
    Eq.by { a => (
      a.grating,
      a.filter,
      a.fpu
    )}

}
