// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import clue.data.Input
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.enum.ScienceMode
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import monocle.Lens
import monocle.Focus

final case class ScienceRequirements(
  mode:                     ScienceMode,
  spectroscopyRequirements: SpectroscopyScienceRequirements
)

object ScienceRequirements extends ScienceRequirementsOptics {
  val Default: ScienceRequirements = ScienceRequirements(ScienceMode.Spectroscopy, SpectroscopyScienceRequirements.Default)

  implicit val eqScienceRequirements: Eq[ScienceRequirements] =
    Eq.by(x => (x.mode, x.spectroscopyRequirements))
}

object ScienceRequirementsModel {
  final case class Create(
    mode: ScienceMode,
    spectroscopyRequirements: SpectroscopyScienceRequirementsModel.Create
  ) {
    val create: ValidatedInput[ScienceRequirements] =
      spectroscopyRequirements.create.map { s =>
        ScienceRequirements(mode, s)
      }
  }

  object Create {
    val Default: Create = Create(ScienceMode.Spectroscopy, SpectroscopyScienceRequirementsModel.Create.Default)

    implicit val DecoderCreate: Decoder[Create] = deriveDecoder

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.mode,
        a.spectroscopyRequirements
      )}

  }

  final case class Edit(
    mode:                     Input[ScienceMode]                                = Input.ignore,
    spectroscopyRequirements: Option[SpectroscopyScienceRequirementsModel.Edit] = None
  ) {

    def editor: StateT[EitherInput, ScienceRequirements, Unit] =
      for {
        m <- StateT.liftF(mode.validateIsNotNull("mode").toEither)
        _ <- ScienceRequirements.mode := m
        _ <- ScienceRequirements
               .spectroscopyRequirements
               .transform(spectroscopyRequirements.fold(StateT.empty[EitherInput, SpectroscopyScienceRequirements, Unit])(_.edit))
      } yield ()

  }

  object Edit {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] = deriveConfiguredDecoder

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.mode,
        a.spectroscopyRequirements
      )}
  }

}

trait ScienceRequirementsOptics {
  val mode: Lens[ScienceRequirements, ScienceMode]                                         = Focus[ScienceRequirements](_.mode)

  val spectroscopyRequirements: Lens[ScienceRequirements, SpectroscopyScienceRequirements] = Focus[ScienceRequirements](_.spectroscopyRequirements)

}
