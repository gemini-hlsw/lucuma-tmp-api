// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.all._
import cats.data.State
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.optics.syntax.lens._
import lucuma.core.model.Observation
import lucuma.odb.api.model.syntax.input._
import clue.data.Input
import monocle.Lens
import monocle.Focus

final case class ScienceRequirements(
  mode:                     ScienceMode,
  spectroscopyRequirements: SpectroscopyScienceRequirements
)

object ScienceRequirements {
  val Default: ScienceRequirements = ScienceRequirements(ScienceMode.Spectroscopy, SpectroscopyScienceRequirements.Default)

  val mode: Lens[ScienceRequirements, ScienceMode]                                         = Focus[ScienceRequirements](_.mode)
  val spectroscopyRequirements: Lens[ScienceRequirements, SpectroscopyScienceRequirements] = Focus[ScienceRequirements](_.spectroscopyRequirements)

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

    def editor: ValidatedInput[State[ScienceRequirements, Unit]] =
      (mode.validateIsNotNull("mode"), spectroscopyRequirements.traverse(_.edit)).mapN { (m, s) =>
        for {
          _ <- ScienceRequirements.mode                     := m
          _ <- State.modify[ScienceRequirements] { o =>
            s.fold(o) { ed => ScienceRequirements.spectroscopyRequirements.modify(ed.runS(_).value)(o) }
          }
        } yield ()
      }

  }

  object Edit {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] = deriveConfiguredDecoder
  }

  final case class BulkEdit(
    scienceRequirements: Edit,
    observationIds:      List[Observation.Id]
  )

  object BulkEdit {

    implicit val DecoderEdit: Decoder[BulkEdit] =
      deriveDecoder[BulkEdit]

    implicit val EqBulkEdit: Eq[BulkEdit] =
      Eq.fromUniversalEquals

  }

}
