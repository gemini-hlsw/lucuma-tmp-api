// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.apply._
import cats.syntax.traverse._
import clue.data.Input
import clue.data.syntax._
import io.circe.Decoder
import lucuma.core.enum.ScienceMode
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.Lens
import monocle.Focus

final case class ScienceRequirements(
  mode:         ScienceMode,
  spectroscopy: SpectroscopyScienceRequirements
)

object ScienceRequirements extends ScienceRequirementsOptics {
  val Default: ScienceRequirements =
    ScienceRequirements(
      ScienceMode.Spectroscopy,
      SpectroscopyScienceRequirements.Default
    )

  implicit val eqScienceRequirements: Eq[ScienceRequirements] =
    Eq.by(a => (a.mode, a.spectroscopy))
}

final case class ScienceRequirementsInput(
  mode:         Input[ScienceMode]                          = Input.ignore,
  spectroscopy: Input[SpectroscopyScienceRequirementsInput] = Input.ignore
) extends EditorInput[ScienceRequirements] {

  override val create: ValidatedInput[ScienceRequirements] =
    (mode.notMissing("mode"),
     spectroscopy.toOption.traverse(_.create).map(_.getOrElse(SpectroscopyScienceRequirements.Default))
    ).mapN { (m, s) => ScienceRequirements(m, s) }

  override val edit: StateT[EitherInput, ScienceRequirements, Unit] =
    for {
      m <- mode.validateIsNotNull("mode").liftState
      _ <- ScienceRequirements.mode         := m
      _ <- ScienceRequirements.spectroscopy :! spectroscopy
    } yield ()

}

object ScienceRequirementsInput {
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  val Default: ScienceRequirementsInput =
    ScienceRequirementsInput(ScienceMode.Spectroscopy.assign, SpectroscopyScienceRequirementsInput.Default.assign)

  implicit val DecoderScienceRequirementsInput: Decoder[ScienceRequirementsInput] =
    deriveConfiguredDecoder

  implicit val EqScienceRequirementsInput: Eq[ScienceRequirementsInput] =
    Eq.by(a => (a.mode, a.spectroscopy))
}

trait ScienceRequirementsOptics {
  val mode: Lens[ScienceRequirements, ScienceMode] =
    Focus[ScienceRequirements](_.mode)

  val spectroscopy: Lens[ScienceRequirements, SpectroscopyScienceRequirements] =
    Focus[ScienceRequirements](_.spectroscopy)

}
