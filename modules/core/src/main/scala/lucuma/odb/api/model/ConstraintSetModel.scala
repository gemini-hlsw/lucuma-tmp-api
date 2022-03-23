// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import io.circe.Decoder
import lucuma.core.enum._
import lucuma.core.model.{ConstraintSet, ElevationRange}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._


object ConstraintSetModel {

  /**
   * Constraints that are set by default when a new observation is created.
   */
  val Default: ConstraintSet =
    ConstraintSet(
      ImageQuality.PointEight,
      CloudExtinction.PointThree,
      SkyBackground.Bright,
      WaterVapor.Wet,
      ElevationRange.AirMass.Default
    )
}


final case class ConstraintSetInput(
  imageQuality:    Input[ImageQuality]        = Input.ignore,
  cloudExtinction: Input[CloudExtinction]     = Input.ignore,
  skyBackground:   Input[SkyBackground]       = Input.ignore,
  waterVapor:      Input[WaterVapor]          = Input.ignore,
  elevationRange:  Input[ElevationRangeInput] = Input.ignore
) extends EditorInput[ConstraintSet] {

  override val create: ValidatedInput[ConstraintSet] =
    (imageQuality.notMissing("imageQuality"),
     cloudExtinction.notMissing("cloudExtinction"),
     skyBackground.notMissing("skyBackground"),
     waterVapor.notMissing("waterVapor"),
     elevationRange.notMissingAndThen("elevationRange")(_.create)
    ).mapN { case (iq, cc, sb, wv, el) =>
      ConstraintSet(iq, cc, sb, wv, el)
    }

  override val edit: StateT[EitherInput, ConstraintSet, Unit] = {
    val validArgs =
      (imageQuality.validateIsNotNull("imageQuality"),
       cloudExtinction.validateIsNotNull("cloudExtinction"),
       skyBackground.validateIsNotNull("skyBackground"),
       waterVapor.validateIsNotNull("waterVapor")
      ).tupled

    for {
      args <- validArgs.liftState
      (i, c, s, w) = args
      _ <- ConstraintSet.imageQuality    := i
      _ <- ConstraintSet.cloudExtinction := c
      _ <- ConstraintSet.skyBackground   := s
      _ <- ConstraintSet.waterVapor      := w
      _ <- ConstraintSet.elevationRange  :! elevationRange
    } yield ()
  }
}

object ConstraintSetInput {
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderConstraintSetInput: Decoder[ConstraintSetInput] =
    deriveConfiguredDecoder[ConstraintSetInput]

  implicit val EqConstraintSetInput: Eq[ConstraintSetInput] =
    Eq.by { a => (
      a.imageQuality,
      a.cloudExtinction,
      a.skyBackground,
      a.waterVapor,
      a.elevationRange
    )}
}

