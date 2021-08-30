// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.data.State
import cats.syntax.all._
import clue.data.Input
import io.circe.Decoder
import io.circe.generic.semiauto._
import lucuma.core.enum._
import lucuma.core.optics.syntax.lens._
import lucuma.odb.api.model.syntax.input._
import monocle.{Fold, Lens, Optional}
import monocle.macros.GenLens


final case class ConstraintSetModel(
  imageQuality:    ImageQuality,
  cloudExtinction: CloudExtinction,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRangeModel
)

object ConstraintSetModel extends ConstraintSetModelOptics {

  implicit val EqConstraintSet: Eq[ConstraintSetModel] = Eq.fromUniversalEquals

  /**
   * The loosest possible observing constraints.
   */
  val Any: ConstraintSetModel =
    ConstraintSetModel(
      ImageQuality.TwoPointZero,
      CloudExtinction.ThreePointZero,
      SkyBackground.Bright,
      WaterVapor.Wet,
      ElevationRangeModel.airmassRange.reverseGet(AirmassRange.Any)
    )

  /**
   * Constraints that are set by default when a new observation is created.
   */
  val Default: ConstraintSetModel =
    ConstraintSetModel(
      ImageQuality.PointEight,
      CloudExtinction.PointThree,
      SkyBackground.Bright,
      WaterVapor.Wet,
      ElevationRangeModel.airmassRange.reverseGet(AirmassRange.Default)
    )

  final case class Create(
    imageQuality:    ImageQuality,
    cloudExtinction: CloudExtinction,
    skyBackground:   SkyBackground,
    waterVapor:      WaterVapor,
    elevationRange:  ElevationRangeModel.Create
  ) {

    def create: ValidatedInput[ConstraintSetModel] =
      elevationRange.create.map { e =>
        ConstraintSetModel(
          imageQuality,
          cloudExtinction,
          skyBackground,
          waterVapor,
          e
        )
      }
  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] = deriveDecoder

    implicit val EqCreate: Eq[Create] = Eq.fromUniversalEquals
  }

  final case class Edit(
    imageQuality:    Input[ImageQuality]               = Input.ignore,
    cloudExtinction: Input[CloudExtinction]            = Input.ignore,
    skyBackground:   Input[SkyBackground]              = Input.ignore,
    waterVapor:      Input[WaterVapor]                 = Input.ignore,
    elevationRange:  Input[ElevationRangeModel.Create] = Input.ignore
  ) {

    def editor: ValidatedInput[State[ConstraintSetModel, Unit]] =
      (imageQuality.validateIsNotNull("imageQuality"),
       cloudExtinction.validateIsNotNull("cloudExtinction"),
       skyBackground.validateIsNotNull("skyBackground"),
       waterVapor.validateIsNotNull("waterVapor"),
       elevationRange.validateNotNullable("elevationRange")(_.create)
      ).mapN { (i, c, s, w, el) =>
        for {
          _ <- ConstraintSetModel.imageQuality    := i
          _ <- ConstraintSetModel.cloudExtinction := c
          _ <- ConstraintSetModel.skyBackground   := s
          _ <- ConstraintSetModel.waterVapor      := w
          _ <- ConstraintSetModel.elevationRange  := el
        } yield ()
      }
  }

  object Edit {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] = deriveConfiguredDecoder[Edit]

    implicit val EqEdit: Eq[Edit] = Eq.fromUniversalEquals
  }

}

trait ConstraintSetModelOptics {

  /** @group Optics */
  lazy val imageQuality: Lens[ConstraintSetModel, ImageQuality] =
    GenLens[ConstraintSetModel](_.imageQuality)

  /** @group Optics */
  lazy val cloudExtinction: Lens[ConstraintSetModel, CloudExtinction] =
    GenLens[ConstraintSetModel](_.cloudExtinction)

  /** @group Optics */
  lazy val skyBackground: Lens[ConstraintSetModel, SkyBackground] =
    GenLens[ConstraintSetModel](_.skyBackground)

  /** @group Optics */
  lazy val waterVapor: Lens[ConstraintSetModel, WaterVapor] =
    GenLens[ConstraintSetModel](_.waterVapor)

  /** @group Optics */
  lazy val elevationRange: Lens[ConstraintSetModel, ElevationRangeModel] =
    GenLens[ConstraintSetModel](_.elevationRange)

  /** @group Optics */
  lazy val airmass: Optional[ConstraintSetModel, AirmassRange] =
    elevationRange.andThen(ElevationRangeModel.airmassRange)

  /** @group Optics */
  lazy val airMassMin: Fold[ConstraintSetModel, AirmassRange.DecimalValue] =
    airmass.andThen(AirmassRange.min)

  /** @group Optics */
  lazy val airMassMax: Fold[ConstraintSetModel, AirmassRange.DecimalValue] =
    airmass.andThen(AirmassRange.max)

  /** @group Optics */
  lazy val hourAngle: Optional[ConstraintSetModel, HourAngleRange] =
    elevationRange.andThen(ElevationRangeModel.hourAngleRange)

  /** @group Optics */
  lazy val hourAngleMin: Fold[ConstraintSetModel, HourAngleRange.DecimalHour] =
    hourAngle.andThen(HourAngleRange.minHours)

  /** @group Optics */
  lazy val hourAngleMax: Fold[ConstraintSetModel, HourAngleRange.DecimalHour] =
    hourAngle.andThen(HourAngleRange.maxHours)
}
