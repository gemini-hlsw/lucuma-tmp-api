// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats._
import cats.data.State
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.enum._
import lucuma.core.model.{ ConstraintSet, Program }
import lucuma.core.optics.syntax.lens._
import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.input._
import monocle.{ Fold, Lens, Optional }
import monocle.macros.GenLens

final case class ConstraintSetModel(
  id:              ConstraintSet.Id,
  existence:       Existence,
  programId:       Program.Id,
  name:            NonEmptyString,
  imageQuality:    ImageQuality,
  cloudExtinction: CloudExtinction,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRangeModel
)

object ConstraintSetModel extends ConstraintSetModelOptics {

  implicit val TopLevelConstraintSet: TopLevelModel[ConstraintSet.Id, ConstraintSetModel] =
    TopLevelModel.instance(_.id, ConstraintSetModel.existence)

  implicit val EqConstraintSet: Eq[ConstraintSetModel] = Eq.fromUniversalEquals

  final case class Create(
    constraintSetId: Option[ConstraintSet.Id],
    programId:       Program.Id,
    name:            NonEmptyString,
    imageQuality:    ImageQuality,
    cloudExtinction: CloudExtinction,
    skyBackground:   SkyBackground,
    waterVapor:      WaterVapor,
    elevationRange:  ElevationRangeModel.Create
  ) {

    def create[T](db: Database[T]): State[T, ValidatedInput[ConstraintSetModel]] =
      for {
        i <- db.constraintSet.getUnusedId(constraintSetId)
        p <- db.program.lookup(programId)
        c  = (i, p, elevationRange.create).mapN { (iʹ, _, e) =>
          ConstraintSetModel(
            iʹ,
            Present,
            programId,
            name,
            imageQuality,
            cloudExtinction,
            skyBackground,
            waterVapor,
            e
          )
        }
        _ <- db.constraintSet.saveValid(c)(_.id)
      } yield c
  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] = deriveDecoder

    implicit val EqCreate: Eq[Create] = Eq.fromUniversalEquals
  }

  final case class Edit(
    constraintSetId: ConstraintSet.Id,
    existence:       Input[Existence] = Input.ignore,
    name:            Input[NonEmptyString] = Input.ignore,
    imageQuality:    Input[ImageQuality] = Input.ignore,
    cloudExtinction: Input[CloudExtinction] = Input.ignore,
    skyBackground:   Input[SkyBackground] = Input.ignore,
    waterVapor:      Input[WaterVapor] = Input.ignore,
    elevationRange:  Input[ElevationRangeModel.Create] = Input.ignore
  ) {

    def id: ConstraintSet.Id = constraintSetId

    def editor: ValidatedInput[State[ConstraintSetModel, Unit]] =
      (existence.validateIsNotNull("existence"),
       name.validateIsNotNull("name"),
       imageQuality.validateIsNotNull("imageQuality"),
       cloudExtinction.validateIsNotNull("cloudExtinction"),
       skyBackground.validateIsNotNull("skyBackground"),
       waterVapor.validateIsNotNull("waterVapor"),
       elevationRange.validateNotNullable("elevationRange")(_.create)
      ).mapN { (e, n, i, c, s, w, el) =>
        for {
          _ <- ConstraintSetModel.existence := e
          _ <- ConstraintSetModel.name := n
          _ <- ConstraintSetModel.imageQuality := i
          _ <- ConstraintSetModel.cloudExtinction := c
          _ <- ConstraintSetModel.skyBackground := s
          _ <- ConstraintSetModel.waterVapor := w
          _ <- ConstraintSetModel.elevationRange := el
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

  final case class ConstraintSetEvent(
    id:       Long,
    editType: Event.EditType,
    value:    ConstraintSetModel
  ) extends Event.Edit[ConstraintSetModel]

  object ConstraintSetEvent {
    def created(value: ConstraintSetModel)(id: Long): ConstraintSetEvent =
      ConstraintSetEvent(id, Event.EditType.Created, value)

    def updated(value: ConstraintSetModel)(id: Long): ConstraintSetEvent =
      ConstraintSetEvent(id, Event.EditType.Updated, value)
  }
}

trait ConstraintSetModelOptics {

  /** @group Optics */
  lazy val id: Lens[ConstraintSetModel, ConstraintSet.Id] =
    GenLens[ConstraintSetModel](_.id)

  /** @group Optics */
  lazy val existence: Lens[ConstraintSetModel, Existence] =
    GenLens[ConstraintSetModel](_.existence)

  /** @group Optics */
  lazy val name: Lens[ConstraintSetModel, NonEmptyString] =
    GenLens[ConstraintSetModel](_.name)

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
    elevationRange.composePrism(ElevationRangeModel.airmassRange)

  /** @group Optics */
  lazy val airMassMin: Fold[ConstraintSetModel, AirmassRange.DecimalValue] =
    airmass.composeGetter(AirmassRange.min)

  /** @group Optics */
  lazy val airMassMax: Fold[ConstraintSetModel, AirmassRange.DecimalValue] =
    airmass.composeGetter(AirmassRange.max)

  /** @group Optics */
  lazy val hourAngle: Optional[ConstraintSetModel, HourAngleRange] =
    elevationRange.composePrism(ElevationRangeModel.hourAngleRange)

  /** @group Optics */
  lazy val hourAngleMin: Fold[ConstraintSetModel, HourAngleRange.DecimalHour] =
    hourAngle.composeGetter(HourAngleRange.minHours)

  /** @group Optics */
  lazy val hourAngleMax: Fold[ConstraintSetModel, HourAngleRange.DecimalHour] =
    hourAngle.composeGetter(HourAngleRange.maxHours)
}
