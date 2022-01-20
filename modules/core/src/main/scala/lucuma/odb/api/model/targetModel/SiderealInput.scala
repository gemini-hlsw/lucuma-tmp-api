// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperMotion, RadialVelocity, RightAscension}
import lucuma.core.model.{CatalogInfo, SiderealTracking, Target}
import lucuma.core.model.Target.Sidereal
import lucuma.odb.api.model.{DeclinationModel, EditorInput, EitherInput, ParallaxModel, ProperMotionModel, RadialVelocityModel, RightAscensionModel, ValidatedInput}
import lucuma.odb.api.model.json.target._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.optional._
import lucuma.odb.api.model.targetModel.SourceProfileModel.SourceProfileInput
import monocle.{Focus, Lens, Optional}


// Editor for (SiderealTracking, Option[CatalogInfo]).  It's a bit weird in that
// it wants to be an editor for Sidereal, but that has target name and source
// profile which belong on Target.

final case class SiderealInput(
  ra:               Input[RightAscensionModel.Input]  = Input.ignore,
  dec:              Input[DeclinationModel.Input]     = Input.ignore,
  epoch:            Input[Epoch]                      = Input.ignore,
  properMotion:     Input[ProperMotionModel.Input]    = Input.ignore,
  radialVelocity:   Input[RadialVelocityModel.Input]  = Input.ignore,
  parallax:         Input[ParallaxModel.Input]        = Input.ignore,
  catalogInfo:      Input[CatalogInfoInput]           = Input.ignore
) extends EditorInput[(SiderealTracking, Option[CatalogInfo])] {

  val toSiderealTracking: ValidatedInput[SiderealTracking] =
    (ra.notMissingAndThen("sidereal 'ra'")(_.toRightAscension),
     dec.notMissingAndThen("sidereal 'dec'")(_.toDeclination),
     properMotion.toOption.traverse(_.toProperMotion),
     radialVelocity.toOption.traverse(_.toRadialVelocity),
     parallax.toOption.traverse(_.toParallax)
    ).mapN { (ra, dec, pm, rv, px) =>
      SiderealTracking(
        Coordinates(ra, dec),
        epoch.toOption.getOrElse(Epoch.J2000),
        pm,
        rv,
        px
      )
    }

  override val create: ValidatedInput[(SiderealTracking, Option[CatalogInfo])] =
    (toSiderealTracking, catalogInfo.toOption.traverse(_.create)).tupled

  def createTarget(
    name:          NonEmptyString,
    sourceProfile: SourceProfileInput
  ): ValidatedInput[Target] =
    (create, sourceProfile.create).mapN { case ((track, catInfo), profile) =>
      Target.Sidereal(name, track, profile, catInfo)
    }

  override val edit: StateT[EitherInput, (SiderealTracking, Option[CatalogInfo]), Unit] = {
    val validArgs =
      (ra   .validateNotNullable("ra")(_.toRightAscension),
       dec  .validateNotNullable("dec")(_.toDeclination),
       epoch.validateIsNotNull("epoch"),
       properMotion  .validateNullable(_.toProperMotion),
       radialVelocity.validateNullable(_.toRadialVelocity),
       parallax      .validateNullable(_.toParallax)
      ).tupled.toEither

    import SiderealInput.optics

    for {
      args <- StateT.liftF(validArgs)
      (r, d, e, pm, rv, px) = args

      _ <- optics.baseRa         := r
      _ <- optics.baseDec        := d
      _ <- optics.epoch          := e
      _ <- optics.properMotion   := pm
      _ <- optics.radialVelocity := rv
      _ <- optics.parallax       := px
      _ <- optics.catalogInfo    :? catalogInfo
    } yield ()

  }

  val targetEditor: StateT[EitherInput, Target, Unit] =
    StateT.modifyF[EitherInput, Target] {
      case Target.Sidereal(name, tracking, sourceProfile, catalogInfo)  =>
        edit.runS((tracking, catalogInfo)).map { case (t, c) =>
          Target.Sidereal(name, t, sourceProfile, c)
        }

      case Target.Nonsidereal(name, _, sourceProfile) =>
        create.map { case (t, c) => Target.Sidereal(name, t, sourceProfile, c) }.toEither
    }

}

object SiderealInput {

  object optics {
    type SiderealPair = (SiderealTracking, Option[CatalogInfo])

    val siderealPair: Lens[Sidereal, SiderealPair] =
      Lens[Sidereal, SiderealPair](
        s    => (s.tracking, s.catalogInfo)
      )(
        pair => _.copy(tracking = pair._1, catalogInfo = pair._2)
      )

    val target: Optional[Target, (SiderealTracking, Option[CatalogInfo])] =
      Target.sidereal.andThen(siderealPair)

    val tracking: Lens[SiderealPair, SiderealTracking] =
      Focus[SiderealPair](_._1)

    val catalogInfo: Lens[SiderealPair, Option[CatalogInfo]] =
      Focus[SiderealPair](_._2)

    val baseRa: Lens[SiderealPair, RightAscension] =
      tracking.andThen(SiderealTracking.baseCoordinates.andThen(Coordinates.rightAscension))

    val baseDec: Lens[SiderealPair, Declination] =
      tracking.andThen(SiderealTracking.baseCoordinates.andThen(Coordinates.declination))

    val epoch: Lens[SiderealPair, Epoch] =
      tracking.andThen(SiderealTracking.epoch)

    val properMotion: Lens[SiderealPair, Option[ProperMotion]] =
      tracking.andThen(SiderealTracking.properMotion)

    val radialVelocity: Lens[SiderealPair, Option[RadialVelocity]] =
      tracking.andThen(SiderealTracking.radialVelocity)

    val parallax: Lens[SiderealPair, Option[Parallax]] =
      tracking.andThen(SiderealTracking.parallax)
  }

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderEditSidereal: Decoder[SiderealInput] =
    deriveConfiguredDecoder[SiderealInput]

  implicit val EqEditSidereal: Eq[SiderealInput] =
    Eq.by { a => (
      a.ra,
      a.dec,
      a.epoch,
      a.properMotion,
      a.radialVelocity,
      a.parallax,
      a.catalogInfo
    )}

}

