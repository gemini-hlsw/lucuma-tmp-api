// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.json.targetmath._
import lucuma.core.`enum`.{EphemerisKeyType, MagnitudeBand}
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperMotion, RadialVelocity, RightAscension}
import lucuma.core.model.{CatalogId, EphemerisKey, Magnitude, Program, SiderealTracking, Target}
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.syntax.input._
import cats.{Eq, Monad}
import cats.data._
import cats.implicits._
import cats.mtl.Stateful
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import monocle.{Lens, Optional}

import scala.collection.immutable.SortedMap

/**
 * A Target combining an ID with a `gem.Target`.
 */
final case class TargetModel(
  id:         Target.Id,
  existence:  Existence,
  target:     Target
)

object TargetModel extends TargetOptics {

  implicit val TopLevelTarget: TopLevelModel[Target.Id, TargetModel] =
    TopLevelModel.instance(_.id, existence)

  implicit val EqTarget: Eq[TargetModel] =
    Eq.by(t => (t.id, t.existence, t.target))

  object parse {

    def ephemerisKey(
      fieldName: String,
      key:       EphemerisKeyType,
      input:     String
    ): ValidatedInput[EphemerisKey] =
      EphemerisKey
        .fromTypeAndDes
        .getOption((key, input))
        .toValidNec(
          InputError.invalidField(fieldName, input, s"Invalid description for ephemeris key type `${key.shortName}`")
        )

  }

  private def createTarget[F[_]: Monad, T](
    db:         DatabaseState[T],
    targetId:   Option[Target.Id],
    programIds: Option[List[Program.Id]],
    gemTarget:  ValidatedInput[Target]
  )(
    implicit S: Stateful[F, T]
  ): F[ValidatedInput[TargetModel]] =
     for {
       i  <- db.target.getUnusedId[F](targetId)
       ps <- programIds.toList.flatten.traverse(db.program.lookupValidated[F])
       t   = (i, ps.sequence, gemTarget).mapN { (iʹ, _, tʹ) =>
         TargetModel(iʹ, Existence.Present, tʹ)
       }
       _  <- db.target.saveIfValid[F](t)(_.id)
     } yield t

  /**
   * Describes input used to create a nonsidereal target.
   *
   * @param targetId optional client-provided target id (if not specified one
   *                 will be created)
   * @param programIds associated program(s), if any (which either exist or
   *                   results in an error)
   * @param name target name
   * @param key ephemeris key type
   * @param des semi-permanent horizons identifier (relative to key type)
   */
  final case class CreateNonsidereal(
    targetId:   Option[Target.Id],
    programIds: Option[List[Program.Id]],
    name:       NonEmptyString,
    key:        EphemerisKeyType,
    des:        String,
    magnitudes: Option[List[MagnitudeModel.Input]]
  ) {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    val toGemTarget: ValidatedInput[Target] = {
      (toEphemerisKey,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (k, ms) =>
        Target(name, Left(k), SortedMap.from(ms.map(m => m.band -> m)))
      }
    }

    def create[F[_]: Monad, T](db: DatabaseState[T])(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =
      createTarget[F, T](db, targetId, programIds, toGemTarget)

  }

  object CreateNonsidereal {

    implicit val DecoderCreateNonsidereal: Decoder[CreateNonsidereal] =
      deriveDecoder[CreateNonsidereal]

    implicit val EqCreateNonsidereal: Eq[CreateNonsidereal] =
      Eq.by(cn => (
        cn.targetId,
        cn.programIds,
        cn.name,
        cn.key,
        cn.des,
        cn.magnitudes
      ))

  }

  /**
   * Describes input used to create a sidereal target.
   *
   * @param targetId optional client-provided target id (if not specified one
   *                 will be created)
   * @param programIds associated program(s), if any (which either exist or results
   *             in an error)
   * @param name target name
   * @param ra right ascension coordinate at epoch
   * @param dec declination coordinate at epoch
   * @param epoch time of the base observation
   * @param properMotion proper motion per year in right ascension and declination
   * @param radialVelocity radial velocity
   * @param parallax parallax
   */
  final case class CreateSidereal(
    targetId:       Option[Target.Id],
    programIds:     Option[List[Program.Id]],
    name:           NonEmptyString,
    catalogId:      Option[CatalogIdModel.Input],
    ra:             RightAscensionModel.Input,
    dec:            DeclinationModel.Input,
    epoch:          Option[Epoch],
    properMotion:   Option[ProperMotionModel.Input],
    radialVelocity: Option[RadialVelocityModel.Input],
    parallax:       Option[ParallaxModel.Input],
    magnitudes:     Option[List[MagnitudeModel.Input]]
  ) {

    val toSiderealTracking: ValidatedInput[SiderealTracking] =
      (catalogId.traverse(_.toCatalogId),
       ra.toRightAscension,
       dec.toDeclination,
       properMotion.traverse(_.toProperMotion),
       radialVelocity.traverse(_.toRadialVelocity),
       parallax.traverse(_.toParallax)
      ).mapN { (catalogId, ra, dec, pm, rv, px) =>
        SiderealTracking(
          catalogId,
          Coordinates(ra, dec),
          epoch.getOrElse(Epoch.J2000),
          pm,
          rv,
          px
        )
      }

    val toGemTarget: ValidatedInput[Target] =
      (toSiderealTracking,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (pm, ms) =>
        Target(name, Right(pm), SortedMap.from(ms.map(m => m.band -> m)))
      }

    def create[F[_]: Monad, T](db: DatabaseState[T])(implicit S: Stateful[F, T]): F[ValidatedInput[TargetModel]] =
      createTarget[F, T](db, targetId, programIds, toGemTarget)

  }

  object CreateSidereal {

    def fromRaDec(
      name: NonEmptyString,
      ra:   RightAscensionModel.Input,
      dec:  DeclinationModel.Input
    ): CreateSidereal =
      CreateSidereal(
        targetId       = None,
        programIds     = None,
        name           = name,
        catalogId      = None,
        ra             = ra,
        dec            = dec,
        epoch          = None,
        properMotion   = None,
        radialVelocity = None,
        parallax       = None,
        magnitudes     = None
      )

    implicit val DecoderCreateSidereal: Decoder[CreateSidereal] =
      deriveDecoder[CreateSidereal]

    implicit val EqCreateSidereal: Eq[CreateSidereal] =
      Eq.by(cs => (
        cs.targetId,
        cs.programIds,
        cs.name,
        cs.catalogId,
        cs.ra,
        cs.dec,
        cs.epoch,
        cs.properMotion,
        cs.radialVelocity,
        cs.parallax,
        cs.magnitudes
      ))

  }

  final case class EditNonsidereal(
    targetId:  Target.Id,
    existence: Option[Existence],
    name:      Option[NonEmptyString],
    key:       Option[EphemerisKey],
  ) {

    def id: Target.Id =
      targetId

    val editor: ValidatedInput[State[TargetModel, Unit]] =
      (for {
        _ <- TargetModel.existence    := existence
        _ <- TargetModel.name         := name
        _ <- TargetModel.ephemerisKey := key
      } yield ()).validNec

  }

  object EditNonsidereal {

    implicit val EqEditNonsidereal: Eq[EditNonsidereal] =
      Eq.by(en => (
        en.targetId,
        en.existence,
        en.name,
        en.key
      ))

  }

  final case class EditSidereal(
    targetId:         Target.Id,
    existence:        Input[Existence]                 = Input.ignore,
    name:             Input[NonEmptyString]            = Input.ignore,
    catalogId:        Input[CatalogIdModel.Input]      = Input.ignore,
    ra:               Input[RightAscensionModel.Input] = Input.ignore,
    dec:              Input[DeclinationModel.Input]    = Input.ignore,
    epoch:            Input[Epoch]                     = Input.ignore,
    properMotion:     Input[ProperMotionModel.Input]   = Input.ignore,
    radialVelocity:   Input[RadialVelocityModel.Input] = Input.ignore,
    parallax:         Input[ParallaxModel.Input]       = Input.ignore,
    magnitudes:       Option[List[MagnitudeModel.Input]],
    modifyMagnitudes: Option[List[MagnitudeModel.Input]],
    deleteMagnitudes: Option[List[MagnitudeBand]]
  ) {

    def id: Target.Id =
      targetId

    private def validateMags(
      ms: Option[List[MagnitudeModel.Input]]
    ): ValidatedInput[Option[SortedMap[MagnitudeBand, Magnitude]]] =
      ms.traverse(_.traverse(_.toMagnitude).map { lst =>
        SortedMap.from(lst.map(m => m.band -> m))
      })

    val editor: ValidatedInput[State[TargetModel, Unit]] =
      (existence     .validateIsNotNull("existence"),
       name          .validateIsNotNull("name"),
       catalogId     .validateNullable(_.toCatalogId),
       ra            .validateNotNullable("ra")(_.toRightAscension),
       dec           .validateNotNullable("dec")(_.toDeclination),
       epoch         .validateIsNotNull("epoch"),
       properMotion  .validateNullable(_.toProperMotion),
       radialVelocity.validateNullable(_.toRadialVelocity),
       parallax      .validateNullable(_.toParallax),
       validateMags(magnitudes),
       validateMags(modifyMagnitudes)
      ).mapN { (ex, name, catalogId, ra, dec, epoch, pm, rv, px, ms, mp) =>
        for {
          _ <- TargetModel.existence      := ex
          _ <- TargetModel.name           := name
          _ <- TargetModel.catalogId      := catalogId
          _ <- TargetModel.ra             := ra
          _ <- TargetModel.dec            := dec
          _ <- TargetModel.epoch          := epoch
          _ <- TargetModel.properMotion   := pm
          _ <- TargetModel.radialVelocity := rv
          _ <- TargetModel.parallax       := px
          _ <- TargetModel.magnitudes     := ms
          _ <- TargetModel.magnitudes.mod(_ ++ mp.getOrElse(SortedMap.empty[MagnitudeBand, Magnitude]))
          _ <- TargetModel.magnitudes.mod(_ -- deleteMagnitudes.toList.flatten)
        } yield ()
      }

  }

  object EditSidereal {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults


    implicit val DecoderEditSidereal: Decoder[EditSidereal] =
      deriveConfiguredDecoder[EditSidereal]

    implicit val EqEditSidereal: Eq[EditSidereal] =
      Eq.by(es => (
        es.targetId,
        es.existence,
        es.name,
        es.catalogId,
        es.ra,
        es.dec,
        es.epoch,
        es.properMotion,
        es.radialVelocity,
        es.parallax,
        es.magnitudes,
        es.modifyMagnitudes,
        es.deleteMagnitudes
      ))

  }

  final case class TargetEvent (
    id:       Long,
    editType: Event.EditType,
    value:    TargetModel,
  ) extends Event.Edit[TargetModel]

  object TargetEvent {
    def created(value: TargetModel)(id: Long): TargetEvent =
      TargetEvent(id, Event.EditType.Created, value)

    def updated(value: TargetModel)(id: Long): TargetEvent =
      TargetEvent(id, Event.EditType.Updated, value)
  }

}

trait TargetOptics { self: TargetModel.type =>

  val id: Lens[TargetModel, Target.Id] =
    Lens[TargetModel, Target.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[TargetModel, Existence] =
    Lens[TargetModel, Existence](_.existence)(a => b => b.copy(existence = a))

  val lucumaTarget: Lens[TargetModel, Target] =
    Lens[TargetModel, Target](_.target)(a => b => b.copy(target = a))

  val name: Lens[TargetModel, NonEmptyString] =
    lucumaTarget.andThen(Target.name)

  private val gemTargetEphemerisKey: Optional[Target, EphemerisKey] =
    Target.track.andThen(monocle.std.either.stdLeft[EphemerisKey, SiderealTracking])

  val ephemerisKey: Optional[TargetModel, EphemerisKey] =
    lucumaTarget.andThen(gemTargetEphemerisKey)

  private val gemTargetSiderealTracking: Optional[Target, SiderealTracking] =
    Target.track.andThen(monocle.std.either.stdRight[EphemerisKey, SiderealTracking])

  val siderealTracking: Optional[TargetModel, SiderealTracking] =
    lucumaTarget.andThen(gemTargetSiderealTracking)

  val catalogId: Optional[TargetModel, Option[CatalogId]] =
    siderealTracking.andThen(SiderealTracking.catalogId)

  val coordinates: Optional[TargetModel, Coordinates] =
    siderealTracking.andThen(SiderealTracking.baseCoordinates)

  val ra: Optional[TargetModel, RightAscension] =
    coordinates.andThen(Coordinates.rightAscension)

  val dec: Optional[TargetModel, Declination] =
    coordinates.andThen(Coordinates.declination)

  val epoch: Optional[TargetModel, Epoch] =
    siderealTracking.andThen(SiderealTracking.epoch)

  val properMotion: Optional[TargetModel, Option[ProperMotion]] =
    siderealTracking.andThen(SiderealTracking.properMotion)

  val radialVelocity: Optional[TargetModel, Option[RadialVelocity]] =
    siderealTracking.andThen(SiderealTracking.radialVelocity)

  val parallax: Optional[TargetModel, Option[Parallax]] =
    siderealTracking.andThen(SiderealTracking.parallax)

  val magnitudes: Lens[TargetModel, SortedMap[MagnitudeBand, Magnitude]] =
    lucumaTarget.andThen(Target.magnitudes)
}
