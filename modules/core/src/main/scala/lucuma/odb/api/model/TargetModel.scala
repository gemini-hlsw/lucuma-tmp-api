// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.syntax.all._
import lucuma.odb.api.model.json.targetmath._
import lucuma.core.`enum`.{EphemerisKeyType, MagnitudeBand}
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperVelocity, RadialVelocity, RightAscension}
import lucuma.core.model.{EphemerisKey, Magnitude, SiderealTracking, Target}
import lucuma.core.util.Gid
import cats.data._
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.{Lens, Optional}

import scala.collection.immutable.SortedMap

/**
 * A Target combining an ID with a `gem.Target`.
 */
final case class TargetModel(
  id:         TargetModel.Id,
  existence:  Existence,
  target:     Target
)

object TargetModel extends TargetOptics {

  final case class Id(value: PosLong) {
    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {
    implicit val GidTargetId: Gid[Id] =
      Gid.instance('t', _.value, apply)
  }

  implicit val TopLevelTarget: TopLevelModel[Id, TargetModel] =
    TopLevelModel.instance(_.id, existence)


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

  private def targetName(name: String): ValidatedInput[NonEmptyString] =
    NonEmptyString
      .from(name)
      .leftMap(er => InputError.invalidField("name", name, s"Target name must be non-empty: $er"))
      .toValidatedNec

  /**
   * Describes input used to create a nonsidereal target.
   *
   * @param programIds associated program(s), if any (which either exist or
   *                   results in an error)
   * @param name target name
   * @param key ephemeris key type
   * @param des semi-permanent horizons identifier (relative to key type)
   */
  final case class CreateNonsidereal(
    programIds: List[ProgramModel.Id],
    name:       String,
    key:        EphemerisKeyType,
    des:        String
  ) {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    val toGemTarget: ValidatedInput[Target] = {
      (targetName(name), toEphemerisKey).mapN { (n, k) =>
        Target(n, Left(k), SortedMap.empty[MagnitudeBand, Magnitude])
      }
    }
  }

  object CreateNonsidereal {

    implicit val DecoderCreateNonsidereal: Decoder[CreateNonsidereal] =
      deriveDecoder[CreateNonsidereal]

  }

  /**
   * Describes input used to create a sidereal target.
   *
   * @param programIds associated program(s), if any (which either exist or results
   *             in an error)
   * @param name target name
   * @param ra right ascension coordinate at epoch
   * @param dec declination coordinate at epoch
   * @param epoch time of the base observation
   * @param properVelocity proper velocity per year in right ascension and declination
   * @param radialVelocity radial velocity
   * @param parallax parallax
   */
  final case class CreateSidereal(
    programIds:     List[ProgramModel.Id],
    name:           String,
    ra:             RightAscensionModel.Input,
    dec:            DeclinationModel.Input,
    epoch:          Option[Epoch],
    properVelocity: Option[ProperVelocityModel.Input],
    radialVelocity: Option[RadialVelocityModel.Input],
    parallax:       Option[ParallaxModel.Input]
  ) {

    val toSiderealTracking: ValidatedInput[SiderealTracking] =
      (ra.toRightAscension,
       dec.toDeclination,
       properVelocity.traverse(_.toProperVelocity),
       radialVelocity.traverse(_.toRadialVelocity),
       parallax.traverse(_.toParallax)
      ).mapN { (ra, dec, pv, rv, px) =>
        SiderealTracking(
          None,
          Coordinates(ra, dec),
          epoch.getOrElse(Epoch.J2000),
          pv,
          rv,
          px
        )
      }

    val toGemTarget: ValidatedInput[Target] =
      (targetName(name), toSiderealTracking).mapN { (n, pm) =>
        Target(n, Right(pm), SortedMap.empty)
      }

  }

  object CreateSidereal {

    implicit val DecoderCreateSidereal: Decoder[CreateSidereal] =
      deriveDecoder[CreateSidereal]

  }

  final case class EditNonsidereal(
    targetId:  Id,
    existence: Option[Existence],
    name:      Option[String],
    key:       Option[EphemerisKey],
  ) extends Editor[Id, TargetModel] {

    override def id: Id =
      targetId

    override val editor: ValidatedInput[State[TargetModel, Unit]] =
      (for {
        _ <- TargetModel.existence    := existence
        _ <- TargetModel.name         := name.flatMap(n => NonEmptyString.from(n).toOption)
        _ <- TargetModel.ephemerisKey := key
      } yield ()).validNec

  }

  final case class EditSidereal(
    targetId:       Id,
    existence:      Option[Existence],
    name:           Option[String],
    ra:             Option[RightAscensionModel.Input],
    dec:            Option[DeclinationModel.Input],
    epoch:          Option[Epoch],
    properVelocity: Option[Option[ProperVelocityModel.Input]],
    radialVelocity: Option[Option[RadialVelocityModel.Input]],
    parallax:       Option[Option[ParallaxModel.Input]]
  ) extends Editor[Id, TargetModel] {

    override def id: Id =
      targetId

    override val editor: ValidatedInput[State[TargetModel, Unit]] =
      (ra.traverse(_.toRightAscension),
       dec.traverse(_.toDeclination),
       Nested(properVelocity).traverse(_.toProperVelocity).map(_.value),
       Nested(radialVelocity).traverse(_.toRadialVelocity).map(_.value),
       Nested(parallax).traverse(_.toParallax).map(_.value)
      ).mapN { (ra, dec, pv, rv, px) =>
        for {
          _ <- TargetModel.existence      := existence
          _ <- TargetModel.name           := name.flatMap(n => NonEmptyString.from(n).toOption)
          _ <- TargetModel.ra             := ra
          _ <- TargetModel.dec            := dec
          _ <- TargetModel.epoch          := epoch
          _ <- TargetModel.properVelocity := pv
          _ <- TargetModel.radialVelocity := rv
          _ <- TargetModel.parallax       := px
        } yield ()
      }

  }

  object EditSidereal {

    implicit val DecoderEditSidereal: Decoder[EditSidereal] =
      deriveDecoder[EditSidereal]

  }

  final case class TargetProgramLinks(
    targetId:   Id,
    programIds: List[ProgramModel.Id]
  )

  object TargetProgramLinks {

    implicit val DecoderTargetProgramLinks: Decoder[TargetProgramLinks] =
      deriveDecoder[TargetProgramLinks]

  }

  final case class TargetCreatedEvent (
    id:    Long,
    value: TargetModel,
  ) extends Event.Created[TargetModel]

  object TargetCreatedEvent {
    def apply(value: TargetModel)(id: Long): TargetCreatedEvent =
      TargetCreatedEvent(id, value)
  }

  final case class TargetEditedEvent (
    id:       Long,
    oldValue: TargetModel,
    newValue: TargetModel
  ) extends Event.Edited[TargetModel]

  object TargetEditedEvent {
    def apply(oldValue: TargetModel, newValue: TargetModel)(id: Long): TargetEditedEvent =
      TargetEditedEvent(id, oldValue, newValue)
  }

}

trait TargetOptics { self: TargetModel.type =>

  val id: Lens[TargetModel, TargetModel.Id] =
    Lens[TargetModel, TargetModel.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[TargetModel, Existence] =
    Lens[TargetModel, Existence](_.existence)(a => b => b.copy(existence = a))

  val lucumaTarget: Lens[TargetModel, Target] =
    Lens[TargetModel, Target](_.target)(a => b => b.copy(target = a))

  val name: Lens[TargetModel, NonEmptyString] =
    lucumaTarget.composeLens(Target.name)

  private val gemTargetEphemerisKey: Optional[Target, EphemerisKey] =
    Target.track.composePrism(monocle.std.either.stdLeft)

  val ephemerisKey: Optional[TargetModel, EphemerisKey] =
    lucumaTarget.composeOptional(gemTargetEphemerisKey)

  private val gemTargetSiderealTracking: Optional[Target, SiderealTracking] =
    Target.track.composePrism(monocle.std.either.stdRight)

  val siderealTracking: Optional[TargetModel, SiderealTracking] =
    lucumaTarget.composeOptional(gemTargetSiderealTracking)

  val coordinates: Optional[TargetModel, Coordinates] =
    siderealTracking.composeLens(SiderealTracking.baseCoordinates)

  val ra: Optional[TargetModel, RightAscension] =
    coordinates.composeLens(Coordinates.rightAscension)

  val dec: Optional[TargetModel, Declination] =
    coordinates.composeLens(Coordinates.declination)

  val epoch: Optional[TargetModel, Epoch] =
    siderealTracking.composeLens(SiderealTracking.epoch)

  val properVelocity: Optional[TargetModel, Option[ProperVelocity]] =
    siderealTracking.composeLens(SiderealTracking.properVelocity)

  val radialVelocity: Optional[TargetModel, Option[RadialVelocity]] =
    siderealTracking.composeLens(SiderealTracking.radialVelocity)

  val parallax: Optional[TargetModel, Option[Parallax]] =
    siderealTracking.composeLens(SiderealTracking.parallax)
}