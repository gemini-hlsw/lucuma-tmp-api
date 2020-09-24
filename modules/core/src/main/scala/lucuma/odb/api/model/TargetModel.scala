// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.syntax.all._
import lucuma.odb.api.model.json.targetmath._
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.math.{Coordinates, Declination, Epoch, ProperMotion, ProperVelocity, RadialVelocity, RightAscension}
import lucuma.core.model.{EphemerisKey, Target}
import lucuma.core.util.Gid

import cats.data._
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.{Lens, Optional}

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

  /**
   * Describes input used to create a nonsidereal target.
   *
   * @param pids associated program(s), if any (which either exist or results
   *             in an error)
   * @param name target name
   * @param key ephemeris key type
   * @param des semi-permanent horizons identifier (relative to key type)
   */
  final case class CreateNonsidereal(
    pids: List[ProgramModel.Id],
    name: String,
    key:  EphemerisKeyType,
    des:  String
  ) {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    val toGemTarget: ValidatedInput[Target] =
      toEphemerisKey.map { k => Target(name, Left(k)) }
  }

  object CreateNonsidereal {

    implicit val DecoderCreateNonsidereal: Decoder[CreateNonsidereal] =
      deriveDecoder[CreateNonsidereal]

  }

  /**
   * Describes input used to create a sidereal target.
   *
   * @param pids associated program(s), if any (which either exist or results
   *             in an error)
   * @param name target name
   * @param ra right ascension coordinate at epoch
   * @param dec declination coordinate at epoch
   * @param epoch time of the base observation
   * @param properVelocity proper velocity per year in right ascension and declination
   * @param radialVelocity radial velocity
   */
  final case class CreateSidereal(
    pids:           List[ProgramModel.Id],
    name:           String,
    ra:             RightAscensionModel.Input,
    dec:            DeclinationModel.Input,
    epoch:          Option[Epoch],
    properVelocity: Option[ProperVelocityModel.Input],
    radialVelocity: Option[RadialVelocityModel.Input]
    // TODO: more proper motion
  ) {

    val toProperMotion: ValidatedInput[ProperMotion] =
      (ra.toRightAscension,
       dec.toDeclination,
       properVelocity.traverse(_.toProperVelocity),
       radialVelocity.traverse(_.toRadialVelocity)
      ).mapN { (ra, dec, pv, rv) =>
        ProperMotion(
          Coordinates(ra, dec),
          epoch.getOrElse(Epoch.J2000),
          pv,
          rv,
          None
        )
      }

    val toGemTarget: ValidatedInput[Target] =
      toProperMotion.map { pm => Target(name, Right(pm)) }

  }

  object CreateSidereal {

    implicit val DecoderCreateSidereal: Decoder[CreateSidereal] =
      deriveDecoder[CreateSidereal]

  }

  final case class EditNonsidereal(
    id:        TargetModel.Id,
    existence: Option[Existence],
    name:      Option[String],
    key:       Option[EphemerisKey],
  ) extends Editor[Id, TargetModel] {

    override val editor: ValidatedInput[State[TargetModel, Unit]] =
      (for {
        _ <- TargetModel.existence    := existence
        _ <- TargetModel.name         := name
        _ <- TargetModel.ephemerisKey := key
      } yield ()).validNec

  }

  final case class EditSidereal(
    id:             TargetModel.Id,
    existence:      Option[Existence],
    name:           Option[String],
    ra:             Option[RightAscensionModel.Input],
    dec:            Option[DeclinationModel.Input],
    epoch:          Option[Epoch],
    properVelocity: Option[Option[ProperVelocityModel.Input]],
    radialVelocity: Option[Option[RadialVelocityModel.Input]]
  ) extends Editor[Id, TargetModel] {

    override val editor: ValidatedInput[State[TargetModel, Unit]] =
      (ra.traverse(_.toRightAscension),
       dec.traverse(_.toDeclination),
       Nested(properVelocity).traverse(_.toProperVelocity).map(_.value),
       Nested(radialVelocity).traverse(_.toRadialVelocity).map(_.value)
      ).mapN { (ra, dec, pv, rv) =>
        for {
          _ <- TargetModel.existence      := existence
          _ <- TargetModel.name           := name
          _ <- TargetModel.ra             := ra
          _ <- TargetModel.dec            := dec
          _ <- TargetModel.epoch          := epoch
          _ <- TargetModel.properVelocity := pv
          _ <- TargetModel.radialVelocity := rv
        } yield ()
      }

  }

  object EditSidereal {

    implicit val DecoderEditSidereal: Decoder[EditSidereal] =
      deriveDecoder[EditSidereal]

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

  val name: Lens[TargetModel, String] =
    lucumaTarget.composeLens(Target.name)

  private val gemTargetEphemerisKey: Optional[Target, EphemerisKey] =
    Target.track.composePrism(monocle.std.either.stdLeft)

  val ephemerisKey: Optional[TargetModel, EphemerisKey] =
    lucumaTarget.composeOptional(gemTargetEphemerisKey)

  private val gemTargetProperMotion: Optional[Target, ProperMotion] =
    Target.track.composePrism(monocle.std.either.stdRight)

  val properMotion: Optional[TargetModel, ProperMotion] =
    lucumaTarget.composeOptional(gemTargetProperMotion)

  val coordinates: Optional[TargetModel, Coordinates] =
    properMotion.composeLens(ProperMotion.baseCoordinates)

  val ra: Optional[TargetModel, RightAscension] =
    coordinates.composeLens(Coordinates.rightAscension)

  val dec: Optional[TargetModel, Declination] =
    coordinates.composeLens(Coordinates.declination)

  val epoch: Optional[TargetModel, Epoch] =
    properMotion.composeLens(ProperMotion.epoch)

  val properVelocity: Optional[TargetModel, Option[ProperVelocity]] =
    properMotion.composeLens(ProperMotion.properVelocity)

  val radialVelocity: Optional[TargetModel, Option[RadialVelocity]] =
    properMotion.composeLens(ProperMotion.radialVelocity)

}