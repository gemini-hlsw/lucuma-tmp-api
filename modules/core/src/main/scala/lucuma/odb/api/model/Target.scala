// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.syntax.all._

import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.math.{
  Declination,
  Coordinates,
  Epoch,
  ProperMotion,
  ProperVelocity,
  RadialVelocity,
  RightAscension
}
import lucuma.core.model.EphemerisKey
import lucuma.core.util.Gid

import cats.data.State
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import monocle.{Lens, Optional}


/**
 * A Target combining an ID with a `gem.Target`.
 */
final case class Target(
  id:         Target.Id,
  existence:  Existence,
  target:     lucuma.core.model.Target
)

object Target extends TargetOptics {

  final case class Id(value: PosLong) {
    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {
    implicit val GidTargetId: Gid[Id] =
      Gid.instance('t', _.value, apply)
  }

  implicit val TopLevelTarget: TopLevel[Id, Target] =
    TopLevel.instance(_.id, existence)


  object parse {

    def ephemerisKey(
      fieldName: String,
      key: EphemerisKeyType,
      input: String
    ): ValidatedInput[EphemerisKey] =
      EphemerisKey
        .fromTypeAndDes
        .getOption((key, input))
        .toValidNec(
          InputError.InvalidField(fieldName, input, s"Invalid description for ephemeris key type `${key.shortName}`")
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
    pids: List[Program.Id],
    name: String,
    key:  EphemerisKeyType,
    des:  String
  ) {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    val toGemTarget: ValidatedInput[lucuma.core.model.Target] =
      toEphemerisKey.map { k => lucuma.core.model.Target(name, Left(k)) }
  }

  /**
   * Describes input used to create a sidereal target.
   *
   * @param pids associated program(s), if any (which either exist or results
   *             in an error)
   * @param name target name
   * @param ra right ascension at epoch
   * @param dec declination at epoch
   * @param epoch time of the base observation
   * @param properVelocity proper velocity per year in right ascension and declination
   * @param radialVelocity radial velocity
   */
  final case class CreateSidereal(
    pids:  List[Program.Id],
    name:  String,
    ra:    RightAscension,
    dec:   Declination,
    epoch: Option[Epoch],
    properVelocity: Option[ProperVelocity],
    radialVelocity: Option[RadialVelocity]
    // TODO: more proper motion
  ) {

    val toProperMotion: ProperMotion =
      ProperMotion(
        Coordinates(ra, dec),
        epoch.getOrElse(Epoch.J2000),
        properVelocity,
        radialVelocity,
        None
      )

    val toGemTarget: lucuma.core.model.Target =
      lucuma.core.model.Target(name, Right(toProperMotion))

  }

  final case class EditNonsidereal(
    id:        Target.Id,
    existence: Option[Existence],
    name:      Option[String],
    key:       Option[EphemerisKey],
  ) extends Editor[Id, Target] {

    override val editor: State[Target, Unit] =
      for {
        _ <- Target.existence    := existence
        _ <- Target.name         := name
        _ <- Target.ephemerisKey := key
      } yield ()

  }

  final case class EditSidereal(
    id:             Target.Id,
    existence:      Option[Existence],
    name:           Option[String],
    ra:             Option[RightAscension],
    dec:            Option[Declination],
    epoch:          Option[Epoch],
    properVelocity: Option[Option[ProperVelocity]],
    radialVelocity: Option[Option[RadialVelocity]]
  ) extends Editor[Id, Target] {

    override val editor: State[Target, Unit] =
      for {
        _ <- Target.existence      := existence
        _ <- Target.name           := name
        _ <- Target.ra             := ra
        _ <- Target.dec            := dec
        _ <- Target.epoch          := epoch
        _ <- Target.properVelocity := properVelocity
        _ <- Target.radialVelocity := radialVelocity
      } yield ()

  }

  final case class TargetCreatedEvent (
    id: Long,
    value: Target,
  ) extends Event.Created[Target]

  object TargetCreatedEvent {
    def apply(value: Target)(id: Long): TargetCreatedEvent =
      TargetCreatedEvent(id, value)
  }

  final case class TargetEditedEvent (
    id: Long,
    oldValue: Target,
    newValue: Target
  ) extends Event.Edited[Target]

  object TargetEditedEvent {
    def apply(oldValue: Target, newValue: Target)(id: Long): TargetEditedEvent =
      TargetEditedEvent(id, oldValue, newValue)
  }

}

trait TargetOptics { self: Target.type =>

  val id: Lens[Target, Target.Id] =
    Lens[Target, Target.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[Target, Existence] =
    Lens[Target, Existence](_.existence)(a => b => b.copy(existence = a))

  val lucumaTarget: Lens[Target, lucuma.core.model.Target] =
    Lens[Target, lucuma.core.model.Target](_.target)(a => b => b.copy(target = a))

  val name: Lens[Target, String] =
    lucumaTarget.composeLens(lucuma.core.model.Target.name)

  private val gemTargetEphemerisKey: Optional[lucuma.core.model.Target, EphemerisKey] =
    lucuma.core.model.Target.track.composePrism(monocle.std.either.stdLeft)

  val ephemerisKey: Optional[Target, lucuma.core.model.EphemerisKey] =
    lucumaTarget.composeOptional(gemTargetEphemerisKey)

  private val gemTargetProperMotion: Optional[lucuma.core.model.Target, ProperMotion] =
    lucuma.core.model.Target.track.composePrism(monocle.std.either.stdRight)

  val properMotion: Optional[Target, ProperMotion] =
    lucumaTarget.composeOptional(gemTargetProperMotion)

  val coordinates: Optional[Target, Coordinates] =
    properMotion.composeLens(ProperMotion.baseCoordinates)

  val ra: Optional[Target, RightAscension] =
    coordinates.composeLens(Coordinates.rightAscension)

  val dec: Optional[Target, Declination] =
    coordinates.composeLens(Coordinates.declination)

  val epoch: Optional[Target, Epoch] =
    properMotion.composeLens(ProperMotion.epoch)

  val properVelocity: Optional[Target, Option[ProperVelocity]] =
    properMotion.composeLens(ProperMotion.properVelocity)

  val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    properMotion.composeLens(ProperMotion.radialVelocity)

}