// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.json.targetmath._
import lucuma.core.`enum`.EphemerisKeyType
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperVelocity, RadialVelocity, RightAscension}
import lucuma.core.model.{CatalogId, EphemerisKey, Program, SiderealTracking, Target}
import lucuma.core.optics.syntax.lens._
import lucuma.core.optics.syntax.optional._
import cats.data._
import cats.implicits._
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
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
    name:       String,
    key:        EphemerisKeyType,
    des:        String,
    magnitudes: Option[List[MagnitudeModel.Input]]
  ) {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    val toGemTarget: ValidatedInput[Target] = {
      (targetName(name),
       toEphemerisKey,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (n, k, ms) =>
        Target(n, Left(k), SortedMap.from(ms.map(m => m.band -> m)))
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
   * @param targetId optional client-provided target id (if not specified one
   *                 will be created)
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
    targetId:       Option[Target.Id],
    programIds:     Option[List[Program.Id]],
    name:           String,
    catalogId:      Option[CatalogIdModel.Input],
    ra:             RightAscensionModel.Input,
    dec:            DeclinationModel.Input,
    epoch:          Option[Epoch],
    properVelocity: Option[ProperVelocityModel.Input],
    radialVelocity: Option[RadialVelocityModel.Input],
    parallax:       Option[ParallaxModel.Input],
    magnitudes:     Option[List[MagnitudeModel.Input]]
  ) {

    val toSiderealTracking: ValidatedInput[SiderealTracking] =
      (catalogId.traverse(_.toCatalogId),
       ra.toRightAscension,
       dec.toDeclination,
       properVelocity.traverse(_.toProperVelocity),
       radialVelocity.traverse(_.toRadialVelocity),
       parallax.traverse(_.toParallax)
      ).mapN { (catalogId, ra, dec, pv, rv, px) =>
        SiderealTracking(
          catalogId,
          Coordinates(ra, dec),
          epoch.getOrElse(Epoch.J2000),
          pv,
          rv,
          px
        )
      }

    val toGemTarget: ValidatedInput[Target] =
      (targetName(name),
       toSiderealTracking,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (n, pm, ms) =>
        Target(n, Right(pm), SortedMap.from(ms.map(m => m.band -> m)))
      }

  }

  object CreateSidereal {

    implicit val DecoderCreateSidereal: Decoder[CreateSidereal] =
      deriveDecoder[CreateSidereal]

  }

  final case class EditNonsidereal(
    targetId:  Target.Id,
    existence: Option[Existence],
    name:      Option[String],
    key:       Option[EphemerisKey],
  ) extends Editor[Target.Id, TargetModel] {

    override def id: Target.Id =
      targetId

    override val editor: ValidatedInput[State[TargetModel, Unit]] =
      (for {
        _ <- TargetModel.existence    := existence
        _ <- TargetModel.name         := name.flatMap(n => NonEmptyString.from(n).toOption)
        _ <- TargetModel.ephemerisKey := key
      } yield ()).validNec

  }

  final case class EditSidereal(
    targetId:       Target.Id,
    existence:      Option[Existence],
    name:           Option[String],
    catalogId:      Option[Option[CatalogIdModel.Input]],
    ra:             Option[RightAscensionModel.Input],
    dec:            Option[DeclinationModel.Input],
    epoch:          Option[Epoch],
    properVelocity: Option[Option[ProperVelocityModel.Input]],
    radialVelocity: Option[Option[RadialVelocityModel.Input]],
    parallax:       Option[Option[ParallaxModel.Input]]
  ) extends Editor[Target.Id, TargetModel] {

    override def id: Target.Id =
      targetId

    override val editor: ValidatedInput[State[TargetModel, Unit]] =
      (Nested(catalogId).traverse(_.toCatalogId).map(_.value),
       ra.traverse(_.toRightAscension),
       dec.traverse(_.toDeclination),
       Nested(properVelocity).traverse(_.toProperVelocity).map(_.value),
       Nested(radialVelocity).traverse(_.toRadialVelocity).map(_.value),
       Nested(parallax).traverse(_.toParallax).map(_.value)
      ).mapN { (catalogId, ra, dec, pv, rv, px) =>
        for {
          _ <- TargetModel.existence      := existence
          _ <- TargetModel.name           := name.flatMap(n => NonEmptyString.from(n).toOption)
          _ <- TargetModel.catalogId      := catalogId
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
    targetId:   Target.Id,
    programIds: List[Program.Id]
  )

  object TargetProgramLinks {

    implicit val DecoderTargetProgramLinks: Decoder[TargetProgramLinks] =
      deriveDecoder[TargetProgramLinks]

  }

  final case class TargetEvent (
    id:       Long,
    editType: Event.EditType,
    value:    TargetModel,
  ) extends Event.Edit[TargetModel]

  object TargetEvent {
    def apply(editType: Event.EditType, value: TargetModel)(id: Long): TargetEvent =
      TargetEvent(id, editType, value)
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
    lucumaTarget.composeLens(Target.name)

  private val gemTargetEphemerisKey: Optional[Target, EphemerisKey] =
    Target.track.composePrism(monocle.std.either.stdLeft)

  val ephemerisKey: Optional[TargetModel, EphemerisKey] =
    lucumaTarget.composeOptional(gemTargetEphemerisKey)

  private val gemTargetSiderealTracking: Optional[Target, SiderealTracking] =
    Target.track.composePrism(monocle.std.either.stdRight)

  val siderealTracking: Optional[TargetModel, SiderealTracking] =
    lucumaTarget.composeOptional(gemTargetSiderealTracking)

  val catalogId: Optional[TargetModel, Option[CatalogId]] =
    siderealTracking.composeLens(SiderealTracking.catalogId)

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