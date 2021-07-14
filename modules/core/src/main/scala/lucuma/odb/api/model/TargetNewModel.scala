// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.json.target._
import lucuma.core.`enum`.{EphemerisKeyType, MagnitudeBand}
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperMotion, RadialVelocity, RightAscension}
import lucuma.core.model.{CatalogId, EphemerisKey, Magnitude, SiderealTracking, Target}
import lucuma.core.optics.syntax.optional._
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.syntax.input._
import cats.Eq
import cats.data._
import cats.implicits._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import monocle.{Lens, Optional}

import scala.collection.immutable.SortedMap

object TargetNewModel extends TargetNewOptics {

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
   * Input required to create either a non-sidereal or sidereal target.
   */
  final case class Create(
    nonSidereal: Option[CreateNonsidereal],
    sidereal:    Option[CreateSidereal]
  ) {

    val toGemTarget: ValidatedInput[Target] =
      ValidatedInput.requireOne(
        "create",
        nonSidereal.map(_.toGemTarget),
        sidereal.map(_.toGemTarget)
      )

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.nonSidereal,
        a.sidereal
      )}

  }

  /**
   * Describes input used to create a nonsidereal target.
   *
   * @param name target name
   * @param key ephemeris key type
   * @param des semi-permanent horizons identifier (relative to key type)
   */
  final case class CreateNonsidereal(
    name:       NonEmptyString,
    key:        EphemerisKeyType,
    des:        String,
    magnitudes: Option[List[MagnitudeModel.Create]]
  ) {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    val toGemTarget: ValidatedInput[Target] = {
      (toEphemerisKey,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (k, ms) =>
        Target(name, Left(k), SortedMap.from(ms.fproductLeft(_.band)))
      }
    }

  }

  object CreateNonsidereal {

    implicit val DecoderCreateNonsidereal: Decoder[CreateNonsidereal] =
      deriveDecoder[CreateNonsidereal]

    implicit val EqCreateNonsidereal: Eq[CreateNonsidereal] =
      Eq.by(cn => (
        cn.name,
        cn.key,
        cn.des,
        cn.magnitudes
      ))

  }

  /**
   * Describes input used to create a sidereal target.
   *
   * @param name target name
   * @param ra right ascension coordinate at epoch
   * @param dec declination coordinate at epoch
   * @param epoch time of the base observation
   * @param properMotion proper motion per year in right ascension and declination
   * @param radialVelocity radial velocity
   * @param parallax parallax
   */
  final case class CreateSidereal(
    name:           NonEmptyString,
    catalogId:      Option[CatalogIdModel.Input],
    ra:             RightAscensionModel.Input,
    dec:            DeclinationModel.Input,
    epoch:          Option[Epoch],
    properMotion:   Option[ProperMotionModel.Input],
    radialVelocity: Option[RadialVelocityModel.Input],
    parallax:       Option[ParallaxModel.Input],
    magnitudes:     Option[List[MagnitudeModel.Create]]
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
        Target(name, Right(pm), SortedMap.from(ms.fproductLeft(_.band)))
      }

  }

  object CreateSidereal {

    def fromRaDec(
      name: NonEmptyString,
      ra:   RightAscensionModel.Input,
      dec:  DeclinationModel.Input
    ): CreateSidereal =
      CreateSidereal(
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

  /**
   * Input required to edit either a non-sidereal or sidereal target.
   */
  final case class Edit(
    nonSidereal: Option[EditNonsidereal],
    sidereal:    Option[EditSidereal]
  ) {

    def name: Option[NonEmptyString] =
      nonSidereal.map(_.name) orElse sidereal.map(_.name)

    val editor: ValidatedInput[State[Target, Unit]] =
      ValidatedInput.requireOne(
        "create",
        nonSidereal.map(_.editor),
        sidereal.map(_.editor)
      )

  }

  object Edit {

    implicit val DecoderEdit: Decoder[Edit] =
      deriveDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.nonSidereal,
        a.sidereal
      )}

  }


  final case class EditNonsidereal(
    name:      NonEmptyString,
//    newName:   Option[NonEmptyString],
    key:       Option[EphemerisKey],
  ) {

    val editor: ValidatedInput[State[Target, Unit]] =
      (for {
//        _ <- TargetNewModel.name         := newName
        _ <- TargetNewModel.ephemerisKey := key
      } yield ()).validNec

  }

  object EditNonsidereal {

    implicit val DecoderEditNonSidereal: Decoder[EditNonsidereal] =
      deriveDecoder[EditNonsidereal]

    implicit val EqEditNonsidereal: Eq[EditNonsidereal] =
      Eq.by(en => (
        en.name,
        en.key
      ))

  }

  final case class EditSidereal(
    name:             NonEmptyString,
    catalogId:        Input[CatalogIdModel.Input]      = Input.ignore,
    ra:               Input[RightAscensionModel.Input] = Input.ignore,
    dec:              Input[DeclinationModel.Input]    = Input.ignore,
    epoch:            Input[Epoch]                     = Input.ignore,
    properMotion:     Input[ProperMotionModel.Input]   = Input.ignore,
    radialVelocity:   Input[RadialVelocityModel.Input] = Input.ignore,
    parallax:         Input[ParallaxModel.Input]       = Input.ignore,
    magnitudes:       Option[MagnitudeModel.EditList],
  ) {

    val editor: ValidatedInput[State[Target, Unit]] =
      (catalogId     .validateNullable(_.toCatalogId),
       ra            .validateNotNullable("ra")(_.toRightAscension),
       dec           .validateNotNullable("dec")(_.toDeclination),
       epoch         .validateIsNotNull("epoch"),
       properMotion  .validateNullable(_.toProperMotion),
       radialVelocity.validateNullable(_.toRadialVelocity),
       parallax      .validateNullable(_.toParallax),
       magnitudes    .traverse(_.editor)
      ).mapN { (catalogId, ra, dec, epoch, pm, rv, px, ms) =>
        for {
          _ <- TargetNewModel.catalogId      := catalogId
          _ <- TargetNewModel.ra             := ra
          _ <- TargetNewModel.dec            := dec
          _ <- TargetNewModel.epoch          := epoch
          _ <- TargetNewModel.properMotion   := pm
          _ <- TargetNewModel.radialVelocity := rv
          _ <- TargetNewModel.parallax       := px
          _ <- TargetNewModel.magnitudes.mod(m => ms.fold(m)(_.runS(m).value))
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
        es.name,
        es.catalogId,
        es.ra,
        es.dec,
        es.epoch,
        es.properMotion,
        es.radialVelocity,
        es.parallax,
        es.magnitudes
      ))

  }


  final case class EditTargetAction(
    add:    Option[TargetNewModel.Create],
    delete: Option[NonEmptyString],
    edit:   Option[TargetNewModel.Edit]
  ) {

    val editor: ValidatedInput[State[SortedMap[NonEmptyString, Target], Unit]] =
      ValidatedInput.requireOne(
        "edit",
        add.map(_.toGemTarget).map(_.map { t =>
          State.modify[SortedMap[NonEmptyString, Target]](_.updated(t.name, t))
        }),
        delete.map(n => State.modify[SortedMap[NonEmptyString, Target]](_.removed(n)).validNec),
        edit.map { e =>
          e.editor.map { s =>
            State.modify[SortedMap[NonEmptyString, Target]] { m =>
              e.name.flatMap(m.get).fold(m) { t =>
                m.updated(t.name, s.runS(t).value)
              }
            }
          }
        }
      )

  }

  object EditTargetAction {

    implicit val DecoderEditTargetAction: Decoder[EditTargetAction] =
      deriveDecoder[EditTargetAction]

    implicit val EqEditTargetAction: Eq[EditTargetAction] =
      Eq.by { a => (
        a.add,
        a.delete,
        a.edit
      )}

  }

  final case class EditTargetList(
    replaceList: Option[List[TargetNewModel.Create]],
    editList:    Option[List[EditTargetAction]]
  ) {

    /**
     * @param listName for example "science", "blindOffset"
     * @return validated editor
     */
    def editor(listName: String): ValidatedInput[State[SortedMap[NonEmptyString, Target], Unit]] =
      ValidatedInput.requireOne(
        listName,
        replaceList.map(_.traverse(_.toGemTarget).map { ts =>
          State.set[SortedMap[NonEmptyString, Target]](
            SortedMap.from(ts.fproductLeft(_.name))
          )
        }),
        editList.map(_.traverse(_.editor).map(_.sequence.void))
      )

  }

  object EditTargetList {

    implicit val DecoderEditTargetList: Decoder[EditTargetList] =
      deriveDecoder[EditTargetList]

    implicit val EqEditTargetList: Eq[EditTargetList] =
      Eq.by { a => (
        a.replaceList,
        a.editList
      )}
  }



}

trait TargetNewOptics { self: TargetNewModel.type =>

  val name: Lens[Target, NonEmptyString] =
    Target.name

  val ephemerisKey: Optional[Target, EphemerisKey] =
    Target.track.andThen(monocle.std.either.stdLeft[EphemerisKey, SiderealTracking])

  val siderealTracking: Optional[Target, SiderealTracking] =
    Target.track.andThen(monocle.std.either.stdRight[EphemerisKey, SiderealTracking])

  val catalogId: Optional[Target, Option[CatalogId]] =
    siderealTracking.andThen(SiderealTracking.catalogId)

  val coordinates: Optional[Target, Coordinates] =
    siderealTracking.andThen(SiderealTracking.baseCoordinates)

  val ra: Optional[Target, RightAscension] =
    coordinates.andThen(Coordinates.rightAscension)

  val dec: Optional[Target, Declination] =
    coordinates.andThen(Coordinates.declination)

  val epoch: Optional[Target, Epoch] =
    siderealTracking.andThen(SiderealTracking.epoch)

  val properMotion: Optional[Target, Option[ProperMotion]] =
    siderealTracking.andThen(SiderealTracking.properMotion)

  val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    siderealTracking.andThen(SiderealTracking.radialVelocity)

  val parallax: Optional[Target, Option[Parallax]] =
    siderealTracking.andThen(SiderealTracking.parallax)

  val magnitudes: Lens[Target, SortedMap[MagnitudeBand, Magnitude]] =
  Target.magnitudes

}
