// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.json.target._
import lucuma.core.`enum`.{EphemerisKeyType, MagnitudeBand}
import lucuma.core.math.{Coordinates, Declination, Epoch, Parallax, ProperMotion, RadialVelocity, RightAscension}
import lucuma.core.model.{CatalogId, EphemerisKey, Magnitude, Observation, SiderealTracking, Target}
import lucuma.core.optics.syntax.optional._
import lucuma.core.optics.state.all._
import lucuma.odb.api.model.syntax.input._
import cats.Eq
import cats.data._
import cats.implicits._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.util.Gid
import monocle.{Lens, Optional}

import scala.collection.immutable.SortedMap

object TargetModel extends TargetOptics {

  type TargetMap = SortedMap[NonEmptyString, Target]

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

  private def formatObservationId(oid: Option[Observation.Id]): String =
    oid.map(o => s" in observation ${Gid[Observation.Id].show(o)}").getOrElse("")

  /**
   * Input required to create either a non-sidereal or sidereal target.
   */
  final case class Create(
    nonsidereal: Option[CreateNonsidereal],
    sidereal:    Option[CreateSidereal]
  ) {

    def validateObservationEdit(
      targetNames:   Set[NonEmptyString],
      listName:      String,
      observationId: Option[Observation.Id]
    ): ValidatedInput[Set[NonEmptyString]] =
      nonsidereal.map(_.validateObservationEdit(targetNames, listName, observationId))
        .orElse(sidereal.map(_.validateObservationEdit(targetNames, listName, observationId)))
        .getOrElse(targetNames.validNec[InputError])

    def targetMapEditor: ValidatedInput[TargetMap => TargetMap] =
      ValidatedInput.requireOne(
        "create",
        nonsidereal.map(_.targetMapEditor),
        sidereal.map(_.targetMapEditor)
      )

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.nonsidereal,
        a.sidereal
      )}

    def nonsidereal(n: CreateNonsidereal): Create =
      Create(n.some, None)

    def sidereal(s: CreateSidereal): Create =
      Create(None, s.some)

  }

  sealed trait TargetCreator {
    def name: NonEmptyString

    def toGemTarget: ValidatedInput[Target]

    def validateObservationEdit(
      targetNames:   Set[NonEmptyString],
      listName:      String,
      observationId: Option[Observation.Id]
    ): ValidatedInput[Set[NonEmptyString]] = {

      def alreadyExists: InputError =
        InputError.fromMessage(
          s"Cannot create a new $listName target with name '$name' because one already exists${formatObservationId(observationId)}"
        )

      if (targetNames(name)) alreadyExists.invalidNec[Set[NonEmptyString]]
      else (targetNames + name).validNec[InputError]

    }

    def targetMapEditor: ValidatedInput[TargetMap => TargetMap] =
      toGemTarget.map { t => _ + (name -> t) }

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
  ) extends TargetCreator {

    val toEphemerisKey: ValidatedInput[EphemerisKey] =
      parse.ephemerisKey("des", key, des)

    override val toGemTarget: ValidatedInput[Target] =
      (toEphemerisKey,
       magnitudes.toList.flatten.traverse(_.toMagnitude)
      ).mapN { (k, ms) =>
        Target(name, Left(k), SortedMap.from(ms.fproductLeft(_.band)))
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
  ) extends TargetCreator {

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

    override val toGemTarget: ValidatedInput[Target] =
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
    selectTarget: NonEmptyString,
    nonsidereal:  Option[EditNonsidereal],
    sidereal:     Option[EditSidereal],
  ) {

    def validateObservationEdit(
      targetNames:   Set[NonEmptyString],
      listName:      String,
      observationId: Option[Observation.Id]
    ): ValidatedInput[Set[NonEmptyString]] =

      nonsidereal.map(_.validateObservationEdit(selectTarget, targetNames, listName, observationId))
        .orElse(sidereal.map(_.validateObservationEdit(selectTarget, targetNames, listName, observationId)))
        .getOrElse(targetNames.validNec[InputError])

    val targetMapEditor: ValidatedInput[TargetMap => TargetMap] =
      ValidatedInput.requireOne(
        "edit",
        nonsidereal.map(_.targetMapEditor(selectTarget)),
        sidereal.map(_.targetMapEditor(selectTarget))
      )

  }

  object Edit {

    implicit val DecoderEdit: Decoder[Edit] =
      deriveDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.selectTarget,
        a.nonsidereal,
        a.sidereal
      )}

    def nonsidereal(selectTarget: NonEmptyString, n: EditNonsidereal): Edit =
      Edit(selectTarget, n.some, None)

    def sidereal(selectTarget: NonEmptyString, s: EditSidereal): Edit =
      Edit(selectTarget, None, s.some)

  }

  sealed trait TargetEditor {

    def name: Input[NonEmptyString]

    def editor: ValidatedInput[State[Target, Unit]]

    def validateObservationEdit(
      selectTarget:  NonEmptyString,
      targetNames:   Set[NonEmptyString],
      listName:      String,
      observationId: Option[Observation.Id]
    ): ValidatedInput[Set[NonEmptyString]] = {

      def missing: InputError =
        InputError.fromMessage(
          s"Missing $listName target '$selectTarget'${formatObservationId(observationId)}"
        )

      def wouldReplace(newName: NonEmptyString): InputError =
        InputError.fromMessage(
          s"Cannot rename '$selectTarget' to '$newName' because there is already a $listName target named '$newName'${formatObservationId(observationId)}"
        )

      val validateRename: ValidatedInput[Set[NonEmptyString]] =
        name.fold(
          targetNames.validNec[InputError],
          targetNames.validNec[InputError],
          n => if ((n =!= selectTarget) && targetNames(n)) wouldReplace(n).invalidNec[Set[NonEmptyString]]
               else ((targetNames - selectTarget) + n).validNec[InputError]
        )

      if (targetNames(selectTarget)) validateRename
      else missing.invalidNec[Set[NonEmptyString]]

    }

    def targetMapEditor(selectTarget: NonEmptyString): ValidatedInput[TargetMap => TargetMap] =

      editor.map { ed => m =>

        def normalEdit(t: Target): TargetMap =
          m.updated(selectTarget, ed.runS(t).value)

        def renameEdit(newName: NonEmptyString, t: Target): TargetMap =
          m.removed(selectTarget).updated(newName, ed.runS(t).value)

        m.get(selectTarget).fold(m) { t =>
          name.fold(normalEdit(t), normalEdit(t), renameEdit(_, t))
        }

      }

  }

  final case class EditNonsidereal(
    name: Input[NonEmptyString] = Input.ignore,
    key:  Input[EphemerisKey]   = Input.ignore,
  ) extends TargetEditor {

    override val editor: ValidatedInput[State[Target, Unit]] =
      (name.validateIsNotNull("name"),
       key.validateIsNotNull("key")
      ).mapN { case (n, k) =>
        for {
          _ <- TargetModel.name         := n
          _ <- TargetModel.ephemerisKey := k
        } yield ()
      }

  }

  object EditNonsidereal {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEditNonSidereal: Decoder[EditNonsidereal] =
      deriveConfiguredDecoder[EditNonsidereal]

    implicit val EqEditNonsidereal: Eq[EditNonsidereal] =
      Eq.by(en => (
        en.name,
        en.key
      ))

  }

  final case class EditSidereal(
    name:             Input[NonEmptyString]            = Input.ignore,
    catalogId:        Input[CatalogIdModel.Input]      = Input.ignore,
    ra:               Input[RightAscensionModel.Input] = Input.ignore,
    dec:              Input[DeclinationModel.Input]    = Input.ignore,
    epoch:            Input[Epoch]                     = Input.ignore,
    properMotion:     Input[ProperMotionModel.Input]   = Input.ignore,
    radialVelocity:   Input[RadialVelocityModel.Input] = Input.ignore,
    parallax:         Input[ParallaxModel.Input]       = Input.ignore,
    magnitudes:       Option[MagnitudeModel.EditList],
  ) extends TargetEditor {

    override val editor: ValidatedInput[State[Target, Unit]] =
      (name          .validateIsNotNull("name"),
       catalogId     .validateNullable(_.toCatalogId),
       ra            .validateNotNullable("ra")(_.toRightAscension),
       dec           .validateNotNullable("dec")(_.toDeclination),
       epoch         .validateIsNotNull("epoch"),
       properMotion  .validateNullable(_.toProperMotion),
       radialVelocity.validateNullable(_.toRadialVelocity),
       parallax      .validateNullable(_.toParallax),
       magnitudes    .traverse(_.editor)
      ).mapN { (name, catalogId, ra, dec, epoch, pm, rv, px, ms) =>
        for {
          _ <- TargetModel.name           := name
          _ <- TargetModel.catalogId      := catalogId
          _ <- TargetModel.ra             := ra
          _ <- TargetModel.dec            := dec
          _ <- TargetModel.epoch          := epoch
          _ <- TargetModel.properMotion   := pm
          _ <- TargetModel.radialVelocity := rv
          _ <- TargetModel.parallax       := px
          _ <- TargetModel.magnitudes.mod(m => ms.fold(m)(_.runS(m).value))
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
    add:    Option[Create],
    delete: Option[NonEmptyString],
    edit:   Option[Edit],
  ) {

    def targetMapEditor(fieldName: String): ValidatedInput[TargetMap => TargetMap] =
      ValidatedInput.requireOne(
        fieldName,
        add.map(_.targetMapEditor),
        delete.map { name => ((m: TargetMap) => m.removed(name)).validNec[InputError] },
        edit.map(_.targetMapEditor)
      )

    def validateObservationEdit(
      targetNames:   Set[NonEmptyString],
      listName:      String,
      observationId: Option[Observation.Id]
    ): ValidatedInput[Set[NonEmptyString]] = {

      def validateDelete(n: NonEmptyString): ValidatedInput[Set[NonEmptyString]] =
        if (targetNames(n))
          (targetNames - n).validNec[InputError]
        else
          InputError.fromMessage(
            s"Could not delete $listName target '$n'${formatObservationId(observationId)} because it was not found"
          ).invalidNec[Set[NonEmptyString]]

      add.map(_.validateObservationEdit(targetNames, listName, observationId))
        .orElse(delete.map(validateDelete))
        .orElse(edit.map(_.validateObservationEdit(targetNames, listName, observationId)))
        .getOrElse(targetNames.validNec[InputError])

    }

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

    def add(c: Create): EditTargetAction =
      EditTargetAction(c.some, None, None)

    def delete(n: NonEmptyString): EditTargetAction =
      EditTargetAction(None, n.some, None)

    def edit(e: Edit): EditTargetAction =
      EditTargetAction(None, None, e.some)

  }

  final case class EditTargetList(
    replaceList: Option[List[TargetModel.Create]],
    editList:    Option[List[EditTargetAction]]
  ) {

    def validateObservationEdit(
      targetNames:   Set[NonEmptyString],
      listName:      String,
      observationId: Option[Observation.Id]
    ): ValidatedInput[Set[NonEmptyString]] =
      replaceList.map { cs =>
        cs.foldLeft(Set.empty[NonEmptyString].validNec[InputError]) { (vs, c) =>
          vs.andThen(s => c.validateObservationEdit(s, listName, observationId))
        }
      }.orElse(editList.map { es =>
        es.foldLeft(targetNames.validNec[InputError]) { (vs, e) =>
          vs.andThen(s => e.validateObservationEdit(s, listName, observationId))
        }
      }).getOrElse(targetNames.validNec[InputError])

    def targetMapEditor(
      listName:      String,
    ): ValidatedInput[TargetMap => TargetMap] =
      ValidatedInput.requireOne(
        listName,
        replaceList.map { cs =>
          cs.traverse(_.targetMapEditor)
            .map { lst => (_: TargetMap) => lst.foldLeft(SortedMap.empty[NonEmptyString, Target])((m, f) => f(m)) }
        },
        editList.map { es =>
          es.traverse(_.targetMapEditor("editList"))
            .map { lst => (m: TargetMap) => lst.foldLeft(m)((m, f) => f(m)) }
        }
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

    def replace(cs: List[TargetModel.Create]): EditTargetList =
      EditTargetList(cs.some, None)

    def edit(es: List[EditTargetAction]): EditTargetList =
      EditTargetList(None, es.some)

  }

}

trait TargetOptics { self: TargetModel.type =>

  val name: Lens[Target, NonEmptyString] =
    Target.name

  val nonsiderealTarget: Optional[Target, Target] =
    Optional.filter[Target](_.track.isLeft)

  val siderealTarget: Optional[Target, Target] =
    Optional.filter[Target](_.track.isRight)

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
