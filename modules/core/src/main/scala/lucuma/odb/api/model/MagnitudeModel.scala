// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.{MagnitudeBand, MagnitudeSystem}
import lucuma.core.math.MagnitudeValue
import lucuma.core.model.Magnitude
import lucuma.core.optics.state.all._
import lucuma.core.optics.syntax.lens._
import lucuma.odb.api.model.syntax.input._
import cats.Eq
import cats.data.State
import cats.syntax.all._
import clue.data.Input
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Lens
import monocle.function.At

import scala.collection.immutable.SortedMap

object MagnitudeModel {

  private def toMagnitudeValue(field: String, d: BigDecimal): ValidatedInput[MagnitudeValue] =
    MagnitudeValue
      .fromBigDecimal
      .getOption(d)
      .toValidNec(InputError.fromMessage(s"Could not read '$field' field value $d as a magnitude value"))

  final case class Create(
    band:   MagnitudeBand,
    value:  BigDecimal,
    system: Option[MagnitudeSystem],
    error:  Option[BigDecimal]
  ) {

    val toMagnitude: ValidatedInput[Magnitude] =
      (toMagnitudeValue("value", value),
       error.traverse(e => toMagnitudeValue("error", e))
      ).mapN { (v, e) =>
        Magnitude(v, band, e, system.getOrElse(MagnitudeSystem.Vega))
      }

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { in => (
        in.band,
        in.value,
        in.system,
        in.error
      )}

  }

  final case class Edit(
    band:   MagnitudeBand,
    value:  Input[BigDecimal]      = Input.ignore,
    system: Input[MagnitudeSystem] = Input.ignore,
    error:  Input[BigDecimal]      = Input.ignore
  ) {

    def editor: ValidatedInput[State[Magnitude, Unit]] =
      (
        value.validateNotNullable("value")(toMagnitudeValue("value", _)),
        system.validateIsNotNull("system"),
        error.validateNotNullable("error")(toMagnitudeValue("error", _))
      ).mapN { (v, s, e) =>
        for {
          _ <- Magnitude.value  := v
          _ <- Magnitude.system := s
          _ <- Magnitude.error  := e
        } yield ()
      }

  }

  object Edit {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] =
      deriveConfiguredDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by { a => (
        a.band,
        a.value,
        a.system,
        a.error
      )}

  }

  type MagnitudeMap = SortedMap[MagnitudeBand, Magnitude]

  final case class EditAction(
    add:    Option[Create],
    delete: Option[MagnitudeBand],
    edit:   Option[Edit]
  ) {

    import EditAction.magAt

    def editor: ValidatedInput[State[MagnitudeMap, Option[Magnitude]]] =
      (add, delete, edit) match {

        // add
        case (Some(a), None, None) =>
          a.toMagnitude.map { mag =>
            magAt(mag.band).mod(_ => mag.some)
          }

        // delete
        case (None, Some(d), None) =>
          magAt(d).modo(_ => None).validNec

        // edit
        case (None, None, Some(e)) =>
          e.editor.map { ed =>
            magAt(e.band).mod(_.map(mag => ed.runS(mag).value))
          }

        // no action
        case (None, None, None)    =>
          InputError.missingInput("edit action").invalidNec

        // multiple actions
        case _                     =>
          InputError.fromMessage(s"Multiple edit actions are not permitted in a single list element").invalidNec
      }


  }

  object EditAction {

    def magAt(band: MagnitudeBand): Lens[MagnitudeMap, Option[Magnitude]] =
      At.at[MagnitudeMap, MagnitudeBand, Option[Magnitude]](band)

    implicit val DecoderEditActionInput: Decoder[EditAction] =
      deriveDecoder[EditAction]

    implicit val EqEditActionInput: Eq[EditAction] =
      Eq.by { a => (
        a.add,
        a.delete,
        a.edit
      )}

  }

  final case class EditList(
    replaceList: Option[List[Create]],
    editList:    Option[List[EditAction]]
  ) {

    val editor: ValidatedInput[State[MagnitudeMap, Unit]] = {

      val ed: ValidatedInput[Either[List[Magnitude], State[MagnitudeMap, Unit]]] =
        ValidatedInput.optionEither(
          "replaceList",
          "editList",
          replaceList.traverse(_.traverse(_.toMagnitude)),
          editList.traverse(_.traverse(_.editor).map(_.sequence.void))
        ).map(_.getOrElse(State.get[MagnitudeMap].void.asRight))

      ed.map(_.leftMap(lst => State.set(SortedMap.from(lst.fproductLeft(_.band)))).merge)
    }

  }

  object EditList {

    implicit val DecoderListInput: Decoder[EditList] =
      deriveDecoder[EditList]

    implicit val EqListInput: Eq[EditList] =
      Eq.by { a => (
        a.replaceList,
        a.editList
      )}

  }


}
