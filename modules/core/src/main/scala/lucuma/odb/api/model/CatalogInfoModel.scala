// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.{EitherNec, StateT}
import cats.syntax.apply._
import cats.syntax.option._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.NonEmptyString
import io.circe.Decoder
import io.circe.refined._
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.`enum`.CatalogName
import lucuma.core.model.CatalogInfo
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._

object CatalogInfoModel {

  final case class EditInput(
    name:       Input[CatalogName]    = Input.ignore,
    id:         Input[NonEmptyString] = Input.ignore,
    objectType: Input[NonEmptyString] = Input.ignore
  ) {

    val create: ValidatedInput[CatalogInfo] =
      (name.toOption.toValidNec(InputError.missingInput("catalog 'name'")),
       id.toOption.toValidNec(InputError.missingInput("catalog 'id'"))
      ).mapN { (n, i) => CatalogInfo(n, i, objectType.toOption) }

    val edit: StateT[EitherNec[InputError, *], CatalogInfo, Unit] = {
      val validArgs =
        (
          name.validateIsNotNull("name"),
          id.validateIsNotNull("id")
        ).tupled.toEither

      for {
        args <- StateT.liftF(validArgs)
        (n, i) = args
        _ <- CatalogInfo.catalog    := n
        _ <- CatalogInfo.id         := i
        _ <- CatalogInfo.objectType := objectType.toOptionOption
      } yield ()

    }
  }

  object EditInput {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[EditInput] =
      deriveConfiguredDecoder[EditInput]

    implicit val EqEditInput: Eq[EditInput] =
      Eq.by { a => (
        a.name,
        a.id,
        a.objectType
      )}

  }

  implicit val DecoderCatalogInfo: Decoder[CatalogInfo] =
    deriveDecoder[CatalogInfo]

  /*
  final case class Input(
    name: CatalogName,
    id:   String
  ) {

    def toCatalogInfo: ValidatedInput[CatalogInfo] =
      NonEmptyString
        .from(id)
        .leftMap(er => InputError.invalidField("id", id, s"Catalog id must be non-empty: $er"))
        .toValidatedNec
        .map(id => CatalogInfo(name, id, None))

  }

  object Input {

    implicit val DecoderInput: Decoder[Input] =
      deriveDecoder[Input]

    implicit val EqInput: Eq[Input] =
      Eq.by(i => (i.name, i.id))

  }
   */

}
