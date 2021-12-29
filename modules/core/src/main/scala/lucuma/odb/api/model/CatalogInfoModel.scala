// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.`enum`.CatalogName
import lucuma.core.model.CatalogInfo

import cats.Eq
import cats.syntax.either._
import eu.timepit.refined.types.all.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._

object CatalogInfoModel {

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
}
