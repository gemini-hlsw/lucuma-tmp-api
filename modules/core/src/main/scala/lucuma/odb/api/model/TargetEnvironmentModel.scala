// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Coordinates
import lucuma.core.model.Target

import cats.Eq
import cats.data.State
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.all._
import eu.timepit.refined.types.string._
import eu.timepit.refined.cats.refTypeOrder
import io.circe.Decoder
import io.circe.generic.semiauto._

import scala.collection.immutable.SortedMap


final case class TargetEnvironmentModel(
  explicitBase: Option[Coordinates],
  science:      SortedMap[NonEmptyString, Target]
  // guide stars, blind offset, etc. go here in the future
)

object TargetEnvironmentModel {

  implicit val EqTargetEnvironmentModel: Eq[TargetEnvironmentModel] =
    Eq.by { tem => (
      tem.explicitBase,
      tem.science
    )}

  final case class Create(
    explicitBase: Option[CoordinatesModel.Input],
    science:      List[TargetNewModel.Create]
  ) {

    def create: ValidatedInput[TargetEnvironmentModel] =
      (explicitBase.traverse(_.toCoordinates),
       science.traverse(_.toGemTarget)
      ).mapN { (b, s) =>
        TargetEnvironmentModel(b, SortedMap.from(s.fproductLeft(_.name)))
      }

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.explicitBase,
        a.science
      )}

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

}
