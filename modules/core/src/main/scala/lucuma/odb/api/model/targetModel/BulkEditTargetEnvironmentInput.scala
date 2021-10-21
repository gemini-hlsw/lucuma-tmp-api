// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.{Eq, Monad}
import cats.data.State
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import clue.data.Input
import io.circe.Decoder
import lucuma.core.optics.syntax.optional._
import lucuma.odb.api.model.{CoordinatesModel, DatabaseState, DeclinationModel, RightAscensionModel, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.validatedinput._


/**
 * Bulk editing for the non-science target parts of the target environment.
 */
final case class BulkEditTargetEnvironmentInput(
  select:       SelectTargetEnvironmentInput,
  explicitBase: Input[CoordinatesModel.Input] = Input.ignore
) {

  val editor: ValidatedInput[State[TargetEnvironmentModel, Unit]] =
    explicitBase.validateNullable(_.toCoordinates).map { b =>
      (TargetEnvironmentModel.explicitBase := b).void
    }

  def edit[F[_]: Monad, T](
    db: DatabaseState[T]
  )(implicit S: Stateful[F, T]): F[ValidatedInput[List[TargetEnvironmentContext]]] =
    for {
      vs <- select.select(db)
      es  = (vs, editor).mapN { (v, ed) =>
        v.map(ed.runS).map(_.value)
      }
      _ <- es.traverse_(tms => tms.traverse_(tm => db.targetEnvironment.update(tm.id, tm)))
      r <- es.traverse(_.traverse(e => TargetEnvironmentContext.fromId(db, e.id)).map(_.sequence))
    } yield r.flatten

}

object BulkEditTargetEnvironmentInput {

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderEdit: Decoder[BulkEditTargetEnvironmentInput] =
    deriveConfiguredDecoder[BulkEditTargetEnvironmentInput]

  implicit val EqEdit: Eq[BulkEditTargetEnvironmentInput] =
    Eq.by { a => (
      a.select,
      a.explicitBase
    )}

  def explicitBase(
    select: SelectTargetEnvironmentInput,
    ra:     RightAscensionModel.Input,
    dec:    DeclinationModel.Input
  ): BulkEditTargetEnvironmentInput =
    BulkEditTargetEnvironmentInput(
      select,
      Input.assign(CoordinatesModel.Input(ra, dec))
    )

}
