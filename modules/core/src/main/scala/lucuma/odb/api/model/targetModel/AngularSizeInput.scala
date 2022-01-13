// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import cats.data.{EitherNec, StateT}
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.traverse._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.AngularSize
import lucuma.odb.api.model.AngleModel.AngleInput
import lucuma.odb.api.model.{InputError, NullableInput, ValidatedInput}
import lucuma.odb.api.model.syntax.lens._

final case class AngularSizeInput(
  majorAxis: Option[AngleInput],
  minorAxis: Option[AngleInput]
) extends NullableInput[AngularSize] {

  override val create: ValidatedInput[AngularSize] =
    (majorAxis.toValidNec(InputError.missingInput("majorAxis")),
     minorAxis.toValidNec(InputError.missingInput("minorAxis"))
    ).tupled.andThen { case (j, n) =>
      (j.toAngle, n.toAngle).mapN((ja, na) => AngularSize(ja, na))
    }

  override val edit: StateT[EitherNec[InputError, *], AngularSize, Unit] =
    for {
      args  <- StateT.liftF((majorAxis.traverse(_.toAngle), minorAxis.traverse(_.toAngle)).tupled.toEither)
      (j, n) = args
      _     <- AngularSize.majorAxis := j
      _     <- AngularSize.minorAxis := n
    } yield ()

}

object AngularSizeInput {

  implicit val DecoderAngularSizeInput: Decoder[AngularSizeInput] =
    deriveDecoder[AngularSizeInput]

  implicit val EqAngularSizeInput: Eq[AngularSizeInput] =
    Eq.by { a => (
      a.majorAxis,
      a.minorAxis
    )}

}
