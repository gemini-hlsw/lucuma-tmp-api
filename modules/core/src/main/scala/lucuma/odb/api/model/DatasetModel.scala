// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.model.{Observation, Step, WithId}
import cats.Monad
import cats.kernel.Eq

import cats.mtl.Stateful
import cats.syntax.all._
import eu.timepit.refined.auto._
import monocle.macros.Lenses
import io.chrisdavenport.cats.time.instances.instant._
import io.circe.Decoder
import io.circe.generic.semiauto._

import java.time.Instant

// TODO: Move to lucuma-core
object Dataset extends WithId('d')


@Lenses final case class DatasetModel(
  id:            Dataset.Id,
  observationId: Observation.Id,
  stepId:        Step.Id,
  timestamp:     Instant,
  filename:      DatasetFilename
)

object DatasetModel {

  implicit val EqDatasetModel: Eq[DatasetModel] =
    Eq.by { a => (
      a.id,
      a.timestamp,
      a.filename,
      a.observationId,
      a.stepId
    )}


  final case class Create(
    datasetId:     Option[Dataset.Id],
    observationId: Observation.Id,
    stepId:        Step.Id,
    timestamp:     Instant,
    filename:      DatasetFilename
  ) {

    def create[F[_]: Monad, T](db: DatabaseState[T])(implicit S: Stateful[F, T]): F[ValidatedInput[DatasetModel]] =
      for {
        i <- db.dataset.getUnusedId(datasetId)
        o <- db.observation.lookupValidated(observationId)
        s <- db.step.lookupValidated(stepId)
        d  = (i, o, s).mapN((iʹ, _, _) => DatasetModel.apply(iʹ, observationId, stepId, timestamp, filename))
        _ <- db.dataset.saveIfValid(d)(_.id)
      } yield d

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.datasetId,
        a.stepId,
        a.timestamp,
        a.filename
      )}

  }
}
