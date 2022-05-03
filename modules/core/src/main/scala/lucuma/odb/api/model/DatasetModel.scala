// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.{Order, Show}
import cats.syntax.bitraverse._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt
import io.circe._
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.`enum`.DatasetQaState
import lucuma.core.model.Observation
import lucuma.core.optics.Format
import monocle.{Focus, Lens}

import scala.util.matching.Regex

final case class DatasetModel(
  id:      DatasetModel.Id,
  dataset: DatasetModel.Dataset
)

object DatasetModel extends DatasetModelOptics {

  final case class Id(
    stepId: Step.Id,
    index:  PosInt
  )

  object Id {
    implicit val OrderId: Order[Id] =
      Order.by { a => (a.stepId, a.index) }

    val PosIntPattern: Regex =
      raw"([1-9a-f][0-9a-f]*)".r

    val fromString: Format[String, Id] =
      Format(
        _.split(',').toList match {
          case List(sid, PosIntPattern(idx)) =>
            (Step.Id.parse(sid), PosInt.unapply(java.lang.Integer.parseInt(idx)))
            .bisequence
            .map { case (sid, idx) => Id(sid, idx) }
          case _                             =>
            None
        },
        id => s"${Uid[Step.Id].show(id.stepId)},${id.index}"
      )

    implicit val ShowId: Show[Id] =
      Show.show[Id](fromString.reverseGet)

    implicit val DecoderId: Decoder[Id] =
      deriveDecoder[Id]

  }

  final case class Dataset(
    observationId: Observation.Id,
    filename:      DatasetFilename,
    qaState:       Option[DatasetQaState]
  )

  object Dataset extends DatasetOptics {

    implicit val OrderDataset: Order[Dataset] =
      Order.by { a => (
        a.observationId,
        a.filename,
        a.qaState
      )}

  }

  sealed trait DatasetOptics { self: Dataset.type =>

    val filename: Lens[Dataset, DatasetFilename] =
      Focus[Dataset](_.filename)

    val qaState: Lens[Dataset, Option[DatasetQaState]] =
      Focus[Dataset](_.qaState)

  }

  implicit val OrderDatasetModel: Order[DatasetModel] =
    Order.by { a => (
      a.id,
      a.dataset
    )}

  final case class SetDatasetQaStateInput(
    qaState:    DatasetQaState,
    datasetIds: List[DatasetModel.Id]
  )

  object SetDatasetQaStateInput {

    implicit val DecoderSetDatasetQaStateInput: Decoder[SetDatasetQaStateInput] =
      deriveDecoder[SetDatasetQaStateInput]

  }

}

sealed trait DatasetModelOptics { self: DatasetModel.type =>

  val dataset: Lens[DatasetModel, Dataset] =
    Focus[DatasetModel](_.dataset)

  val filename: Lens[DatasetModel, DatasetFilename] =
    dataset andThen DatasetModel.Dataset.filename

  val qaState: Lens[DatasetModel, Option[DatasetQaState]] =
    dataset andThen DatasetModel.Dataset.qaState

}