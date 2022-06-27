// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.{Eq, Order, Show}
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.{PosInt, NonNegInt}
import io.circe._
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.Observation
import lucuma.core.optics.Format
import lucuma.core.util.Gid
import lucuma.odb.api.model.query.SizeLimitedResult
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.input._
import monocle.{Focus, Lens}

import scala.util.matching.Regex

final case class DatasetModel(
  id:      DatasetModel.Id,
  dataset: DatasetModel.Dataset
)

object DatasetModel extends DatasetModelOptics {

  final case class Id(
    observationId: Observation.Id,
    stepId:        Step.Id,
    index:         PosInt
  )

  object Id {
    implicit val OrderId: Order[Id] =
      Order.by { a => (a.observationId, a.stepId, a.index) }

    val PosIntPattern: Regex =
      raw"([1-9a-f][0-9a-f]*)".r

    val fromString: Format[String, Id] =
      Format(
        _.split(',').toList match {
          case List(oid, sid, PosIntPattern(idx)) =>
            (Observation.Id.parse(oid), Step.Id.parse(sid), PosInt.unapply(java.lang.Integer.parseInt(idx)))
            .tupled
            .map { case (oid, sid, idx) => Id(oid, sid, idx) }
          case _                             =>
            None
        },
        id => s"${Gid[Observation.Id].show(id.observationId)},${Uid[Step.Id].show(id.stepId)},${id.index}"
      )

    implicit val ShowId: Show[Id] =
      Show.show[Id](fromString.reverseGet)

    implicit val DecoderId: Decoder[Id] =
      deriveDecoder[Id]

  }

  final case class Dataset(
    filename: DatasetFilename,
    qaState:  Option[DatasetQaState]
  )

  object Dataset extends DatasetOptics {

    implicit val OrderDataset: Order[Dataset] =
      Order.by { a => (
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

  final case class PropertiesInput(
    qaState:  Input[DatasetQaState] = Input.ignore
  ) {

    val edit: StateT[EitherInput, DatasetModel, Unit] =
      (DatasetModel.qaState := qaState.toOptionOption).void

  }

  object PropertiesInput {

    val Empty: PropertiesInput =
      PropertiesInput(Input.ignore)

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderPropertiesInput: Decoder[PropertiesInput] =
      deriveConfiguredDecoder[PropertiesInput]

    implicit val EqPropertiesInput: Eq[PropertiesInput] =
      Eq.by(_.qaState)

  }

  final case class UpdateInput(
    SET:   PropertiesInput,
    WHERE: Option[WhereDatasetInput],
    LIMIT: Option[NonNegInt]
  ) {

    def filteredDatasets(db: Database): List[DatasetModel] = {
      val ds = db.datasets.datasets.iterator.to(LazyList).map((DatasetModel.apply _).tupled)
      WHERE.fold(ds)(where => ds.filter(where.matches)).toList
    }

    val editor: StateT[EitherInput, Database, SizeLimitedResult.Update[DatasetModel]] =
      for {
        ds  <- StateT.inspect[EitherInput, Database, List[DatasetModel]](filteredDatasets)
        dsʹ <- StateT.liftF[EitherInput, Database, List[DatasetModel]](ds.traverse(SET.edit.runS))
        _   <- Database.datasets.mod_ { t =>
          dsʹ.foldLeft(t)((tʹ, d) => tʹ.updated(d))
        }
      } yield SizeLimitedResult.Update.fromAll(dsʹ, LIMIT)

  }

  object UpdateInput {

    implicit val DecoderUpdateInput: Decoder[UpdateInput] =
      deriveDecoder[UpdateInput]

    implicit val EqUpdateInput: Eq[UpdateInput] =
      Eq.by { a => (
        a.SET,
        a.WHERE,
        a.LIMIT
      )}

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
