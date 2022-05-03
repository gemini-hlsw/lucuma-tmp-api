// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import lucuma.odb.api.model.{DatasetFilename, DatasetModel}
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.Paging.Cursor
import lucuma.odb.api.schema.syntax.scalar._
import monocle.Prism
import org.typelevel.log4cats.Logger
import sangria.schema._


object DatasetSchema {

  import context._
  import StepSchema.StepIdType

  val DatasetIdCursor: Prism[Cursor, DatasetModel.Id] =
    Prism[Cursor, DatasetModel.Id] { c =>
      DatasetModel.Id.fromString.getOption(c.toString)
    } { did => new Cursor(DatasetModel.Id.fromString.reverseGet(did)) }

  val IndexCursor: Prism[Cursor, PosInt] =
    Prism[Cursor, PosInt](
      _.toString match {
        case DatasetModel.Id.PosIntPattern(idx) => PosInt.unapply(java.lang.Integer.parseInt(idx))
        case _                                  => None
      }
    ) {
      idx => new Cursor(idx.value.toString)
    }

  implicit val DatasetFilenameScalar: ScalarType[DatasetFilename] =
    ScalarType.fromScalarFormat(
      name         = "DatasetFilename",
      description  = "Dataset filename in standard format",
      scalarFormat = ScalarFormat(DatasetFilename.fromString, "N20210519S0001.fits")
    )

  def DatasetType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], DatasetModel] =
    ObjectType(
      name     = "Dataset",
      fieldsFn = () => fields(

        Field(
          name        = "observation",
          fieldType   = ObservationSchema.ObservationType[F],
          description = Some("Observation associated with this dataset"),
          resolve     = c => c.observation(_.unsafeSelect(c.value.dataset.observationId, includeDeleted = true))
        ),

        Field(
          name        = "stepId",
          fieldType   = StepIdType,
          description = "Step ID".some,
          resolve     = _.value.id.stepId
        ),

        Field(
          name        = "index",
          fieldType   = IntType,
          description = Some("Dataset index"),
          resolve     = _.value.id.index.value
        ),

        Field(
          name        = "filename",
          fieldType   = DatasetFilenameScalar,
          description = Some("Dataset filename"),
          resolve     = _.value.dataset.filename
        )

      )
    )

  def DatasetEdgeType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Edge[DatasetModel]] =
    Paging.EdgeType(
      "DatasetEdge",
      "A Dataset and its cursor",
      DatasetType[F]
    )

  def DatasetConnectionType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], Paging.Connection[DatasetModel]] =
    Paging.ConnectionType(
      "DatasetConnection",
      "Datasets in the current page",
      DatasetType[F],
      DatasetEdgeType[F]
    )

}
