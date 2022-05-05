// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.effect.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import lucuma.core.`enum`.DatasetQaState
import lucuma.odb.api.model.{DatasetFilename, DatasetModel}
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.repo.OdbCtx
import lucuma.odb.api.schema.Paging.Cursor
import lucuma.odb.api.schema.syntax.`enum`._
import lucuma.odb.api.schema.syntax.scalar._
import monocle.Prism
import org.typelevel.log4cats.Logger
import sangria.schema._


object DatasetSchema {

  import context._
  import ObservationSchema.ObservationIdType
  import RefinedSchema.PosIntType
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

  implicit val EnumTypeDatasetQaState: EnumType[DatasetQaState] =
    EnumType.fromEnumerated(
      "DatasetQaState",
      "Dataset QA State"
    )

  val ArgumentDatasetQaState: Argument[DatasetQaState] =
    Argument(
      name         = "qaState",
      argumentType = EnumTypeDatasetQaState,
      description  = "Dataset QA State"
    )

  val ArgumentDatasetIndex: Argument[PosInt] =
    Argument(
      name         = "index",
      argumentType = PosIntType,
      description  = "Dataset index"
    )

  val ArgumentOptionalDatasetIndex: Argument[Option[PosInt]] =
    Argument(
      name         = "index",
      argumentType = OptionInputType(PosIntType),
      description  = "Dataset index"
    )

  val DatasetIdType: ObjectType[Any, DatasetModel.Id] =
    ObjectType(
      name     = "DatasetId",
      fieldsFn = () => fields(
        Field(
          name        = "observationId",
          fieldType   = ObservationIdType,
          description = "Observation ID".some,
          resolve     = _.value.observationId
        ),

        Field(
          name        = "stepId",
          fieldType   = StepIdType,
          description = "Step ID".some,
          resolve     = _.value.stepId
        ),

        Field(
          name        = "index",
          fieldType   = PosIntType,
          description = "Dataset index".some,
          resolve     = _.value.index
        )
      )
    )

  def DatasetType[F[_]: Dispatcher: Async: Logger]: ObjectType[OdbCtx[F], DatasetModel] =
    ObjectType(
      name     = "Dataset",
      fieldsFn = () => fields(

        Field(
          name        = "observation",
          fieldType   = ObservationSchema.ObservationType[F],
          description = Some("Observation associated with this dataset"),
          resolve     = c => c.observation(_.unsafeSelect(c.value.id.observationId, includeDeleted = true))
        ),

        Field(
          name        = "id",
          fieldType   = DatasetIdType,
          description = "Dataset id".some,
          resolve     = _.value.id
        ),

        Field(
          name        = "filename",
          fieldType   = DatasetFilenameScalar,
          description = "Dataset filename".some,
          resolve     = _.value.dataset.filename
        ),

        Field(
          name        = "qaState",
          fieldType   = OptionType(EnumTypeDatasetQaState),
          description = "Dataset QA state".some,
          resolve     = _.value.dataset.qaState
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
