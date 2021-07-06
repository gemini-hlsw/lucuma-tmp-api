// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.core.model.Step
import lucuma.core.util.Gid
import lucuma.odb.api.model.{DatasetFilename, DatasetModel}
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.Paging.Cursor
import lucuma.odb.api.schema.syntax.scalar._

import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import eu.timepit.refined.types.all.PosInt
import monocle.Prism
import sangria.schema._

import scala.util.matching.Regex


object DatasetSchema {

  import context._

  private val PosIntPattern: Regex =
    raw"([1-9a-f][0-9a-f]*)".r

  val StepAndIndexCursor: Prism[Cursor, (Step.Id, PosInt)] =
    Prism[Cursor, (Step.Id, PosInt)](
      _.toString.split(',').toList match {
        case List(sid, PosIntPattern(idx)) =>
          (Step.Id.parse(sid),
           PosInt.unapply(java.lang.Integer.parseInt(idx))
          ).bisequence
        case _                             =>
          None
      }
    ) {
      case (sid, idx) =>
        new Cursor(s"${Gid[Step.Id].show(sid)},${idx.value}")
    }

  val IndexCursor: Prism[Cursor, PosInt] =
    Prism[Cursor, PosInt](
      _.toString match {
        case PosIntPattern(idx) => PosInt.unapply(java.lang.Integer.parseInt(idx))
        case _                  => None
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

  def DatasetType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], DatasetModel] =
    ObjectType(
      name     = "Dataset",
      fieldsFn = () => fields(

        Field(
          name        = "observation",
          fieldType   = ObservationSchema.ObservationType[F],
          description = Some("Observation associated with this dataset"),
          resolve     = c => c.observation(_.unsafeSelect(c.value.observationId, includeDeleted = true))
        ),

        Field(
          name        = "step",
          fieldType   = StepSchema.StepInterfaceType[F],
          description = Some("Step that produced the dataset"),
          resolve     = c => c.step(_.unsafeSelectStep(c.value.stepId))
        ),

        Field(
          name        = "index",
          fieldType   = IntType,
          description = Some("Dataset index"),
          resolve     = _.value.index.value
        ),

        Field(
          name        = "filename",
          fieldType   = DatasetFilenameScalar,
          description = Some("Dataset filename"),
          resolve     = _.value.filename
        )

      )
    )

  def DatasetEdgeType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Edge[DatasetModel]] =
    Paging.EdgeType(
      "DatasetEdge",
      "A Dataset and its cursor",
      DatasetType[F]
    )

  def DatasetConnectionType[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], Paging.Connection[DatasetModel]] =
    Paging.ConnectionType(
      "DatasetConnection",
      "Datasets in the current page",
      DatasetType[F],
      DatasetEdgeType[F]
    )

}
