// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.model.{DatasetFilename, DatasetModel}
import lucuma.odb.api.model.format.ScalarFormat
import lucuma.odb.api.repo.OdbRepo
import lucuma.odb.api.schema.syntax.scalar._

import cats.effect.Effect

import sangria.schema._


object DatasetSchema {

  import context._

  implicit val DatasetFilenameScalar: ScalarType[DatasetFilename] =
    ScalarType.fromScalarFormat(
      name         = "DatasetFilename",
      description  = "Dataset filename in standard format",
      scalarFormat = ScalarFormat(DatasetFilename.fromString, "N20210519S0001.fits")
    )

  def DatasetType[F[_]](implicit F: Effect[F]): ObjectType[OdbRepo[F], DatasetModel] =
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

  def DatasetEdgeType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Edge[DatasetModel]] =
    Paging.EdgeType(
      "DatasetEdge",
      "A Dataset and its cursor",
      DatasetType[F]
    )

  def DatasetConnectionType[F[_]: Effect]: ObjectType[OdbRepo[F], Paging.Connection[DatasetModel]] =
    Paging.ConnectionType(
      "DatasetConnection",
      "Datasets in the current page",
      DatasetType[F],
      DatasetEdgeType[F]
    )

}
