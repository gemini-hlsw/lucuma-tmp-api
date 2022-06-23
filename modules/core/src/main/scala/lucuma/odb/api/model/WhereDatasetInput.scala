// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.option._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt
import io.circe.refined._
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import lucuma.core.`enum`.DatasetQaState
import lucuma.core.model.Observation
import lucuma.odb.api.model.query.{WhereEqInput, WhereOptionEqInput, WhereOrderInput, WherePredicate, WhereStringInput}


final case class WhereDatasetInput(
  observationId: Option[WhereOrderInput[Observation.Id]]    = None,
  stepId:        Option[WhereEqInput[Step.Id]]              = None,
  index:         Option[WhereOrderInput[PosInt]]            = None,
  filename:      Option[WhereStringInput]                   = None,
  qaState:       Option[WhereOptionEqInput[DatasetQaState]] = None
) extends WherePredicate[DatasetModel] {

  override def matches(a: DatasetModel): Boolean =
    observationId.forall(_.matches(a.id.observationId))     &&
      stepId.forall(_.matches(a.id.stepId))                 &&
      index.forall(_.matches(a.id.index))                   &&
      filename.forall(_.matches(a.dataset.filename.format)) &&
      qaState.forall(_.matches(a.dataset.qaState))

}

object WhereDatasetInput {

  val MatchAll: WhereDatasetInput =
    WhereDatasetInput()

  def matchObservation(observationId: Observation.Id): WhereDatasetInput =
    MatchAll.copy(observationId = WhereOrderInput.EQ(observationId).some)

  def matchStep(observationId: Observation.Id, stepId: Step.Id): WhereDatasetInput =
    MatchAll.copy(
      observationId = WhereOrderInput.EQ(observationId).some,
      stepId        = WhereEqInput.EQ(stepId).some
    )

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereDatasetInput: Decoder[WhereDatasetInput] =
    deriveConfiguredDecoder[WhereDatasetInput]

}
