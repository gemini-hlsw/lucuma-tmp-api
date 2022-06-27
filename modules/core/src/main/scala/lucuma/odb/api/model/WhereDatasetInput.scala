// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.syntax.option._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt
import io.circe.refined._
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import lucuma.core.enums.DatasetQaState
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

  def withObservation(id: Observation.Id): WhereDatasetInput =
    copy(observationId = WhereOrderInput.EQ(id).some)

  def withStep(id: Step.Id): WhereDatasetInput =
    copy(stepId = WhereEqInput.EQ(id).some)
}

object WhereDatasetInput {

  val MatchAll: WhereDatasetInput =
    WhereDatasetInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereDatasetInput: Decoder[WhereDatasetInput] =
    deriveConfiguredDecoder[WhereDatasetInput]

  implicit val EqWhereDatasetInput: Eq[WhereDatasetInput] =
    Eq.fromUniversalEquals

}
