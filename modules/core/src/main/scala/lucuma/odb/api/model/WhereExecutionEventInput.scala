// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import eu.timepit.refined.cats._
import eu.timepit.refined.types.all.PosInt
import io.circe.Decoder
import io.circe.refined._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import lucuma.core.model.{ExecutionEvent, Observation}
import lucuma.odb.api.model.ExecutionEventModel.{DatasetEvent, DatasetStageType, SequenceCommandType, SequenceEvent, StepEvent, StepStageType}
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOptionStringInput, WhereOrderInput, WherePredicate}
import org.typelevel.cats.time._

import java.time.Instant

final case class WhereSequenceEventInput(
  command: Option[WhereOrderInput[SequenceCommandType]] = None
) extends WherePredicate[SequenceEvent] {

  override def matches(a: SequenceEvent): Boolean =
    command.forall(_.matches(a.payload.command))

}

object WhereSequenceEventInput {

  val MatchAll: WhereSequenceEventInput =
    WhereSequenceEventInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderSequenceEventInput: Decoder[WhereSequenceEventInput] =
    deriveConfiguredDecoder[WhereSequenceEventInput]

}

final case class WhereStepEventInput(
  stepId:       Option[WhereEqInput[Step.Id]]                       = None,
  sequenceType: Option[WhereOrderInput[SequenceModel.SequenceType]] = None,
  stage:        Option[WhereOrderInput[StepStageType]]              = None
) extends WherePredicate[StepEvent] {

  override def matches(a: StepEvent): Boolean =
    stepId.forall(_.matches(a.location.stepId))              &&
      sequenceType.forall(_.matches(a.payload.sequenceType)) &&
      stage.forall(_.matches(a.payload.stepStage))

}

object WhereStepEventInput {

  val MatchAll: WhereStepEventInput =
    WhereStepEventInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderStepEventInput: Decoder[WhereStepEventInput] =
    deriveConfiguredDecoder[WhereStepEventInput]

}


final case class WhereDatasetEventInput(
  stepId:   Option[WhereEqInput[Step.Id]]             = None,
  index:    Option[WhereOrderInput[PosInt]]           = None,
  stage:    Option[WhereOrderInput[DatasetStageType]] = None,
  filename: Option[WhereOptionStringInput]            = None
) extends WherePredicate[DatasetEvent] {

  override def matches(a: DatasetEvent): Boolean =
    stepId.forall(_.matches(a.location.stepId))                    &&
      index.forall(_.matches(a.location.index))                    &&
      stage.forall(_.matches(a.payload.datasetStage))                     &&
      filename.forall(_.matches(a.payload.filename.map(_.format)))

}

object WhereDatasetEventInput {

  val MatchAll: WhereDatasetEventInput =
    WhereDatasetEventInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereDatasetEventInput: Decoder[WhereDatasetEventInput] =
    deriveConfiguredDecoder[WhereDatasetEventInput]

}

final case class WhereExecutionEventInput(
  AND: Option[List[WhereExecutionEventInput]]               = None,
  OR:  Option[List[WhereExecutionEventInput]]               = None,
  NOT: Option[WhereExecutionEventInput]                     = None,

  id:            Option[WhereOrderInput[ExecutionEvent.Id]] = None,
  visitId:       Option[WhereEqInput[Visit.Id]]             = None,
  observationId: Option[WhereOrderInput[Observation.Id]]    = None,
  received:      Option[WhereOrderInput[Instant]]           = None,

  sequenceEvent: Option[WhereSequenceEventInput]            = None,
  stepEvent:     Option[WhereStepEventInput]                = None,
  datasetEvent:  Option[WhereDatasetEventInput]             = None

) extends WhereCombinator[ExecutionEventModel] {

  private def typeMatches(a: ExecutionEventModel): Boolean =
    a match {
      case e @ ExecutionEventModel.SequenceEvent(_, _, _, _, _) =>
        sequenceEvent.forall(_.matches(e)) && stepEvent.isEmpty && datasetEvent.isEmpty

      case e @ StepEvent(_, _, _, _, _)                         =>
        sequenceEvent.isEmpty && stepEvent.forall(_.matches(e)) && datasetEvent.isEmpty

      case e @ DatasetEvent(_, _, _, _, _)                      =>
        sequenceEvent.isEmpty && stepEvent.isEmpty && datasetEvent.forall(_.matches(e))
    }

  override def matches(a: ExecutionEventModel): Boolean =
    combinatorMatches(a)                               &&
      id.forall(_.matches(a.id))                       &&
      visitId.forall(_.matches(a.visitId))             &&
      observationId.forall(_.matches(a.observationId)) &&
      received.forall(_.matches(a.received))           &&
      typeMatches(a)

}


object WhereExecutionEventInput {

  val MatchAll: WhereExecutionEventInput =
    WhereExecutionEventInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereExecutionEventInput: Decoder[WhereExecutionEventInput] =
    deriveConfiguredDecoder[WhereExecutionEventInput]

}
