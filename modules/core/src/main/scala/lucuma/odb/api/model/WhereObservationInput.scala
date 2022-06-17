// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{Observation, Program}
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOptionStringInput, WhereOrderInput}

final case class WhereObservationInput(
  AND:          Option[List[WhereObservationInput]],
  OR:           Option[List[WhereObservationInput]],
  NOT:          Option[WhereObservationInput],

  id:           Option[WhereOrderInput[Observation.Id]],
  programId:    Option[WhereOrderInput[Program.Id]],
  subtitle:     Option[WhereOptionStringInput],
  existence:    Option[WhereEqInput[Existence]],
  status:       Option[WhereOrderInput[ObsStatus]],
  activeStatus: Option[WhereOrderInput[ObsActiveStatus]]
) extends WhereCombinator[ObservationModel] {

  override def matches(a: ObservationModel): Boolean =
    combinatorMatches(a)                                   &&
      id.forall(_.matches(a.id))                           &&
      programId.forall(_.matches(a.programId))             &&
      subtitle.forall(_.matchesNonEmptyString(a.subtitle)) &&
      existence.forall(_.matches(a.existence))             &&
      status.forall(_.matches(a.status))                   &&
      activeStatus.forall(_.matches(a.activeStatus))

}

object WhereObservationInput {

  implicit val DecoderWhereObservationInput: Decoder[WhereObservationInput] =
   deriveDecoder[WhereObservationInput]

}