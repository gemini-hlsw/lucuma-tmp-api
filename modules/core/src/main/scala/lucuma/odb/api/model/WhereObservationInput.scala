// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{Observation, Program}
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOptionStringInput, WhereOrderInput}

final case class WhereObservationInput(
  AND:          Option[List[WhereObservationInput]]      = None,
  OR:           Option[List[WhereObservationInput]]      = None,
  NOT:          Option[WhereObservationInput]            = None,

  id:           Option[WhereOrderInput[Observation.Id]]  = None,
  programId:    Option[WhereOrderInput[Program.Id]]      = None,
  subtitle:     Option[WhereOptionStringInput]           = None,
  status:       Option[WhereOrderInput[ObsStatus]]       = None,
  activeStatus: Option[WhereOrderInput[ObsActiveStatus]] = None,
  existence:    Option[WhereEqInput[Existence]]          = WhereEqInput.EQ(Existence.Present: Existence).some
) extends WhereCombinator[ObservationModel] {

  override def matches(a: ObservationModel): Boolean =
    combinatorMatches(a)                                   &&
      id.forall(_.matches(a.id))                           &&
      programId.forall(_.matches(a.programId))             &&
      subtitle.forall(_.matchesNonEmptyString(a.subtitle)) &&
      status.forall(_.matches(a.status))                   &&
      activeStatus.forall(_.matches(a.activeStatus))       &&
      existence.forall(_.matches(a.existence))

}

object WhereObservationInput {

  val MatchPresent: WhereObservationInput =
    WhereObservationInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereObservationInput: Decoder[WhereObservationInput] =
   deriveConfiguredDecoder[WhereObservationInput]

}