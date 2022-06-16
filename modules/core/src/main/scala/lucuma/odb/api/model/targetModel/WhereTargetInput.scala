// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import lucuma.core.model.{Program, Target}
import lucuma.odb.api.model.Existence
import lucuma.odb.api.model.query.{WhereCombinator, WhereEqInput, WhereOrderInput, WhereStringInput}

final case class WhereTargetInput(
  AND:       Option[List[WhereTargetInput]],
  OR:        Option[List[WhereTargetInput]],
  NOT:       Option[WhereTargetInput],

  id:        Option[WhereOrderInput[Target.Id]],
  programId: Option[WhereOrderInput[Program.Id]],
  name:      Option[WhereStringInput],
  existence: Option[WhereEqInput[Existence]]
) extends WhereCombinator[TargetModel] {

  override def matches(a: TargetModel): Boolean =
    combinatorMatches(a)                           &&
      id.forall(_.matches(a.id))                   &&
      programId.forall(_.matches(a.programId))     &&
      name.forall(_.matchesNonEmptyString(a.name)) &&
      existence.forall(_.matches(a.existence))

}

object WhereTargetInput {

  implicit val DecoderWhereTargetInput: Decoder[WhereTargetInput] =
    deriveDecoder[WhereTargetInput]

}
