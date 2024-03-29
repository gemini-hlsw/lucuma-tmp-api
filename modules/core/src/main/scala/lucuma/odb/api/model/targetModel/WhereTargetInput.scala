// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.kernel.Eq
import cats.syntax.option._
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import lucuma.core.model.{Program, Target}
import lucuma.odb.api.model.query.{WhereCombinator, WhereOrderInput, WhereStringInput}

final case class WhereTargetInput(
  AND:       Option[List[WhereTargetInput]]      = None,
  OR:        Option[List[WhereTargetInput]]      = None,
  NOT:       Option[WhereTargetInput]            = None,

  id:        Option[WhereOrderInput[Target.Id]]  = None,
  programId: Option[WhereOrderInput[Program.Id]] = None,
  name:      Option[WhereStringInput]            = None
) extends WhereCombinator[TargetModel] {

  override def matches(a: TargetModel): Boolean =
    combinatorMatches(a)                           &&
      id.forall(_.matches(a.id))                   &&
      programId.forall(_.matches(a.programId))     &&
      name.forall(_.matchesNonEmptyString(a.name))

  def withId(id: Target.Id): WhereTargetInput =
    copy(id = WhereOrderInput.EQ(id).some)

  def withIds(ids: List[Target.Id]): WhereTargetInput =
    copy(id = WhereOrderInput.IN(ids).some)

  def withProgramId(pid: Program.Id): WhereTargetInput =
    copy(programId = WhereOrderInput.EQ(pid).some)

}

object WhereTargetInput {

  val MatchAll: WhereTargetInput =
    WhereTargetInput()

  implicit val customConfig: Configuration =
    Configuration.default.withDefaults

  implicit val DecoderWhereTargetInput: Decoder[WhereTargetInput] =
    deriveConfiguredDecoder[WhereTargetInput]

  implicit val EqWhereTargetInput: Eq[WhereTargetInput] =
    Eq.fromUniversalEquals

}
