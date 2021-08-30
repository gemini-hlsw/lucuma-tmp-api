// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import cats.Eq
import eu.timepit.refined.cats.refTypeOrder
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import lucuma.core.model.Target

//
// # Target selection.  Choose at least one of `names` or `targetIds`.
// input SelectTargetInput {
//
//   names:     [ NonEmptyString! ]
//   targetIds: [ TargetId! ]
//
// }
//
final case class SelectTargetInput(
  names:     Option[List[NonEmptyString]],
  targetIds: Option[List[Target.Id]]
) {

  private def toSet[A](as: Option[List[A]]): Set[A] =
    as.fold(Set.empty[A])(_.toSet)

  val nameMatches: Set[NonEmptyString] =
    toSet(names)

  val idMatches: Set[Target.Id] =
    toSet(targetIds)

  def matches(vs: Set[TargetEnvironment.Id], t: TargetModel): Boolean =
    (idMatches(t.id) && (vs.isEmpty || vs(t.targetEnvironmentId))) ||  // identified by target id (env optional)
    (nameMatches(t.target.name) && vs(t.targetEnvironmentId))          // identified by name (requires env)

}

object SelectTargetInput {

  implicit val DecoderSelectTarget: Decoder[SelectTargetInput] =
    deriveDecoder[SelectTargetInput]

  implicit val EqSelectTarget: Eq[SelectTargetInput] =
    Eq.by { a => (
      a.names,
      a.targetIds
    )}

}

