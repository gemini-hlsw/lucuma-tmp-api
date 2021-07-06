// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.Program
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.scalacheck.string._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbProgramModel {

  import ArbEnumerated._
  import ArbGid._

  implicit val arbProgramModel: Arbitrary[ProgramModel] =
    Arbitrary {
      for {
        id <- arbitrary[Program.Id]
        ex <- arbitrary[Existence]
        nm <- arbitrary[Option[NonEmptyString]]
      } yield ProgramModel(id, ex, nm)
    }

  implicit val cogProgramModel: Cogen[ProgramModel] =
    Cogen[(
      Program.Id,
      Existence,
      Option[String]
    )].contramap { in => (
      in.id,
      in.existence,
      in.name.map(_.value)
    )}

  implicit val arbProgramModelCreate: Arbitrary[ProgramModel.Create] =
    Arbitrary {
      for {
        id <- arbitrary[Option[Program.Id]]
        nm <- arbitrary[Option[NonEmptyString]]
      } yield ProgramModel.Create(id, nm)
    }

  implicit val cogProgramModelCreate: Cogen[ProgramModel.Create] =
    Cogen[(
      Option[Program.Id],
      Option[String]
    )].contramap { in => (
      in.programId,
      in.name.map(_.value)
    )}
}

object ArbProgramModel extends ArbProgramModel
