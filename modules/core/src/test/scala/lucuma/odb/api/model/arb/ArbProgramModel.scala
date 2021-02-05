// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.Program
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
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
        nm <- arbitrary[Option[String]]
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
      in.name
    )}

}

object ArbProgramModel extends ArbProgramModel
