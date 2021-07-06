// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

import lucuma.odb.api.model.{DeclinationModel, ProgramModel, RightAscensionModel, TargetModel}
import lucuma.odb.api.model.arb._
import lucuma.odb.api.repo.arb._
import lucuma.core.model.{Program, Target}

import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite

final class TargetRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  import ArbDeclinationModel._
  import ArbRightAscensionModel._
  import ArbTables._

  test("shareWithPrograms") {

    sharingTest[Target.Id, TargetModel, Program.Id, ProgramModel] { odb => (
        odb.target,
        odb.program,
        odb.target.shareWithPrograms,
        odb.target.unshareWithPrograms,
        odb.program.selectPageForTarget(_, includeObservations = false)
    )}

  }

  property("simple create") {

    forAll { (tables: Tables, name: NonEmptyString, ra: RightAscensionModel.Input, dec: DeclinationModel.Input) =>

      val target = runTest(tables) { odb =>
        odb.target.insertSidereal(TargetModel.CreateSidereal.fromRaDec(name, ra, dec))
      }

      assertEquals(target.target.name, name)
      assertEquals(target.target.track.map(_.baseCoordinates.ra).toOption, ra.toRightAscension.toOption)
      assertEquals(target.target.track.map(_.baseCoordinates.dec).toOption, dec.toDeclination.toOption)

    }

  }

  property("simple edit") {

    forAll { (tables: Tables, name: NonEmptyString) =>

      val target = runTest(tables) { odb =>

        val edit = tables.targets.values.headOption.filter(_.target.track.isRight).map { t =>
          TargetModel.EditSidereal(t.id, name = Input(name), magnitudes = None, modifyMagnitudes = None, deleteMagnitudes = None)
        }

        edit.traverse(e => odb.target.edit(e.id, e.editor))
      }

      assert(target.forall(_.target.name == name))
    }

  }


}
