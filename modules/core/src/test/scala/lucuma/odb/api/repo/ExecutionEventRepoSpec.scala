// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

//import lucuma.odb.api.model.ObservationModel

//import cats.syntax.all._
//import cats.kernel.instances.order._
//import clue.data.Input
//import eu.timepit.refined.types.numeric.PosLong
//import eu.timepit.refined.types.string.NonEmptyString
//import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import munit.ScalaCheckSuite

final class ExecutionEventRepoSpec extends ScalaCheckSuite with OdbRepoTest {

  import arb.ArbTables._

  val genTables: Gen[Tables] = arbTablesWithSequencesAndEvents.arbitrary

  property("placeholder") {

    forAll(genTables) { (t: Tables) =>
      t.observations.forall { case (_, o) => println(o.config.map(_.science.atoms.length)); true }
    }

  }


}
