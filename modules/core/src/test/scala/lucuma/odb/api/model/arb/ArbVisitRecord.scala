// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant

import scala.collection.immutable.ListMap

trait ArbVisitRecord {

  import Helper._
  import ArbGid._
  import ArbStepRecord._
  import ArbTime._
  import ArbUid._

  implicit def arbVisitRecord[S: Arbitrary, D: Arbitrary]: Arbitrary[VisitRecord[S, D]] =
    Arbitrary {
      for {
        t <- arbitrary[Instant]
        s <- arbitrary[S]
        c <- smallSize
        l <- Gen.listOfN(c, arbitrary[(Step.Id, StepRecord[D])]).map(ListMap.from)
      } yield VisitRecord[S, D](t, s, l)
    }

  implicit def cogVisitRecord[S: Cogen, D: Cogen]: Cogen[VisitRecord[S, D]] =
    Cogen[(
      Instant,
      S,
      List[(Step.Id, StepRecord[D])]
    )].contramap { in => (
      in.created,
      in.static,
      in.steps.toList
    )}

  implicit def arbVisitRecordInput[SI: Arbitrary]: Arbitrary[VisitRecord.Input[SI]] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        s <- arbitrary[SI]
      } yield VisitRecord.Input(o, s)
    }

  implicit def cogVisitRecordInput[SI: Cogen]: Cogen[VisitRecord.Input[SI]] =
    Cogen[(
      Observation.Id,
      SI
    )].contramap { a => (
      a.observationId,
      a.static
    )}

}

object ArbVisitRecord extends ArbVisitRecord
