// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo
package arb

import lucuma.core.model.{Asterism, ConstraintSet, Observation, Program, Target}
import lucuma.odb.api.model.{
  AsterismModel,
  ConstraintSetModel,
  ObservationModel,
  ProgramModel,
  TargetModel
}
import lucuma.odb.api.model.arb._
import lucuma.core.util.Gid

import cats.Order
import cats.kernel.instances.order._
import cats.syntax.all._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.SortedMap

trait ArbTables extends SplitSetHelper {

  import ArbAsterismModel._
  import ArbConstraintSetModel._
  import ArbObservationModel._
  import ArbProgramModel._
  import ArbTargetModel._

  private def map[I: Gid, M: Arbitrary](updateId: (M, I) => M): Gen[SortedMap[I, M]] =
    arbitrary[List[M]]
      .map { lst =>
        SortedMap.from(
          lst
            .zipWithIndex
            .map { case (m, i) =>
              val gid = Gid[I].fromLong.getOption(i.toLong + 1).get
              (gid, updateId(m, gid))
            }
        )
      }

  private def mapAsterisms: Gen[SortedMap[Asterism.Id, AsterismModel]] =
    map[Asterism.Id, AsterismModel]((a, i) => a.copy(id = i))

  private def mapConstraintSets: Gen[SortedMap[ConstraintSet.Id, ConstraintSetModel]] =
    map[ConstraintSet.Id, ConstraintSetModel]((cs, i) => cs.copy(id = i))

  private def mapObservations(
    pids: List[Program.Id],
    aids: List[Asterism.Id],
    tids: List[Target.Id],
    cids: List[ConstraintSet.Id]
  ): Gen[SortedMap[Observation.Id, ObservationModel]] = {
    val emptyTargets = Option.empty[Either[Asterism.Id, Target.Id]]

    if (pids.isEmpty)
      Gen.const(SortedMap.empty[Observation.Id, ObservationModel])
    else
      for {
        om <- map[Observation.Id, ObservationModel]((o, i) => o.copy(id = i))
        ps <- Gen.listOfN(om.size, Gen.oneOf(pids))
        ts <- Gen.listOfN(
          om.size,
          Gen.oneOf(
            Gen.const(emptyTargets),
            if (aids.isEmpty) Gen.const(emptyTargets) else Gen.pick(1, aids).map(_.headOption.map(_.asLeft)),
            if (tids.isEmpty) Gen.const(emptyTargets) else Gen.pick(1, tids).map(_.headOption.map(_.asRight))
          )
        )
        cs <- Gen.listOfN(
          om.size,
          Gen.oneOf(
            Gen.const(Option.empty[ConstraintSet.Id]),
            if (cids.isEmpty) Gen.const(Option.empty[ConstraintSet.Id]) else Gen.pick(1, cids).map(_.headOption)
          )
        )
      } yield
        SortedMap.from(
          om.toList.zip(ps).zip(ts).zip(cs).map { case ((((i, o), pid), t), c) =>
            (i, o.copy(programId = pid, pointing = t, constraintSetId = c))
          }
        )
  }

  private def mapPrograms: Gen[SortedMap[Program.Id, ProgramModel]] =
    map[Program.Id, ProgramModel]((p, i) => p.copy(id = i))

  private def mapTargets: Gen[SortedMap[Target.Id, TargetModel]] =
    map[Target.Id, TargetModel]((t, i) => t.copy(id = i))

  private def lastGid[I: Gid](ms: SortedMap[I, _]): I =
    if (ms.isEmpty) Gid[I].minBound else ms.lastKey

  private def manyToMany[A: Order, B: Order](
    as: Iterable[A],
    bs: Iterable[B]
  ): Gen[ManyToMany[A, B]] =
    for {
      someAs <- Gen.someOf(as)
      someBs <- Gen.someOf(bs)
    } yield ManyToMany((for(a <- someAs; b <- someBs) yield (a, b)).toSeq: _*)

  implicit val arbTables: Arbitrary[Tables] =
    Arbitrary {
      for {
        ps <- mapPrograms
        as <- mapAsterisms
        cs <- mapConstraintSets
        ts <- mapTargets
        os <- mapObservations(ps.keys.toList, as.keys.toList, ts.keys.toList, cs.keys.toList)
        ids = Ids(
          0L,
          lastGid[Asterism.Id](as),
          lastGid[ConstraintSet.Id](cs),
          lastGid[Observation.Id](os),
          lastGid[Program.Id](ps),
          lastGid[Target.Id](ts)
        )
        pa <- manyToMany(ps.keys, as.keys)
        pt <- manyToMany(ps.keys, ts.keys)
        ta <- manyToMany(ts.keys, as.keys)
      } yield Tables(ids, as, cs, os, ps, ts, pa, pt, ta)
    }
}

object ArbTables extends ArbTables
