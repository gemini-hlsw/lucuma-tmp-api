// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo
package arb

import lucuma.core.model.{Asterism, Observation, Program, Target}
import lucuma.odb.api.model.{AsterismModel, ObservationModel, ProgramModel, TargetModel}
import lucuma.odb.api.model.arb._
import lucuma.core.util.Gid

import cats.Order
import cats.kernel.instances.order._
import cats.syntax.all._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.SortedMap


trait ArbTables {

  import ArbAsterismModel._
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
    map[Asterism.Id, AsterismModel]((a,i) => a.copy(id = i))

  private def mapObservations(
    pids: List[Program.Id],
    aids: List[Asterism.Id],
    tids: List[Target.Id]
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
      } yield
        SortedMap.from(
          om.toList.zip(ps).zip(ts).map { case (((i, o), pid), t) =>
            (i, o.copy(programId = pid, targets = t))
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
        ts <- mapTargets
        os <- mapObservations(ps.keys.toList, as.keys.toList, ts.keys.toList)
        ids = Ids(
          0L,
          lastGid[Asterism.Id](as),
          lastGid[Observation.Id](os),
          lastGid[Program.Id](ps),
          lastGid[Target.Id](ts)
        )
        pa <- manyToMany(ps.keys, as.keys)
        pt <- manyToMany(ps.keys, ts.keys)
        ta <- manyToMany(ts.keys, as.keys)
      } yield Tables(ids, as, os, ps, ts, pa, pt, ta)
    }

}

object ArbTables extends ArbTables
