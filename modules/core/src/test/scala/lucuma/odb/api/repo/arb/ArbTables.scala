// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo
package arb

import lucuma.core.model.{Asterism, Atom, ExecutionEvent, Observation, Program, Step, Target}
import lucuma.odb.api.model.{AsterismModel, AtomModel, ExecutionEventModel, InstrumentConfigModel, ObservationModel, ProgramModel, StepModel, TargetModel}
import lucuma.odb.api.model.arb._
import lucuma.core.util.Gid
import cats.Order
import cats.data.{Nested, State}
import cats.kernel.instances.order._
import cats.syntax.all._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.SortedMap

trait ArbTables extends SplitSetHelper {

  import ArbAsterismModel._
  import ArbInstrumentConfigModel._
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
            (i, o.copy(programId = pid, pointing = t))
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

  val initArbTables: Arbitrary[Tables] =
    Arbitrary {
      for {
        ps <- mapPrograms
        as <- mapAsterisms
        ts <- mapTargets
        os <- mapObservations(ps.keys.toList, as.keys.toList, ts.keys.toList)
        ids = Ids(
          0L,
          lastGid[Asterism.Id](as),
          lastGid[Atom.Id](SortedMap.empty[Atom.Id, AtomModel[_]]),
          lastGid[ExecutionEvent.Id](SortedMap.empty[ExecutionEvent.Id, ExecutionEventModel]),
          lastGid[Observation.Id](os),
          lastGid[Program.Id](ps),
          lastGid[Step.Id](SortedMap.empty[Step.Id, StepModel[_]]),
          lastGid[Target.Id](ts)
        )
        pa <- manyToMany(ps.keys, as.keys)
        pt <- manyToMany(ps.keys, ts.keys)
        ta <- manyToMany(ts.keys, as.keys)
      } yield Tables(ids, SortedMap.empty, as, SortedMap.empty, os, ps, SortedMap.empty, ts, pa, pt, ta)
    }

  implicit val arbTables: Arbitrary[Tables] =
    Arbitrary {
      for {
        t <- initArbTables.arbitrary
        c <- Gen.listOfN[Option[InstrumentConfigModel.Create]](t.observations.size, Gen.option(arbValidInstrumentConfigModelCreate.arbitrary))
      } yield {

        // Create an option random sequence for each observation.
        val (tʹ, a) = Nested(c).traverse(_.create[State[Tables, *], Tables](TableState)).run(t).value
        val icms    = a.value.map(_.flatMap(_.toOption))

        // Update the observations to contain the random sequence.
        Tables.observations.modify { obsMap =>
          val icmMap = obsMap.keys.zip(icms).toMap
          obsMap.transform((id, o) => o.copy(config = icmMap.get(id).flatten.map(_.toReference)))
        }(tʹ)

      }
    }

}

object ArbTables extends ArbTables
