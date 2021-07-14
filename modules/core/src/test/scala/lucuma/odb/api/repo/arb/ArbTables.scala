// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.model.{Atom, ExecutionEvent, Observation, Program, Step}
import lucuma.odb.api.model.{AtomModel, ExecutionEventModel, InstrumentConfigModel, ObservationModel, ProgramModel, StepModel}
import lucuma.core.util.Gid
import lucuma.odb.api.model.SequenceModel.SequenceType.{Acquisition, Science}
import lucuma.odb.api.model.arb._

import cats.data.{Nested, State}
import cats.kernel.instances.order._
import cats.syntax.all._
import org.scalacheck._
import org.scalacheck.cats.implicits._
import org.scalacheck.Arbitrary.arbitrary

import java.time.Instant

import scala.collection.immutable.SortedMap

trait ArbTables extends SplitSetHelper {

  import ArbExecutionEventModel._
  import ArbInstrumentConfigModel._
  import ArbObservationModel._
  import ArbProgramModel._
  import ArbTime._

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

  private def mapObservations(
    pids: List[Program.Id]
  ): Gen[SortedMap[Observation.Id, ObservationModel]] = {

    if (pids.isEmpty)
      Gen.const(SortedMap.empty[Observation.Id, ObservationModel])
    else
      for {
        om <- map[Observation.Id, ObservationModel]((o, i) => o.copy(id = i))
        ps <- Gen.listOfN(om.size, Gen.oneOf(pids))
      } yield
        SortedMap.from(
          om.toList.zip(ps).map { case ((i, o), pid) =>
            (i, o.copy(programId = pid))
          }
        )
  }

  private def mapPrograms: Gen[SortedMap[Program.Id, ProgramModel]] =
    map[Program.Id, ProgramModel]((p, i) => p.copy(id = i))

  private def lastGid[I: Gid](ms: SortedMap[I, _]): I =
    if (ms.isEmpty) Gid[I].minBound else ms.lastKey

  implicit val arbTables: Arbitrary[Tables] =
    Arbitrary {
      for {
        ps <- mapPrograms
        os <- mapObservations(ps.keys.toList)
        ids = Ids(
          0L,
          lastGid[Atom.Id](SortedMap.empty[Atom.Id, AtomModel[_]]),
          lastGid[ExecutionEvent.Id](SortedMap.empty[ExecutionEvent.Id, ExecutionEventModel]),
          lastGid[Observation.Id](os),
          lastGid[Program.Id](ps),
          lastGid[Step.Id](SortedMap.empty[Step.Id, StepModel[_]])
        )
      } yield Tables(ids, SortedMap.empty, SortedMap.empty, os, ps, SortedMap.empty)
    }

  /**
   * Arbitrary tables with sequences is slow and since sequences are not always
   * needed for testing, I've made it not implicit.
   */
  val arbTablesWithSequences: Arbitrary[Tables] = {

    def tablesWithSequences(t: Tables, c: List[Option[InstrumentConfigModel.Create]]): Tables = {
        // Create an option random sequence for each observation.
        val (tʹ, a) = Nested(c).traverse(_.create[State[Tables, *], Tables](TableState)).run(t).value
        val icms    = a.value.map(_.flatMap(_.toOption))

        // Update the observations to contain the random sequence.
        Tables.observations.modify { obsMap =>
          val icmMap = obsMap.keys.zip(icms).toMap
          obsMap.transform((id, o) => o.copy(config = icmMap.get(id).flatten.map(_.toReference)))
        }(tʹ)
    }

    Arbitrary {
      for {
        t <- arbTables.arbitrary
        c <- Gen.listOfN[Option[InstrumentConfigModel.Create]](
               t.observations.size,
               Gen.option(arbValidInstrumentConfigModelCreate.arbitrary)
             )
      } yield tablesWithSequences(t, c)
    }
  }

  val arbTablesWithSequencesAndEvents: Arbitrary[Tables] = {

    def addEventsForObservation(t: Tables)(o: ObservationModel): Gen[State[Tables, Unit]] = {
      val acqAtoms = o.config.toList.flatMap(_.acquisition.atoms)
      val sciAtoms = o.config.toList.flatMap(_.science.atoms)
      val acqSteps = acqAtoms.flatMap(aid => t.atoms(aid).steps.toList)
      val sciSteps = sciAtoms.flatMap(aid => t.atoms(aid).steps.toList)

      for {
        seqCnt        <- smallSize
        seqEvents     <- Gen.listOfN(seqCnt, arbSequenceEventAdd(o.id).arbitrary)

        acqIdSize     <- tinyPositiveSize
        acqIds        <- Gen.someOf[Step.Id](acqSteps).map(_.toList.take(acqIdSize))

        sciIdSize     <- tinyPositiveSize
        sciIds        <- Gen.someOf[Step.Id](sciSteps).map(_.toList.take(sciIdSize))

        acqCnts       <- acqIds.traverse(sid => tinySize.map(i => (i, sid)))
        acqStepEvents <- acqCnts.flatTraverse { case (cnt, sid) => Gen.listOfN(cnt, arbStepEventAdd(o.id, sid, Acquisition).arbitrary) }

        sciCnts       <- sciIds.traverse(sid => tinySize.map(i => (i, sid)))
        sciStepEvents <- sciCnts.flatTraverse { case (cnt, sid) => Gen.listOfN(cnt, arbStepEventAdd(o.id, sid, Science).arbitrary) }

        dstCnts       <- (acqIds ++ sciIds).traverse(sid => tinySize.map(i => (i, sid)))
        dstEvents     <- dstCnts.flatTraverse { case (cnt, sid) => Gen.listOfN(cnt, arbDatasetEventAdd(o.id, sid).arbitrary) }

        received      <- arbitrary[Instant]
      } yield
        for {
          _ <- seqEvents.traverse(_.add[State[Tables, *], Tables](TableState, received)).void
          _ <- acqStepEvents.traverse(_.add[State[Tables, *], Tables](TableState, received)).void
          _ <- sciStepEvents.traverse(_.add[State[Tables, *], Tables](TableState, received)).void
          _ <- dstEvents.traverse(_.add[State[Tables, *], Tables](TableState, received)).void
        } yield ()
    }

    Arbitrary {
      for {
        t <- arbTablesWithSequences.arbitrary
        add = addEventsForObservation(t)(_)
        e <- t.observations.values.toList.traverse(add).map(_.sequence_)
      } yield e.runS(t).value
    }
  }
}

object ArbTables extends ArbTables
