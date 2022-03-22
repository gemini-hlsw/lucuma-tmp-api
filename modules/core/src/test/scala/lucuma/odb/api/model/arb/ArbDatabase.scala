// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import cats.data.{Nested, StateT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.kernel.instances.order._
import cats.syntax.all._
import lucuma.core.arb.ArbTime
import lucuma.core.model.{ExecutionEvent, Observation, Program, Target}
import lucuma.core.util.Gid
import lucuma.odb.api.model.SequenceModel.SequenceType.Science
import lucuma.odb.api.model.targetModel.{TargetEnvironmentModel, TargetModel}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._
import org.scalacheck.cats.implicits._

import java.time.Instant

import scala.collection.immutable.{ListMap, SortedMap, SortedSet}

trait ArbDatabase extends SplitSetHelper {

  import ArbExecutionEventModel._
  import ArbInstrumentConfigModel._
  import ArbObservationModel._
  import ArbProgramModel._
  import ArbTargetModel._
  import ArbVisitRecords._
  import ArbTime._

  private def map[I: Gid, M: Arbitrary](updateId: (M, I) => M): Gen[SortedMap[I, M]] =
    for {
      c <- Gen.chooseNum(0, 10)
      l <- Gen.listOfN(c, arbitrary[M])
    } yield
        SortedMap.from(
          l.zipWithIndex
           .map { case (m, i) =>
             val gid = Gid[I].fromLong.getOption(i.toLong + 1).get
             (gid, updateId(m, gid))
           }
        )

  private def mapWithValidPid[I: Gid, M: Arbitrary](
    pids: List[Program.Id],
    updateId: (M, I) => M,
    updatePid: (M, Program.Id) => M
  ): Gen[SortedMap[I, M]] =

    if (pids.isEmpty)
      Gen.const(SortedMap.empty[I, M])
    else
      for {
        ms <- map[I, M](updateId)
        ps <- Gen.listOfN(ms.size, Gen.oneOf(pids))
      } yield
        SortedMap.from(
          ms.toList.zip(ps).map { case ((i, m), pid) => (i, updatePid(m, pid)) }
        )

  private def mapObservations(
    pids: List[Program.Id],
    ts:   SortedMap[Target.Id, TargetModel]
  ): Gen[SortedMap[Observation.Id, ObservationModel]] = {

    // valid targets for each program id
    val validTids: Map[Program.Id, List[Target.Id]] =
      ts.values.toList.groupBy(_.programId).view.mapValues(_.map(_.id)).toMap

    for {
      // observation map where every observation references a valid program
      m <- mapWithValidPid[Observation.Id, ObservationModel](
             pids,
             (m, i)   => m.copy(id = i),
             (m, pid) => m.copy(programId = pid)
           )

      // observations where target asterisms refer to valid existing targets
      // for the program
      os <- m.values.toList.traverse { om =>
              Gen.someOf(validTids.getOrElse(om.programId, List.empty))
                  .map { ts =>
                    ObservationModel.targetEnvironment
                      .andThen(TargetEnvironmentModel.asterism)
                      .replace(SortedSet.from(ts))(om)
                  }
            }

    } yield SortedMap.from(os.fproductLeft(_.id))

  }

  private def mapPrograms: Gen[SortedMap[Program.Id, ProgramModel]] =
    map[Program.Id, ProgramModel]((p, i) => p.copy(id = i))

  private def mapTargets(
    pids: List[Program.Id]
  ): Gen[SortedMap[Target.Id, TargetModel]] =
    mapWithValidPid[Target.Id, TargetModel](
      pids,
      (m, i)   => m.copy(id = i),
      (m, pid) => m.copy(programId = pid)
    )

  private def table[K: Gid, V](ms: SortedMap[K, V]): Table[K, V] =
    Table(if (ms.isEmpty) Gid[K].minBound else ms.lastKey, ms)

  implicit val arbDatabase: Arbitrary[Database] =
    Arbitrary {
      for {
        ps <- mapPrograms
        ts <- mapTargets(ps.keys.toList)
        os <- mapObservations(ps.keys.toList, ts)
        vs <- Gen.listOfN(os.size, arbitrary[VisitRecords])
      } yield Database(
        0L,
        SortedMap.from(os.keys.zip(vs)),
        table(SortedMap.empty[ExecutionEvent.Id, ExecutionEventModel]),
        table(os),
        table(ps),
        table(ts)
      )
    }

  /**
   * Arbitrary tables with sequences is slow and since sequences are not always
   * needed for testing, I've made it not implicit.
   */
  val arbDatabaseWithSequences: Arbitrary[Database] = {

    def dbWithSequences(db: Database, c: List[Option[InstrumentConfigModel.Create]]): Database = {
      // Create an option random sequence for each observation.
      val update =
        for {
          icms <- StateT.liftF(Nested(c).traverse(_.create[IO]).unsafeRunSync().sequence.map(_.value).toEither)
          _    <- db.observations.rows.toList.zip(icms).traverse { case ((oid, o), icm) =>
            Database.observation.update(oid, ObservationModel.config.replace(icm)(o))
          }
      } yield ()

      update.runS(db).getOrElse(db)
    }

    Arbitrary {
      for {
        d <- arbDatabase.arbitrary
        c <- Gen.listOfN[Option[InstrumentConfigModel.Create]](
               d.observations.rows.size,
               Gen.option(arbValidInstrumentConfigModelCreate.arbitrary)
             )
      } yield dbWithSequences(d, c)
    }
  }

  val arbDatabaseWithSequencesAndEvents: Arbitrary[Database] = {

    def addEvents[S, D](
      oid: Observation.Id,
      visits: ListMap[Visit.Id, VisitRecord[S, D]]
    ): Gen[StateT[EitherInput, Database, Unit]] =

      if (visits.keys.isEmpty)
        Gen.const(StateT.pure[EitherInput, Database, Unit](()))
      else
        for {
          vid           <- Gen.oneOf(visits.keys)

          seqCnt        <- smallSize
          seqEvents     <- Gen.listOfN(seqCnt, arbSequenceEventAdd(oid, vid).arbitrary)

          sidsSize      <- tinyPositiveSize
          sids           = visits(vid).steps.keys.take(sidsSize).toList

          stpCnts       <- sids.traverse(tinySize.tupleRight)
          stpEvents     <- stpCnts.flatTraverse { case (cnt, sid) => Gen.listOfN(cnt, arbStepEventAdd(oid, vid, sid, Science).arbitrary) }

          dstCnts       <- sids.traverse(tinySize.tupleRight)
          dstEvents     <- dstCnts.flatTraverse { case (cnt, sid) => Gen.listOfN(cnt, arbDatasetEventAdd(oid, vid, sid).arbitrary) }

          received      <- arbitrary[Instant]

        } yield
          for {
          _ <- seqEvents.traverse(_.add(received)).void
          _ <- stpEvents.traverse(_.add(received)).void
          _ <- dstEvents.traverse(_.add(received)).void
          } yield ()

    Arbitrary {
      for {
        db <- arbDatabaseWithSequences.arbitrary
        e  <- db.observations.rows.values.toList.traverse { o =>
          db.visitRecords.get(o.id) match {
            case Some(VisitRecords.GmosNorth(vs)) => addEvents(o.id, vs)
            case Some(VisitRecords.GmosSouth(vs)) => addEvents(o.id, vs)
            case _                                => Gen.const(StateT.pure[EitherInput, Database, Unit](()))
          }
        }
      } yield e.sequence.runS(db).getOrElse(db)
    }
  }
}

object ArbDatabase extends ArbDatabase
