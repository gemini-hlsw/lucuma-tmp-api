// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.option._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.{PosDouble, PosInt}
import lucuma.core.`enum`.{ImageQuality, StepType}
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.GmosModel.NorthDynamic
import lucuma.odb.api.model.ScienceMode
import lucuma.odb.api.model.{AtomModel, StepModel}
import lucuma.odb.api.model.arb._
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scala.concurrent.duration._


final class GmosNorthLongSlitSuite extends ScalaCheckSuite {

  import ArbEnumerated._
  import ArbScienceMode._
  import ArbSourceProfile._

  val λ: Wavelength            = Wavelength.unsafeFromInt(500000)
  val acqTime: AcqExposureTime = AcqExposureTime.unsafeFrom(10.seconds)
  val sciTime: SciExposureTime = SciExposureTime.unsafeFrom( 5.minutes)
  val sampling: PosDouble      = 2.5
  val exposureCount: PosInt    = 100

  def longSlit(
    m:  ScienceMode.GmosNorthLongSlit,
    sp: SourceProfile,
    iq: ImageQuality
  ): GmosNorthLongSlit[IO] =
      GmosNorthLongSlit(
        m,
        λ,
        iq,
        sampling,
        sp,
        acqTime,
        sciTime,
        exposureCount
      )


  property("all atoms and steps have unique ids") {
    forAll { (mode: ScienceMode.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val seq = longSlit(mode, sp, iq).science(Nil)
      val ids = seq.unsafeRunSync().atoms.map(a => a.id.toUuid :: a.steps.toList.map(_.id.toUuid))

      assertEquals(ids.size, ids.distinct.size)
    }

  }

  property("acquisition stops after first atom") {
    forAll { (mode: ScienceMode.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val seq   = longSlit(mode, sp, iq).acquisition(Nil)
      val atoms = seq.unsafeRunSync().atoms
      assertEquals(atoms.size, 1)
    }
  }

  property("there is always a next acquisition atom") {

    forAll { (mode: ScienceMode.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val acq = GmosNorthLongSlit.Acquisition.compute(mode.fpu, acqTime, λ)
      val stp = List(
        RecordedStep(acq.ccd2, isExecuted = true),
        RecordedStep(acq.p10,  isExecuted = true),
        RecordedStep(acq.slit, isExecuted = true)
      )
      val seq   = longSlit(mode, sp, iq).acquisition(stp)
      val atoms = seq.unsafeRunSync().atoms.flatMap(_.steps.toList).map(_.config)
      assertEquals(atoms, List(acq.slit))
    }
  }

  implicit val ArbitraryGmosNorthScience: Arbitrary[List[AtomModel[StepModel[NorthDynamic]]]] =
    Arbitrary {
      for {
        m   <- arbitrary[ScienceMode.GmosNorthLongSlit]
        sp  <- arbitrary[SourceProfile]
        iq  <- arbitrary[ImageQuality]
      } yield longSlit(m, sp, iq).science(Nil).unsafeRunSync().atoms
    }

  property("science sequence atoms always consist of a flat and science") {
    forAll { (atoms: List[AtomModel[StepModel[NorthDynamic]]]) =>

      val obtained = atoms.map { a =>
        a.steps.toList.map { s =>
          s.config.science.as("S").orElse(s.config.gcal.as("F")).getOrElse("X")
        }
      }
      val expected = fs2.Stream(List("S", "F"), List("F", "S")).repeat.take(atoms.size.toLong).toList

      assertEquals(obtained, expected)
    }
  }

  property("science sequence is repeating ABBA") {
    forAll { (atoms: List[AtomModel[StepModel[NorthDynamic]]]) =>

      val obtained = atoms.flatMap { a =>
        a.steps.toList.flatMap { s =>
          s.config.science.map(_.offset.q.toAngle.toMicroarcseconds).toList
        }.map(_ === 0L)
      }
      val expected = fs2.Stream(true, false, false, true).repeat.take(atoms.size.toLong).toList

      assertEquals(obtained, expected)
    }
  }

  property("skip executed atoms") {

    forAll { (mode: ScienceMode.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>
      val sci   = GmosNorthLongSlit.Science.compute(mode, sciTime, λ, sp, iq, sampling)
      val stp   = sci.atom0.toList.map(c => RecordedStep(c, isExecuted = true))
      val seq   = longSlit(mode, sp, iq).science(stp)
      val atoms = seq.unsafeRunSync().atoms.flatMap(_.steps.toList).map(_.config)
      assertEquals(atoms.size, (exposureCount.value - 1) * 2)
    }
  }

  property("executing part of an atom is not executing it at all") {

    forAll { (mode: ScienceMode.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val sci   = GmosNorthLongSlit.Science.compute(mode, sciTime, λ, sp, iq, sampling)

      val steps =
        List.unfold(0)(i => if (i < exposureCount) (sci.atom(i), i+1).some else none)
          .map(nel => if (nel.head.stepType === StepType.Science) nel.head else nel.last)
          .map(sc => RecordedStep(sc, isExecuted = true))

      val seq   = longSlit(mode, sp, iq).science(steps)
      val atoms = seq.unsafeRunSync().atoms.flatMap(_.steps.toList).map(_.config)
      assertEquals(atoms.size, exposureCount.value * 2)
    }
  }

  property("executing all the steps stops the sequence") {

    forAll { (mode: ScienceMode.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val sci   = GmosNorthLongSlit.Science.compute(mode, sciTime, λ, sp, iq, sampling)
      val steps =
        List.unfold(0)(i => if (i < exposureCount) (sci.atom(i), i+1).some else none)
          .flatMap(nel => nel.toList)
          .map(sc => RecordedStep(sc, isExecuted = true))

      val seq   = longSlit(mode, sp, iq).science(steps)
      val atoms = seq.unsafeRunSync().atoms.flatMap(_.steps.toList).map(_.config)
      assert(atoms.isEmpty)
    }
  }

  property("non-contiguous steps do not make an atom") {

    forAll { (mode: ScienceMode.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val acq   = GmosNorthLongSlit.Acquisition.compute(mode.fpu, acqTime, λ)
      val sci   = GmosNorthLongSlit.Science.compute(mode, sciTime, λ, sp, iq, sampling)
      val steps =
        List.unfold(0)(i => if (i < exposureCount) (sci.atom(i), i+1).some else none)
          .flatMap(nel => nel.toList)
          .flatMap { sc => List(
            RecordedStep(sc,       isExecuted = true),
            RecordedStep(acq.ccd2, isExecuted = true)
          )}

      val seq   = longSlit(mode, sp, iq).science(steps)
      val atoms = seq.unsafeRunSync().atoms.flatMap(_.steps.toList).map(_.config)

      assertEquals(atoms.size, exposureCount.value * 2)
    }
  }
}
