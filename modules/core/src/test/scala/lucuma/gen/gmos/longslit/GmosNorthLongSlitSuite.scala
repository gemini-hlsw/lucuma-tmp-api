// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.eq._
import cats.syntax.functor._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.all.PosDouble
import lucuma.core.`enum`.ImageQuality
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.api.model.GmosModel.NorthDynamic
import lucuma.odb.api.model.ScienceConfigurationModel.Modes
import lucuma.odb.api.model.{AtomModel, StepModel}
import lucuma.odb.api.model.arb._
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scala.concurrent.duration._


final class GmosNorthLongSlitSuite extends ScalaCheckSuite {

  import ArbEnumerated._
  import ArbScienceConfigurationModel._
  import ArbSourceProfile._

  val λ: Wavelength            = Wavelength.unsafeFromInt(500000)
  val acqTime: AcqExposureTime = AcqExposureTime.unsafeFrom(10.seconds)
  val sciTime: SciExposureTime = SciExposureTime.unsafeFrom( 5.minutes)
  val sampling: PosDouble      = 2.5

  def longSlit(
    m:  Modes.GmosNorthLongSlit,
    sp: SourceProfile,
    iq: ImageQuality
  ): GmosNorthLongSlit =
      GmosNorthLongSlit(
        m,
        acqTime,
        sciTime,
        λ,
        sp,
        iq,
        sampling
      )


  property("all atoms and steps have unique ids") {
    forAll { (mode: Modes.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val seq = longSlit(mode, sp, iq).generate.science(IO.pure(false))
      val ids = seq.take(100).compile.toList.unsafeRunSync().flatMap(a => a.id.toUuid :: a.steps.toList.map(_.id.toUuid))

      assertEquals(ids.size, ids.distinct.size)
    }

  }

  property("acquisition stops after first atom when already acquired") {
    forAll { (mode: Modes.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val seq   = longSlit(mode, sp, iq).generate.acquisition(IO.pure(true))
      val atoms = seq.compile.toList.unsafeRunSync()
      assertEquals(atoms.size, 1)
    }
  }

  implicit val ArbitraryGmosNorthScience: Arbitrary[List[AtomModel[StepModel[NorthDynamic]]]] =
    Arbitrary {
      for {
        m   <- arbitrary[Modes.GmosNorthLongSlit]
        sp  <- arbitrary[SourceProfile]
        iq  <- arbitrary[ImageQuality]
        cnt <- Gen.posNum[Int]
        max     = (cnt % 100).abs
        counter = Ref.unsafe[IO, Int](0)
      } yield longSlit(m, sp, iq)
                .generate
                .science(counter.getAndUpdate(_ + 1).map(_ >= max))
                .compile
                .toList
                .unsafeRunSync()
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
}
