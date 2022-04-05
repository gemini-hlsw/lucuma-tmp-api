// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.gen
package gmos
package longslit

import cats.effect.IO
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
import org.scalacheck.Arbitrary
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
  ): GmosNorthLongSlit[IO] =
      GmosNorthLongSlit(
        m,
        λ,
        iq,
        sampling,
        sp,
        acqTime,
        sciTime,
        100
      )


  property("all atoms and steps have unique ids") {
    forAll { (mode: Modes.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val seq = longSlit(mode, sp, iq).science(Nil)
      val ids = seq.unsafeRunSync().atoms.map(a => a.id.toUuid :: a.steps.toList.map(_.id.toUuid))

      assertEquals(ids.size, ids.distinct.size)
    }

  }

  property("acquisition stops after first atom") {
    forAll { (mode: Modes.GmosNorthLongSlit, sp: SourceProfile, iq: ImageQuality) =>

      val seq   = longSlit(mode, sp, iq).acquisition(Nil)
      val atoms = seq.unsafeRunSync().atoms
      assertEquals(atoms.size, 1)
    }
  }

  implicit val ArbitraryGmosNorthScience: Arbitrary[List[AtomModel[StepModel[NorthDynamic]]]] =
    Arbitrary {
      for {
        m   <- arbitrary[Modes.GmosNorthLongSlit]
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
}
