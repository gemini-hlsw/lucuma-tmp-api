// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.model.Step
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.arb.{ArbEnumerated, ArbGid}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbStepModel {

  import ArbEnumerated._
  import ArbGcalModel._
  import ArbGid._
  import ArbOffset._
  import ArbOffsetModel._

  implicit def arbBias[A: Arbitrary]: Arbitrary[StepConfig.Bias[A]] =
    Arbitrary {
      arbitrary[A].map(StepConfig.Bias(_))
    }

  implicit def cogBias[A: Cogen]: Cogen[StepConfig.Bias[A]] =
    Cogen[A].contramap(_.instrumentConfig)

  implicit def arbDark[A: Arbitrary]: Arbitrary[StepConfig.Dark[A]] =
    Arbitrary {
      arbitrary[A].map(StepConfig.Dark(_))
    }

  implicit def cogDark[A: Cogen]: Cogen[StepConfig.Dark[A]] =
    Cogen[A].contramap(_.instrumentConfig)

  implicit def arbGcal[A: Arbitrary]: Arbitrary[StepConfig.Gcal[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        g <- arbitrary[GcalModel]
      } yield StepConfig.Gcal(a, g)
    }

  implicit def cogGcal[A: Cogen]: Cogen[StepConfig.Gcal[A]] =
    Cogen[(A, GcalModel)].contramap { in => (
      in.instrumentConfig,
      in.gcalConfig
    )}

  implicit def arbScience[A: Arbitrary]: Arbitrary[StepConfig.Science[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        o <- arbitrary[Offset]
      } yield StepConfig.Science(a, o)
    }

  implicit def cogScience[A: Cogen]: Cogen[StepConfig.Science[A]] =
    Cogen[(A, Offset)].contramap { in => (
      in.instrumentConfig,
      in.offset
    )}

  implicit def arbStepConfig[A: Arbitrary]: Arbitrary[StepConfig[A]] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[StepConfig.Bias[A]],
        arbitrary[StepConfig.Dark[A]],
        arbitrary[StepConfig.Gcal[A]],
        arbitrary[StepConfig.Science[A]]
      )
    }

  implicit def cogStepConfig[A: Cogen]: Cogen[StepConfig[A]] =
    Cogen[(
      Option[StepConfig.Bias[A]],
      Option[StepConfig.Dark[A]],
      Option[StepConfig.Gcal[A]],
      Option[StepConfig.Science[A]]
    )].contramap { in => (in.bias, in.dark, in.gcal, in.science) }


  implicit def arbCreateBias[A: Arbitrary]: Arbitrary[StepConfig.CreateBias[A]] =
    Arbitrary {
      arbitrary[A].map(StepConfig.CreateBias(_))
    }

  implicit def cogCreateBias[A: Cogen]: Cogen[StepConfig.CreateBias[A]] =
    Cogen[A].contramap(_.config)

  implicit def arbCreateDark[A: Arbitrary]: Arbitrary[StepConfig.CreateDark[A]] =
    Arbitrary {
      arbitrary[A].map(StepConfig.CreateDark(_))
    }

  implicit def cogCreateDark[A: Cogen]: Cogen[StepConfig.CreateDark[A]] =
    Cogen[A].contramap(_.config)

  implicit def arbCreateGcal[A: Arbitrary]: Arbitrary[StepConfig.CreateGcal[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        g <- arbitrary[GcalModel.Create]
      } yield StepConfig.CreateGcal(a, g)
    }

  implicit def cogCreateGcal[A: Cogen]: Cogen[StepConfig.CreateGcal[A]] =
    Cogen[(A, GcalModel.Create)].contramap { in => (
      in.config,
      in.gcalConfig
    )}

  implicit def arbCreateScience[A: Arbitrary]: Arbitrary[StepConfig.CreateScience[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        o <- arbitrary[OffsetModel.Input]
      } yield StepConfig.CreateScience(a, o)
    }

  implicit def cogCreateScience[A: Cogen]: Cogen[StepConfig.CreateScience[A]] =
    Cogen[(A, OffsetModel.Input)].contramap { in => (
      in.config,
      in.offset
    )}

  implicit def arbCreateStepConfig[A: Arbitrary]: Arbitrary[StepConfig.CreateStepConfig[A]] =
    Arbitrary {
      Gen.oneOf(
        arbValidCreateStepConfig[A].arbitrary,
        arbitrary[(StepConfig.CreateGcal[A], StepConfig.CreateScience[A])].map { case (g, s) =>
          StepConfig.CreateStepConfig(None, None, Some(g), Some(s))  // invalid but possible input
        }
      )
    }

  def arbValidCreateStepConfig[A: Arbitrary]: Arbitrary[StepConfig.CreateStepConfig[A]] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[StepConfig.CreateBias[A]].map(   b => StepConfig.CreateStepConfig(Some(b), None, None, None)),
        arbitrary[StepConfig.CreateDark[A]].map(   d => StepConfig.CreateStepConfig(None, Some(d), None, None)),
        arbitrary[StepConfig.CreateGcal[A]].map(   g => StepConfig.CreateStepConfig(None, None, Some(g), None)),
        arbitrary[StepConfig.CreateScience[A]].map(s => StepConfig.CreateStepConfig(None, None, None, Some(s)))
      )
    }

  implicit def cogCreateStepConfig[A: Cogen]: Cogen[StepConfig.CreateStepConfig[A]] =
    Cogen[(
      Option[StepConfig.CreateBias[A]],
      Option[StepConfig.CreateDark[A]],
      Option[StepConfig.CreateGcal[A]],
      Option[StepConfig.CreateScience[A]]
    )].contramap { in => (
      in.bias,
      in.dark,
      in.gcal,
      in.science
    )}

  implicit def arbStepModel[A: Arbitrary]: Arbitrary[StepModel[A]] =
    Arbitrary {
      for {
        i <- arbitrary[Step.Id]
        b <- arbitrary[Breakpoint]
        s <- arbitrary[StepConfig[A]]
      } yield StepModel(i, b, s)
    }

  implicit def cogStepModel[A: Cogen]: Cogen[StepModel[A]] =
    Cogen[(Step.Id, Breakpoint, StepConfig[A])].contramap { in => (
      in.id,
      in.breakpoint,
      in.config
    )}

  implicit def arbStepModelCreate[A: Arbitrary]: Arbitrary[StepModel.Create[A]] =
    Arbitrary {
      for {
        i <- arbitrary[Option[Step.Id]]
        b <- arbitrary[Breakpoint]
        s <- arbitrary[StepConfig.CreateStepConfig[A]]
      } yield StepModel.Create(i, b, s)
    }

  def arbValidStepModelCreate[A: Arbitrary]: Arbitrary[StepModel.Create[A]] =
    Arbitrary {
      for {
        b <- arbitrary[Breakpoint]
        s <- arbValidCreateStepConfig[A].arbitrary
      } yield StepModel.Create(None, b, s)
    }


  implicit def cogStepModelCreate[A: Cogen]: Cogen[StepModel.Create[A]] =
    Cogen[(Option[Step.Id], Breakpoint, StepConfig.CreateStepConfig[A])].contramap { in => (
      in.id,
      in.breakpoint,
      in.config
    )}


}

object ArbStepModel extends ArbStepModel
