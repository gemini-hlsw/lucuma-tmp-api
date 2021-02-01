// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbStepModel {

  import ArbGcalModel._
  import ArbOffset._
  import ArbOffsetModel._

  implicit def arbBias[A: Arbitrary]: Arbitrary[StepModel.Bias[A]] =
    Arbitrary {
      arbitrary[A].map(StepModel.Bias(_))
    }

  implicit def cogBias[A: Cogen]: Cogen[StepModel.Bias[A]] =
    Cogen[A].contramap(_.dynamicConfig)

  implicit def arbDark[A: Arbitrary]: Arbitrary[StepModel.Dark[A]] =
    Arbitrary {
      arbitrary[A].map(StepModel.Dark(_))
    }

  implicit def cogDark[A: Cogen]: Cogen[StepModel.Dark[A]] =
    Cogen[A].contramap(_.dynamicConfig)

  implicit def arbGcal[A: Arbitrary]: Arbitrary[StepModel.Gcal[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        g <- arbitrary[GcalModel]
      } yield StepModel.Gcal(a, g)
    }

  implicit def cogGcal[A: Cogen]: Cogen[StepModel.Gcal[A]] =
    Cogen[(A, GcalModel)].contramap { in => (
      in.dynamicConfig,
      in.gcalConfig
    )}

  implicit def arbScience[A: Arbitrary]: Arbitrary[StepModel.Science[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        o <- arbitrary[Offset]
      } yield StepModel.Science(a, o)
    }

  implicit def cogScience[A: Cogen]: Cogen[StepModel.Science[A]] =
    Cogen[(A, Offset)].contramap { in => (
      in.dynamicConfig,
      in.offset
    )}

  implicit def arbStep[A: Arbitrary]: Arbitrary[StepModel[A]] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[StepModel.Bias[A]],
        arbitrary[StepModel.Dark[A]],
        arbitrary[StepModel.Gcal[A]],
        arbitrary[StepModel.Science[A]]
      )
    }

  implicit def cogStep[A: Cogen]: Cogen[StepModel[A]] =
    Cogen[(
      Option[StepModel.Bias[A]],
      Option[StepModel.Dark[A]],
      Option[StepModel.Gcal[A]],
      Option[StepModel.Science[A]]
    )].contramap { in => (in.bias, in.dark, in.gcal, in.science) }


  implicit def arbCreateBias[A: Arbitrary]: Arbitrary[StepModel.CreateBias[A]] =
    Arbitrary {
      arbitrary[A].map(StepModel.CreateBias(_))
    }

  implicit def cogCreateBias[A: Cogen]: Cogen[StepModel.CreateBias[A]] =
    Cogen[A].contramap(_.config)

  implicit def arbCreateDark[A: Arbitrary]: Arbitrary[StepModel.CreateDark[A]] =
    Arbitrary {
      arbitrary[A].map(StepModel.CreateDark(_))
    }

  implicit def cogCreateDark[A: Cogen]: Cogen[StepModel.CreateDark[A]] =
    Cogen[A].contramap(_.config)

  implicit def arbCreateGcal[A: Arbitrary]: Arbitrary[StepModel.CreateGcal[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        g <- arbitrary[GcalModel.Create]
      } yield StepModel.CreateGcal(a, g)
    }

  implicit def cogCreateGcal[A: Cogen]: Cogen[StepModel.CreateGcal[A]] =
    Cogen[(A, GcalModel.Create)].contramap { in => (
      in.config,
      in.gcalConfig
    )}

  implicit def arbCreateScience[A: Arbitrary]: Arbitrary[StepModel.CreateScience[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        o <- arbitrary[OffsetModel.Input]
      } yield StepModel.CreateScience(a, o)
    }

  implicit def cogCreateScience[A: Cogen]: Cogen[StepModel.CreateScience[A]] =
    Cogen[(A, OffsetModel.Input)].contramap { in => (
      in.config,
      in.offset
    )}

  implicit def arbCreateStep[A: Arbitrary]: Arbitrary[StepModel.CreateStep[A]] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[StepModel.CreateBias[A]].map(   b => StepModel.CreateStep(Some(b), None, None, None)),
        arbitrary[StepModel.CreateDark[A]].map(   d => StepModel.CreateStep(None, Some(d), None, None)),
        arbitrary[StepModel.CreateGcal[A]].map(   g => StepModel.CreateStep(None, None, Some(g), None)),
        arbitrary[StepModel.CreateScience[A]].map(s => StepModel.CreateStep(None, None, None, Some(s)))
      )
    }

  implicit def cogCreateStep[A: Cogen]: Cogen[StepModel.CreateStep[A]] =
    Cogen[(
      Option[StepModel.CreateBias[A]],
      Option[StepModel.CreateDark[A]],
      Option[StepModel.CreateGcal[A]],
      Option[StepModel.CreateScience[A]]
    )].contramap { in => (
      in.bias,
      in.dark,
      in.gcal,
      in.science
    )}

}

object ArbStepModel extends ArbStepModel
