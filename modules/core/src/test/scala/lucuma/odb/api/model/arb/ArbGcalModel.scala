// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import lucuma.core.util.arb.ArbEnumerated

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.concurrent.duration.FiniteDuration

trait ArbGcalModel {

  import ArbCoAddsModel._
  import ArbEnumerated._
  import ArbFiniteDurationModel._

  implicit val arbGcalModelArcs: Arbitrary[GcalModel.Arcs] =
    Arbitrary {
      for {
        a  <- arbitrary[GcalArc]
        as <- arbitrary[List[GcalArc]]
      } yield GcalModel.Arcs(a, as)
    }

  implicit val cogGcalModelArcs: Cogen[GcalModel.Arcs] =
    Cogen[List[GcalArc]].contramap(_.toList)

  implicit val arbGcalModel: Arbitrary[GcalModel] =
    Arbitrary {
      for {
        l <- arbitrary[GcalModel.Lamp]
        f <- arbitrary[GcalFilter]
        d <- arbitrary[GcalDiffuser]
        s <- arbitrary[GcalShutter]
        e <- arbitrary[FiniteDuration]
        c <- arbitrary[CoAddsModel]
      } yield GcalModel(l, f, d, s, e, c)
    }

  implicit val cogGcalModel: Cogen[GcalModel] =
    Cogen[(
      GcalModel.Lamp,
      GcalFilter,
      GcalDiffuser,
      GcalShutter,
      FiniteDuration,
      CoAddsModel
    )].contramap { in => (
      in.lamp,
      in.filter,
      in.diffuser,
      in.shutter,
      in.exposureTime,
      in.coadds
    )}

  implicit val arbGcalModelCreate: Arbitrary[GcalModel.Create] =
    Arbitrary {
      for {
        l <- arbitrary[GcalModel.Lamp]
        f <- arbitrary[GcalFilter]
        d <- arbitrary[GcalDiffuser]
        s <- arbitrary[GcalShutter]
        e <- arbitrary[FiniteDurationModel.Input]
        c <- arbitrary[CoAddsModel.Input]
      } yield GcalModel.Create(l.swap.toOption, l.toOption.toList.flatMap(_.toList), f, d, s, e, c)
    }

  implicit val cogGcalModelCreate: Cogen[GcalModel.Create] =
    Cogen[(
      Option[GcalContinuum],
      List[GcalArc],
      GcalFilter,
      GcalDiffuser,
      GcalShutter,
      FiniteDurationModel.Input,
      CoAddsModel.Input
    )].contramap { in => (
      in.continuum,
      in.arcs,
      in.filter,
      in.diffuser,
      in.shutter,
      in.exposureTime,
      in.coAdds
    )}
}

object ArbGcalModel extends ArbGcalModel
