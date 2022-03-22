// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.ListMap

trait ArbVisitRecords {

  import ArbGmosModel._
  import ArbUid._
  import ArbVisitRecord._
  import Helper._

  def arbListMapVisitRecords[S: Arbitrary, D: Arbitrary]: Arbitrary[ListMap[Visit.Id, VisitRecord[S, D]]] =
    Arbitrary {
      for {
        c    <- tinySize
        vids <- Gen.listOfN(c, arbitrary[Visit.Id])
        vrs  <- Gen.listOfN(vids.length, arbitrary[VisitRecord[S, D]])
      } yield ListMap.from(vids.zip(vrs))
    }

  implicit val arbVisitRecords: Arbitrary[VisitRecords] =
    Arbitrary {
      Gen.oneOf(
        arbListMapVisitRecords[GmosModel.NorthStatic, GmosModel.NorthDynamic].arbitrary.map(VisitRecords.GmosNorth(_)),
        arbListMapVisitRecords[GmosModel.SouthStatic, GmosModel.SouthDynamic].arbitrary.map(VisitRecords.GmosSouth(_)),
      )
    }

  def cogListMapVisitRecords[S: Cogen, D: Cogen]: Cogen[ListMap[Visit.Id, VisitRecord[S, D]]] =
    Cogen[List[(Visit.Id, VisitRecord[S, D])]].contramap(_.toList)

  implicit val cogGmosNorthVisitRecords: Cogen[VisitRecords.GmosNorth] =
    cogListMapVisitRecords[GmosModel.NorthStatic, GmosModel.NorthDynamic].contramap { in =>
      in.visits
    }

  implicit val cogGmosSouthVisitRecords: Cogen[VisitRecords.GmosSouth] =
    cogListMapVisitRecords[GmosModel.SouthStatic, GmosModel.SouthDynamic].contramap { in =>
      in.visits
    }

  implicit val cogVisitRecords: Cogen[VisitRecords] = {
    Cogen[(
      Option[VisitRecords.GmosNorth],
      Option[VisitRecords.GmosSouth]
    )].contramap { in => (
      VisitRecords.gmosNorthVisits.getOption(in).map(VisitRecords.GmosNorth(_)),
      VisitRecords.gmosSouthVisits.getOption(in).map(VisitRecords.GmosSouth(_))
    )}
  }

}

object ArbVisitRecords extends ArbVisitRecords
