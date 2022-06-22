// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.eq._
import cats.syntax.option._
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.odb.api.model.syntax.lens._
import monocle.Prism

import scala.collection.immutable.ListMap

sealed trait VisitRecords extends Product with Serializable {

  def instrument: Instrument

  def visits: ListMap[Visit.Id, VisitRecord[_, _]]

}


object VisitRecords {

  final case class GmosNorth(
    visits: ListMap[Visit.Id, VisitRecord[GmosModel.NorthStatic, GmosModel.NorthDynamic]]
  ) extends VisitRecords {

    def instrument: Instrument =
      Instrument.GmosNorth

  }

  object GmosNorth {

    implicit val EqGmosNorth: Eq[GmosNorth] =
      Eq.by(_.visits.toList)

  }

  final case class GmosSouth(
    visits: ListMap[Visit.Id, VisitRecord[GmosModel.SouthStatic, GmosModel.SouthDynamic]]
  ) extends VisitRecords {

    def instrument: Instrument =
      Instrument.GmosSouth

  }

  object GmosSouth {

    implicit val EqGmosSouth: Eq[GmosSouth] =
      Eq.by(_.visits.toList)

  }

  implicit val EqVisitRecords: Eq[VisitRecords] =
    Eq.instance {
      case (g1@GmosNorth(_), g2@GmosNorth(_)) => g1 === g2
      case (g1@GmosSouth(_), g2@GmosSouth(_)) => g1 === g2
      case _                                  => false
    }

  private def lookup[S, D](
    p: Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]],
    vr: VisitRecords,
    vid: Visit.Id
  ): Option[VisitRecord[S, D]] =
    p.getOption(vr).flatMap(_.get(vid))

  private def list[S, D](
    p: Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[S, D]]],
    vr: VisitRecords
  ): List[(Visit.Id, VisitRecord[S, D])] =
    p.getOption(vr).toList.flatMap(_.toList)

  val gmosNorthVisits: Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.NorthStatic, GmosModel.NorthDynamic]]] =
    Prism.partial[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.NorthStatic, GmosModel.NorthDynamic]]] {
      case GmosNorth(vs) => vs
    }(GmosNorth(_))

  val lookupGmosNorthVisit: (VisitRecords, Visit.Id) => Option[VisitRecord[GmosModel.NorthStatic, GmosModel.NorthDynamic]] =
    lookup(gmosNorthVisits, _, _)

  val listGmosNorthVisits: VisitRecords => List[(Visit.Id, VisitRecord[GmosModel.NorthStatic, GmosModel.NorthDynamic])] =
    list(gmosNorthVisits, _)

  val gmosSouthVisits: Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.SouthStatic, GmosModel.SouthDynamic]]] =
    Prism.partial[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.SouthStatic, GmosModel.SouthDynamic]]] {
      case GmosSouth(vs) => vs
    }(GmosSouth(_))

  val lookupGmosSouthVisit: (VisitRecords, Visit.Id) => Option[VisitRecord[GmosModel.SouthStatic, GmosModel.SouthDynamic]] =
    lookup(gmosSouthVisits, _, _)

  val listGmosSouthVisits: VisitRecords => List[(Visit.Id, VisitRecord[GmosModel.SouthStatic, GmosModel.SouthDynamic])] =
    list(gmosSouthVisits, _)

  def visits(oid: Observation.Id): StateT[EitherInput, Database, Option[ListMap[Visit.Id, VisitRecord[_, _]]]] =
    Database.visitRecordsAt(oid).st.map { _.map(_.visits) }

  def visitAt(oid: Observation.Id, visitId: Visit.Id): StateT[EitherInput, Database, VisitRecord[_, _]] =
    for {
      v <- Database.visitRecordsAt(oid).st
      r <- StateT.liftF(
        v.flatMap(_.visits.get(visitId)).toRightNec(
          InputError.fromMessage(s"Unknown visit for observation $oid, visit $visitId")
        )
      )
    } yield r

  def stepAt(oid: Observation.Id, visitId: Visit.Id, stepId: Step.Id): StateT[EitherInput, Database, StepRecord[_]] =
    for {
      v <- visitAt(oid, visitId)
      r <- StateT.liftF(
        v.steps.get(stepId).toRightNec(
          InputError.fromMessage(s"Unknown step for observation $oid, visit $visitId, step $stepId")
        )
      )
    } yield r

}
