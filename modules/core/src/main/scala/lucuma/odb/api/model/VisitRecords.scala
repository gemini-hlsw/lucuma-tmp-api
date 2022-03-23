// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import cats.data.StateT
import cats.syntax.eq._
import cats.syntax.option._
import lucuma.core.`enum`.Instrument
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

  val gmosNorthVisits: Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.NorthStatic, GmosModel.NorthDynamic]]] =
    Prism.partial[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.NorthStatic, GmosModel.NorthDynamic]]] {
      case GmosNorth(vs) => vs
    }(GmosNorth(_))


  val gmosSouthVisits: Prism[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.SouthStatic, GmosModel.SouthDynamic]]] =
    Prism.partial[VisitRecords, ListMap[Visit.Id, VisitRecord[GmosModel.SouthStatic, GmosModel.SouthDynamic]]] {
      case GmosSouth(vs) => vs
    }(GmosSouth(_))

  def visits(oid: Observation.Id): StateT[EitherInput, Database, Option[ListMap[Visit.Id, VisitRecord[_, _]]]] =
    Database.visitRecordsAt(oid).st[EitherInput].map { _.map(_.visits) }

  def visitAt(oid: Observation.Id, visitId: Visit.Id): StateT[EitherInput, Database, VisitRecord[_, _]] =
    for {
      v <- Database.visitRecordsAt(oid).st[EitherInput]
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
