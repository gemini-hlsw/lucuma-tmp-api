// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.Eq
import lucuma.core.`enum`.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import cats.data.OneAnd
import cats.implicits._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class GcalModel(
  lamp:         GcalModel.Lamp,
  filter:       GcalFilter,
  diffuser:     GcalDiffuser,
  shutter:      GcalShutter,
)


object GcalModel {

  sealed abstract case class Arcs(arcs: OneAnd[Set, GcalArc]) {
    def toList: List[GcalArc] =
      arcs.head :: arcs.tail.toList
  }

  object Arcs {

    def apply(arc0: GcalArc, arcs: List[GcalArc]): Arcs =
      (arc0 :: arcs).distinct.sorted match {
        case h :: t => new Arcs(OneAnd(h, t.toSet)) {}
        case _      => sys.error("not possible")
      }

    implicit val EqArcs: Eq[Arcs] =
      Eq.by[Arcs, OneAnd[Set, GcalArc]](_.arcs)

  }

  type Lamp = Either[GcalContinuum, Arcs]

  object Lamp {

    def fromContinuum(continuum: GcalContinuum): Lamp =
      continuum.asLeft

    def fromArcs(arc0: GcalArc, arcs: GcalArc*): Lamp =
      Arcs(arc0, arcs.toList).asRight

    def fromConfig(
      continuum: Option[GcalContinuum],
      arcs:      List[GcalArc]
    ): ValidatedInput[Lamp] =
      (continuum, arcs) match {
        case (None,    Nil)    => InputError.fromMessage("Either continuum or a collection af arcs is required.").invalidNec
        case (Some(_), _ :: _) => InputError.fromMessage("Either continuum or a collection of arcs, but not both, is required.").invalidNec
        case (Some(c), Nil)    => c.asLeft[Arcs].validNec
        case (None,    h :: t) => Arcs(h, t).asRight[GcalContinuum].validNec
      }

  }

  implicit val EqGcalModel: Eq[GcalModel] =
    Eq.by { a => (
      a.lamp,
      a.filter,
      a.diffuser,
      a.shutter
    )}


  final case class Create(
    continuum:    Option[GcalContinuum],
    arcs:         List[GcalArc],
    filter:       GcalFilter,
    diffuser:     GcalDiffuser,
    shutter:      GcalShutter
  ) {


    val create: ValidatedInput[GcalModel] =
      Lamp.fromConfig(continuum, arcs).map { l =>
        GcalModel(l, filter, diffuser, shutter)
      }

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.continuum,
        a.arcs,
        a.filter,
        a.diffuser,
        a.shutter
      )}
  }

}
