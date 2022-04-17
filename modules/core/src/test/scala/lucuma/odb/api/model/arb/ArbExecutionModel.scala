// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbExecutionModel extends Helper {

  import ArbGmosModel._
  import ArbSequenceModel._

  implicit def arbExecutionModelConfig[S: Arbitrary, D: Arbitrary]: Arbitrary[ExecutionModel.Config[S, D]] =
    Arbitrary {
      for {
        st <- arbitrary[S]
        aq <- arbitrary[Sequence[D]]
        sc <- arbitrary[Sequence[D]]
      } yield ExecutionModel.Config(st, aq, sc)
    }

  implicit def cogExecutionModelConfig[S: Cogen, D: Cogen]: Cogen[ExecutionModel.Config[S, D]] =
    Cogen[(
      S,
      Sequence[D],
      Sequence[D]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}


  implicit def arbExecutionModelGmosNorth: Arbitrary[ExecutionModel.GmosNorth] =
    Arbitrary {
      arbitrary[ExecutionModel.Config[GmosModel.NorthStatic, GmosModel.NorthDynamic]].map { c =>
        ExecutionModel.GmosNorth(c)
      }
    }

  implicit def cogExecutionModelGmosNorth: Cogen[ExecutionModel.GmosNorth] =
    Cogen[
      ExecutionModel.Config[GmosModel.NorthStatic, GmosModel.NorthDynamic]
    ].contramap(_.config)

  implicit def arbExecutionModelGmosSouth: Arbitrary[ExecutionModel.GmosSouth] =
    Arbitrary {
      arbitrary[ExecutionModel.Config[GmosModel.SouthStatic, GmosModel.SouthDynamic]].map { c =>
        ExecutionModel.GmosSouth(c)
      }
    }

  implicit def cogExecutionModelGmosSouth: Cogen[ExecutionModel.GmosSouth] =
    Cogen[
      ExecutionModel.Config[GmosModel.SouthStatic, GmosModel.SouthDynamic]
    ].contramap(_.config)

  implicit def arbExecutionModelCreateGmosNorth: Arbitrary[ExecutionModel.CreateGmosNorth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateNorthStatic]
        aq <- arbitrary[SequenceModel.Create[GmosModel.CreateNorthDynamic]]
        sc <- arbitrary[SequenceModel.Create[GmosModel.CreateNorthDynamic]]
      } yield ExecutionModel.CreateGmosNorth(st, aq, sc)
    }

  def arbValidExecutionModelCreateGmosNorth: Arbitrary[ExecutionModel.CreateGmosNorth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateNorthStatic]
        aq <- arbValidSequenceCreate[GmosModel.CreateNorthDynamic].arbitrary
        sc <- arbValidSequenceCreate[GmosModel.CreateNorthDynamic].arbitrary
      } yield ExecutionModel.CreateGmosNorth(st, aq, sc)
    }

  implicit def cogExecutionModelCreateGmosNorth: Cogen[ExecutionModel.CreateGmosNorth] =
    Cogen[(
      GmosModel.CreateNorthStatic,
      SequenceModel.Create[GmosModel.CreateNorthDynamic],
      SequenceModel.Create[GmosModel.CreateNorthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbExecutionModelCreateGmosSouth: Arbitrary[ExecutionModel.CreateGmosSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateSouthStatic]
        aq <- arbitrary[SequenceModel.Create[GmosModel.CreateSouthDynamic]]
        sc <- arbitrary[SequenceModel.Create[GmosModel.CreateSouthDynamic]]
      } yield ExecutionModel.CreateGmosSouth(st, aq, sc)
    }

  def arbValidExecutionModelCreateGmosSouth: Arbitrary[ExecutionModel.CreateGmosSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateSouthStatic]
        aq <- arbValidSequenceCreate[GmosModel.CreateSouthDynamic].arbitrary
        sc <- arbValidSequenceCreate[GmosModel.CreateSouthDynamic].arbitrary
      } yield ExecutionModel.CreateGmosSouth(st, aq, sc)
    }

  implicit def cogExecutionModelCreateGmosSouth: Cogen[ExecutionModel.CreateGmosSouth] =
    Cogen[(
      GmosModel.CreateSouthStatic,
      SequenceModel.Create[GmosModel.CreateSouthDynamic],
      SequenceModel.Create[GmosModel.CreateSouthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbExecutionModel: Arbitrary[ExecutionModel] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[ExecutionModel.GmosNorth],
        arbitrary[ExecutionModel.GmosSouth]
      )
    }

  implicit def cogExecutionModel: Cogen[ExecutionModel] =
    Cogen[(
      Option[ExecutionModel.GmosNorth],
      Option[ExecutionModel.GmosSouth]
    )].contramap { in => (
      ExecutionModel.AsGmosNorth.getOption(in),
      ExecutionModel.AsGmosSouth.getOption(in)
    )}

  def arbValidExecutionModelCreate: Arbitrary[ExecutionModel.Create] =
    Arbitrary {
      Gen.oneOf(
        arbValidExecutionModelCreateGmosNorth.arbitrary.map(ExecutionModel.Create.gmosNorth),
        arbValidExecutionModelCreateGmosSouth.arbitrary.map(ExecutionModel.Create.gmosSouth)
      )
    }

  implicit def arbExecutionModelCreate: Arbitrary[ExecutionModel.Create] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[ExecutionModel.CreateGmosNorth].map(ExecutionModel.Create.gmosNorth),
        arbitrary[ExecutionModel.CreateGmosSouth].map(ExecutionModel.Create.gmosSouth)
      )
    }

  implicit def cogExecutionModelCreate: Cogen[ExecutionModel.Create] =
    Cogen[(
      Option[ExecutionModel.CreateGmosNorth],
      Option[ExecutionModel.CreateGmosSouth]
    )].contramap { in => (
      in.gmosNorth,
      in.gmosSouth
    )}


}

object ArbExecutionModel extends ArbExecutionModel
