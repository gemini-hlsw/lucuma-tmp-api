// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbInstrumentConfigModel extends Helper {

  import ArbGmosModel._
  import ArbSequenceModel._


  implicit def arbInstrumentConfigGmosNorth: Arbitrary[InstrumentConfigModel.GmosNorth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.NorthStatic]
        aq <- arbitrary[DereferencedSequence[GmosModel.NorthDynamic]]
        sc <- arbitrary[DereferencedSequence[GmosModel.NorthDynamic]]
      } yield InstrumentConfigModel.GmosNorth(st, aq, sc)
    }

  implicit def cogInstrumentConfigGmosNorth: Cogen[InstrumentConfigModel.GmosNorth] =
    Cogen[(
      GmosModel.NorthStatic,
      DereferencedSequence[GmosModel.NorthDynamic],
      DereferencedSequence[GmosModel.NorthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfigGmosSouth: Arbitrary[InstrumentConfigModel.GmosSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.SouthStatic]
        aq <- arbitrary[DereferencedSequence[GmosModel.SouthDynamic]]
        sc <- arbitrary[DereferencedSequence[GmosModel.SouthDynamic]]
      } yield InstrumentConfigModel.GmosSouth(st, aq, sc)
    }

  implicit def cogInstrumentConfigGmosSouth: Cogen[InstrumentConfigModel.GmosSouth] =
    Cogen[(
      GmosModel.SouthStatic,
      DereferencedSequence[GmosModel.SouthDynamic],
      DereferencedSequence[GmosModel.SouthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfigCreateGmosNorth: Arbitrary[InstrumentConfigModel.CreateGmosNorth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateNorthStatic]
        aq <- arbitrary[SequenceModel.Create[GmosModel.CreateNorthDynamic]]
        sc <- arbitrary[SequenceModel.Create[GmosModel.CreateNorthDynamic]]
      } yield InstrumentConfigModel.CreateGmosNorth(st, aq, sc)
    }

  def arbValidInstrumentConfigCreateGmosNorth: Arbitrary[InstrumentConfigModel.CreateGmosNorth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateNorthStatic]
        aq <- arbValidSequenceCreate[GmosModel.CreateNorthDynamic].arbitrary
        sc <- arbValidSequenceCreate[GmosModel.CreateNorthDynamic].arbitrary
      } yield InstrumentConfigModel.CreateGmosNorth(st, aq, sc)
    }

  implicit def cogInstrumentConfigCreateGmosNorth: Cogen[InstrumentConfigModel.CreateGmosNorth] =
    Cogen[(
      GmosModel.CreateNorthStatic,
      SequenceModel.Create[GmosModel.CreateNorthDynamic],
      SequenceModel.Create[GmosModel.CreateNorthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfigCreateGmosSouth: Arbitrary[InstrumentConfigModel.CreateGmosSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateSouthStatic]
        aq <- arbitrary[SequenceModel.Create[GmosModel.CreateSouthDynamic]]
        sc <- arbitrary[SequenceModel.Create[GmosModel.CreateSouthDynamic]]
      } yield InstrumentConfigModel.CreateGmosSouth(st, aq, sc)
    }

  def arbValidInstrumentConfigCreateGmosSouth: Arbitrary[InstrumentConfigModel.CreateGmosSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateSouthStatic]
        aq <- arbValidSequenceCreate[GmosModel.CreateSouthDynamic].arbitrary
        sc <- arbValidSequenceCreate[GmosModel.CreateSouthDynamic].arbitrary
      } yield InstrumentConfigModel.CreateGmosSouth(st, aq, sc)
    }

  implicit def cogInstrumentConfigCreateGmosSouth: Cogen[InstrumentConfigModel.CreateGmosSouth] =
    Cogen[(
      GmosModel.CreateSouthStatic,
      SequenceModel.Create[GmosModel.CreateSouthDynamic],
      SequenceModel.Create[GmosModel.CreateSouthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfig: Arbitrary[InstrumentConfigModel] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[InstrumentConfigModel.GmosNorth],
        arbitrary[InstrumentConfigModel.GmosSouth]
      )
    }

  implicit def cogInstrumentConfig: Cogen[InstrumentConfigModel] =
    Cogen[(
      Option[InstrumentConfigModel.GmosNorth],
      Option[InstrumentConfigModel.GmosSouth]
    )].contramap { in => (
      in.gmosNorth,
      in.gmosSouth
    )}

  def arbValidInstrumentConfigModelCreate: Arbitrary[InstrumentConfigModel.Create] =
    Arbitrary {
      Gen.oneOf(
        arbValidInstrumentConfigCreateGmosNorth.arbitrary.map(InstrumentConfigModel.Create.gmosNorth),
        arbValidInstrumentConfigCreateGmosSouth.arbitrary.map(InstrumentConfigModel.Create.gmosSouth)
      )
    }

  implicit def arbInstrumentConfigModelCreate: Arbitrary[InstrumentConfigModel.Create] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[InstrumentConfigModel.CreateGmosNorth].map(InstrumentConfigModel.Create.gmosNorth),
        arbitrary[InstrumentConfigModel.CreateGmosSouth].map(InstrumentConfigModel.Create.gmosSouth)
      )
    }

  implicit def cogInstrumentConfigModelCreate: Cogen[InstrumentConfigModel.Create] =
    Cogen[(
      Option[InstrumentConfigModel.CreateGmosNorth],
      Option[InstrumentConfigModel.CreateGmosSouth]
    )].contramap { in => (
      in.gmosNorth,
      in.gmosSouth
    )}


}

object ArbInstrumentConfigModel extends ArbInstrumentConfigModel
