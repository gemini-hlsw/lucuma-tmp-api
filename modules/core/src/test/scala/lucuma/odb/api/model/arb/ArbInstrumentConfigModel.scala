// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
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
        aq <- arbitrary[SequenceModel.Sequence[GmosModel.NorthDynamic]]
        sc <- arbitrary[SequenceModel.Sequence[GmosModel.NorthDynamic]]
      } yield InstrumentConfigModel.GmosNorth(st, aq, sc)
    }

  implicit def cogInstrumentConfigGmosNorth: Cogen[InstrumentConfigModel.GmosNorth] =
    Cogen[(
      GmosModel.NorthStatic,
      SequenceModel.Sequence[GmosModel.NorthDynamic],
      SequenceModel.Sequence[GmosModel.NorthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfigGmosSouth: Arbitrary[InstrumentConfigModel.GmosSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.SouthStatic]
        aq <- arbitrary[SequenceModel.Sequence[GmosModel.SouthDynamic]]
        sc <- arbitrary[SequenceModel.Sequence[GmosModel.SouthDynamic]]
      } yield InstrumentConfigModel.GmosSouth(st, aq, sc)
    }

  implicit def cogInstrumentConfigGmosSouth: Cogen[InstrumentConfigModel.GmosSouth] =
    Cogen[(
      GmosModel.SouthStatic,
      SequenceModel.Sequence[GmosModel.SouthDynamic],
      SequenceModel.Sequence[GmosModel.SouthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfigCreateGmosNorth: Arbitrary[InstrumentConfigModel.CreateGmosNorth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateNorthStatic]
        aq <- arbitrary[SequenceModel.Sequence.Create[GmosModel.CreateNorthDynamic]]
        sc <- arbitrary[SequenceModel.Sequence.Create[GmosModel.CreateNorthDynamic]]
      } yield InstrumentConfigModel.CreateGmosNorth(st, aq, sc)
    }

  implicit def cogInstrumentConfigCreateGmosNorth: Cogen[InstrumentConfigModel.CreateGmosNorth] =
    Cogen[(
      GmosModel.CreateNorthStatic,
      SequenceModel.Sequence.Create[GmosModel.CreateNorthDynamic],
      SequenceModel.Sequence.Create[GmosModel.CreateNorthDynamic]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfigCreateGmosSouth: Arbitrary[InstrumentConfigModel.CreateGmosSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateSouthStatic]
        aq <- arbitrary[SequenceModel.Sequence.Create[GmosModel.CreateSouthDynamic]]
        sc <- arbitrary[SequenceModel.Sequence.Create[GmosModel.CreateSouthDynamic]]
      } yield InstrumentConfigModel.CreateGmosSouth(st, aq, sc)
    }

  implicit def cogInstrumentConfigCreateGmosSouth: Cogen[InstrumentConfigModel.CreateGmosSouth] =
    Cogen[(
      GmosModel.CreateSouthStatic,
      SequenceModel.Sequence.Create[GmosModel.CreateSouthDynamic],
      SequenceModel.Sequence.Create[GmosModel.CreateSouthDynamic]
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

  implicit def arbSequenceModelCreate: Arbitrary[InstrumentConfigModel.Create] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[InstrumentConfigModel.CreateGmosNorth].map(InstrumentConfigModel.Create.gmosNorth),
        arbitrary[InstrumentConfigModel.CreateGmosSouth].map(InstrumentConfigModel.Create.gmosSouth)
      )
    }

  implicit def cogSequenceModelCreate: Cogen[InstrumentConfigModel.Create] =
    Cogen[(
      Option[InstrumentConfigModel.CreateGmosNorth],
      Option[InstrumentConfigModel.CreateGmosSouth]
    )].contramap { in => (
      in.gmosNorth,
      in.gmosSouth
    )}


}

object ArbInstrumentConfigModel extends ArbInstrumentConfigModel
