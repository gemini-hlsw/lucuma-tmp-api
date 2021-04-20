// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.odb.api.model.SequenceModel._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbSequenceModel extends Helper {

  import ArbAtomModel._
  import ArbGmosModel._

  implicit def arbSequence[D: Arbitrary]: Arbitrary[Sequence[D]] =
    Arbitrary {
      for {
        s  <- smallSize
        as <- Gen.listOfN(s, arbitrary[AtomModel[D]])
      } yield Sequence(as)
    }

  implicit def cogSequence[D: Cogen]: Cogen[Sequence[D]] =
    Cogen[List[AtomModel[D]]].contramap(_.atoms)

  implicit def arbSequenceCreate[D: Arbitrary]: Arbitrary[Sequence.Create[D]] =
    Arbitrary {
      for {
        s  <- smallSize
        as <- Gen.listOfN(s, arbitrary[AtomModel.Create[D]])
      } yield Sequence.Create(as)
    }

  implicit def cogSequenceCreate[D: Cogen]: Cogen[Sequence.Create[D]] =
    Cogen[List[AtomModel.Create[D]]].contramap(_.atoms)


  implicit def arbConfig[S: Arbitrary, D: Arbitrary]: Arbitrary[Config[S, D]] =
    Arbitrary {
      for {
        st <- arbitrary[S]
        aq <- arbitrary[Sequence[D]]
        sc <- arbitrary[Sequence[D]]
      } yield Config(st, aq, sc)
    }

  implicit def cogConfig[S: Cogen, D: Cogen]: Cogen[Config[S, D]] =
    Cogen[(
      S,
      Sequence[D],
      Sequence[D]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbConfigCreate[S: Arbitrary, D: Arbitrary]: Arbitrary[Config.Create[S, D]] =
    Arbitrary {
      for {
        st <- arbitrary[S]
        aq <- arbitrary[Sequence.Create[D]]
        sc <- arbitrary[Sequence.Create[D]]
      } yield Config.Create(st, aq, sc)
    }

  implicit def cogConfigCreate[S: Cogen, D: Cogen]: Cogen[Config.Create[S, D]] =
    Cogen[(
      S,
      Sequence.Create[D],
      Sequence.Create[D]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit def arbInstrumentConfigGmosNorth: Arbitrary[InstrumentConfig.GmosNorth] =
    Arbitrary {
      arbitrary[Config[GmosModel.NorthStatic, GmosModel.NorthDynamic]].map { c =>
        InstrumentConfig.GmosNorth(c)
      }
    }

  implicit def cogInstrumentConfigGmosNorth: Cogen[InstrumentConfig.GmosNorth] =
    Cogen[Config[GmosModel.NorthStatic, GmosModel.NorthDynamic]].contramap { in =>
      in.config
    }

  implicit def arbInstrumentConfigCreateGmosNorth: Arbitrary[InstrumentConfig.CreateGmosNorth] =
    Arbitrary {
      arbitrary[Config.Create[GmosModel.CreateNorthStatic, GmosModel.CreateNorthDynamic]].map { c =>
        InstrumentConfig.CreateGmosNorth(c)
      }
    }

  implicit def cogInstrumentConfigCreateGmosNorth: Cogen[InstrumentConfig.CreateGmosNorth] =
    Cogen[Config.Create[GmosModel.CreateNorthStatic, GmosModel.CreateNorthDynamic]].contramap { in =>
      in.config
    }

  implicit def arbInstrumentConfigGmosSouth: Arbitrary[InstrumentConfig.GmosSouth] =
    Arbitrary {
      arbitrary[Config[GmosModel.SouthStatic, GmosModel.SouthDynamic]].map { c =>
        InstrumentConfig.GmosSouth(c)
      }
    }

  implicit def cogInstrumentConfigGmosSouth: Cogen[InstrumentConfig.GmosSouth] =
    Cogen[Config[GmosModel.SouthStatic, GmosModel.SouthDynamic]].contramap { in =>
      in.config
    }

  implicit def arbInstrumentConfigCreateGmosSouth: Arbitrary[InstrumentConfig.CreateGmosSouth] =
    Arbitrary {
      arbitrary[Config.Create[GmosModel.CreateSouthStatic, GmosModel.CreateSouthDynamic]].map { c =>
        InstrumentConfig.CreateGmosSouth(c)
      }
    }

  implicit def cogInstrumentConfigCreateGmosSouth: Cogen[InstrumentConfig.CreateGmosSouth] =
    Cogen[Config.Create[GmosModel.CreateSouthStatic, GmosModel.CreateSouthDynamic]].contramap { in =>
      in.config
    }

  implicit def arbInstrumentConfig: Arbitrary[InstrumentConfig] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[InstrumentConfig.GmosNorth],
        arbitrary[InstrumentConfig.GmosSouth]
      )
    }

  implicit def cogInstrumentConfig: Cogen[InstrumentConfig] =
    Cogen[(
      Option[InstrumentConfig.GmosNorth],
      Option[InstrumentConfig.GmosSouth]
    )].contramap { in => (
      in.gmosNorth,
      in.gmosSouth
    )}

  implicit def arbSequenceModelCreate: Arbitrary[InstrumentConfig.Create] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[InstrumentConfig.CreateGmosNorth].map(InstrumentConfig.Create.gmosNorth),
        arbitrary[InstrumentConfig.CreateGmosSouth].map(InstrumentConfig.Create.gmosSouth)
      )
    }

  implicit def cogSequenceModelCreate: Cogen[InstrumentConfig.Create] =
    Cogen[(
      Option[InstrumentConfig.CreateGmosNorth],
      Option[InstrumentConfig.CreateGmosSouth]
    )].contramap { in => (
      in.gmosNorth,
      in.gmosSouth
    )}

}

object ArbSequenceModel extends ArbSequenceModel
