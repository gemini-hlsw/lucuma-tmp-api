// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`._
import lucuma.core.math.{Offset, Wavelength}
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.arb.ArbEnumerated
import clue.data.Input
import eu.timepit.refined.types.all.NonEmptyString
import eu.timepit.refined.scalacheck.string._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.concurrent.duration.FiniteDuration



trait ArbGmosModel {

  import ArbEnumerated._
  import ArbFiniteDurationModel._
  import ArbInput._
  import ArbOffset._
  import ArbOffsetModel._
  import ArbStepModel._
  import ArbWavelength._
  import ArbWavelengthModel._

  implicit val arbNodAndShuffle: Arbitrary[GmosModel.NodAndShuffle] =
    Arbitrary {
      for {
        a <- arbitrary[Offset]
        b <- arbitrary[Offset]
        e <- arbitrary[GmosEOffsetting]
        o <- Gen.posNum[Int]
        c <- Gen.posNum[Int]
      } yield GmosModel.NodAndShuffle(a, b, e, o, c)
    }

  implicit val cogNodAndShuffle: Cogen[GmosModel.NodAndShuffle] =
    Cogen[(
      Offset,
      Offset,
      GmosEOffsetting,
      Int,
      Int
    )].contramap { in => (
      in.posA,
      in.posB,
      in.eOffset,
      in.shuffleOffset,
      in.shuffleCycles
    )}

  implicit val arbCreateNodAndShuffle: Arbitrary[GmosModel.CreateNodAndShuffle] =
    Arbitrary {
      for {
        a <- arbitrary[OffsetModel.Input]
        b <- arbitrary[OffsetModel.Input]
        e <- arbitrary[GmosEOffsetting]
        o <- Gen.posNum[Int]
        c <- Gen.posNum[Int]
      } yield GmosModel.CreateNodAndShuffle(a, b, e, o, c)
    }

  implicit val cogCreateNodAndShuffle: Cogen[GmosModel.CreateNodAndShuffle] =
    Cogen[(
      OffsetModel.Input,
      OffsetModel.Input,
      GmosEOffsetting,
      Int,
      Int
    )].contramap { in => (
      in.posA,
      in.posB,
      in.eOffset,
      in.shuffleOffset,
      in.shuffleCycles
    )}

  implicit val arbEditNodAndShuffle: Arbitrary[GmosModel.EditNodAndShuffle] =
    Arbitrary {
      for {
        a <- arbNotNullableInput[OffsetModel.Input].arbitrary
        b <- arbNotNullableInput[OffsetModel.Input].arbitrary
        e <- arbNotNullableInput[GmosEOffsetting].arbitrary
        o <- arbNotNullableInput[Int].arbitrary.map(_.map(_.abs))
        c <- arbNotNullableInput[Int].arbitrary.map(_.map(_.abs))
      } yield GmosModel.EditNodAndShuffle(a, b, e, o, c)
    }

  implicit val cogEditNodAndShuffle: Cogen[GmosModel.EditNodAndShuffle] =
    Cogen[(
      Input[OffsetModel.Input],
      Input[OffsetModel.Input],
      Input[GmosEOffsetting],
      Input[Int],
      Input[Int]
    )].contramap { in => (
      in.posA,
      in.posB,
      in.eOffset,
      in.shuffleOffset,
      in.shuffleCycles
    )}


  implicit val arbCommonStatic: Arbitrary[GmosModel.CommonStatic] =
    Arbitrary {
      for {
        d <- arbitrary[GmosDetector]
        m <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosModel.NodAndShuffle]]
      } yield GmosModel.CommonStatic(d, m, n)
    }

  implicit val cogCommonStatic: Cogen[GmosModel.CommonStatic] =
    Cogen[(
      GmosDetector,
      MosPreImaging,
      Option[GmosModel.NodAndShuffle]
    )].contramap { in => (
      in.detector,
      in.mosPreImaging,
      in.nodAndShuffle
    )}

  implicit val arbCreateCommonStatic: Arbitrary[GmosModel.CreateCommonStatic] =
    Arbitrary {
      for {
        d <- arbitrary[GmosDetector]
        m <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosModel.CreateNodAndShuffle]]
      } yield GmosModel.CreateCommonStatic(d, m, n)
    }

  implicit val cogCreateCommonStatic: Cogen[GmosModel.CreateCommonStatic] =
    Cogen[(
      GmosDetector,
      MosPreImaging,
      Option[GmosModel.CreateNodAndShuffle]
    )].contramap { in => (
      in.detector,
      in.mosPreImaging,
      in.nodAndShuffle
    )}

  implicit val arbNorthStatic: Arbitrary[GmosModel.NorthStatic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CommonStatic]
        s <- arbitrary[GmosNorthStageMode]
      } yield GmosModel.NorthStatic(c, s)
    }

  implicit val cogNorthStatic: Cogen[GmosModel.NorthStatic] =
    Cogen[(
      GmosModel.CommonStatic,
      GmosNorthStageMode
    )].contramap { in => (
      in.common,
      in.stageMode
    )}

  implicit val arbCreateNorthStatic: Arbitrary[GmosModel.CreateNorthStatic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CreateCommonStatic]
        s <- arbitrary[GmosNorthStageMode]
      } yield GmosModel.CreateNorthStatic(c, s)
    }

  implicit val cogCreateNorthStatic: Cogen[GmosModel.CreateNorthStatic] =
    Cogen[(
      GmosModel.CreateCommonStatic,
      GmosNorthStageMode
    )].contramap { in => (
      in.common,
      in.stageMode
    )}

  implicit val arbSouthStatic: Arbitrary[GmosModel.SouthStatic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CommonStatic]
        s <- arbitrary[GmosSouthStageMode]
      } yield GmosModel.SouthStatic(c, s)
    }

  implicit val cogSouthStatic: Cogen[GmosModel.SouthStatic] =
    Cogen[(
      GmosModel.CommonStatic,
      GmosSouthStageMode
    )].contramap { in => (
      in.common,
      in.stageMode
    )}

  implicit val arbCreateSouthStatic: Arbitrary[GmosModel.CreateSouthStatic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CreateCommonStatic]
        s <- arbitrary[GmosSouthStageMode]
      } yield GmosModel.CreateSouthStatic(c, s)
    }

  implicit val cogCreateSouthStatic: Cogen[GmosModel.CreateSouthStatic] =
    Cogen[(
      GmosModel.CreateCommonStatic,
      GmosSouthStageMode
    )].contramap { in => (
      in.common,
      in.stageMode
    )}


  implicit val arbCcdReadout: Arbitrary[GmosModel.CcdReadout] =
    Arbitrary {
      for {
        x <- arbitrary[GmosXBinning]
        y <- arbitrary[GmosYBinning]
        c <- arbitrary[GmosAmpCount]
        g <- arbitrary[GmosAmpGain]
        r <- arbitrary[GmosAmpReadMode]
      } yield GmosModel.CcdReadout(x, y, c, g, r)
    }

  implicit val cogCcdReadout: Cogen[GmosModel.CcdReadout] =
    Cogen[(
      GmosXBinning,
      GmosYBinning,
      GmosAmpCount,
      GmosAmpGain,
      GmosAmpReadMode
    )].contramap { in =>
      (
        in.xBin,
        in.yBin,
        in.ampCount,
        in.ampGain,
        in.ampRead
      )
    }

  implicit val arbCreateCcdReadout: Arbitrary[GmosModel.CreateCcdReadout] =
    Arbitrary {
      for {
        x <- arbitrary[GmosXBinning]
        y <- arbitrary[GmosYBinning]
        c <- arbitrary[GmosAmpCount]
        g <- arbitrary[GmosAmpGain]
        r <- arbitrary[GmosAmpReadMode]
      } yield GmosModel.CreateCcdReadout(x, y, c, g, r)
    }

  implicit val cogCreateCcdReadout: Cogen[GmosModel.CreateCcdReadout] =
    Cogen[(
      GmosXBinning,
      GmosYBinning,
      GmosAmpCount,
      GmosAmpGain,
      GmosAmpReadMode
    )].contramap { in =>
      (
        in.xBin,
        in.yBin,
        in.ampCount,
        in.ampGain,
        in.ampRead
      )
    }


  implicit val arbCommonDynamic: Arbitrary[GmosModel.CommonDynamic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CcdReadout]
        x <- arbitrary[GmosDtax]
        e <- arbitrary[FiniteDuration]
        r <- arbitrary[GmosRoi]
      } yield GmosModel.CommonDynamic(c, x, e, r)
    }

  implicit val cogCommonDynamic: Cogen[GmosModel.CommonDynamic] =
    Cogen[(
      GmosModel.CcdReadout,
      GmosDtax,
      FiniteDuration,
      GmosRoi
    )].contramap { in =>
      (
        in.readout,
        in.dtax,
        in.exposure,
        in.roi
      )
    }

  implicit val arbCreateCommonDynamic: Arbitrary[GmosModel.CreateCommonDynamic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CreateCcdReadout]
        x <- arbitrary[GmosDtax]
        e <- arbitrary[FiniteDurationModel.Input]
        r <- arbitrary[GmosRoi]
      } yield GmosModel.CreateCommonDynamic(c, x, e, r)
    }

  implicit val cogCreateCommonDynamic: Cogen[GmosModel.CreateCommonDynamic] =
    Cogen[(
      GmosModel.CreateCcdReadout,
      GmosDtax,
      FiniteDurationModel.Input,
      GmosRoi
    )].contramap { in =>
      (
        in.readout,
        in.dtax,
        in.exposure,
        in.roi
      )
    }

  implicit val arbCustomMask: Arbitrary[GmosModel.CustomMask] =
    Arbitrary {
      for {
        f <- arbitrary[NonEmptyString]
        w <- arbitrary[GmosCustomSlitWidth]
      } yield GmosModel.CustomMask(f, w)
    }

  implicit val cogCustomMask: Cogen[GmosModel.CustomMask] =
    Cogen[(
      String,
      GmosCustomSlitWidth
    )].contramap { in =>
      (
        in.filename.value,
        in.slitWidth
      )
    }

  implicit val arbCreateCustomMask: Arbitrary[GmosModel.CreateCustomMask] =
    Arbitrary {
      for {
        f <- arbitrary[NonEmptyString]
        w <- arbitrary[GmosCustomSlitWidth]
      } yield GmosModel.CreateCustomMask(f.value, w)
    }

  implicit val cogCreateCustomMask: Cogen[GmosModel.CreateCustomMask] =
    Cogen[(
      String,
      GmosCustomSlitWidth
    )].contramap { in =>
      (
        in.filename,
        in.slitWidth
      )
    }

  implicit def arbGrating[D: Arbitrary]: Arbitrary[GmosModel.Grating[D]] =
    Arbitrary {
      for {
        d <- arbitrary[D]
        o <- arbitrary[GmosDisperserOrder]
        w <- arbitrary[Wavelength]
      } yield GmosModel.Grating(d, o, w)
    }

  implicit def cogGrating[D: Cogen]: Cogen[GmosModel.Grating[D]] =
    Cogen[(
      D,
      GmosDisperserOrder,
      Wavelength
    )].contramap { in =>
      (
        in.disperser,
        in.order,
        in.wavelength
      )
    }

  implicit def arbCreateGrating[D: Arbitrary]: Arbitrary[GmosModel.CreateGrating[D]] =
    Arbitrary {
      for {
        d <- arbitrary[D]
        o <- arbitrary[GmosDisperserOrder]
        w <- arbitrary[WavelengthModel.Input]
      } yield GmosModel.CreateGrating(d, o, w)
    }

  implicit def cogCreateGrating[D: Cogen]: Cogen[GmosModel.CreateGrating[D]] =
    Cogen[(
      D,
      GmosDisperserOrder,
      WavelengthModel.Input
    )].contramap { in =>
      (
        in.disperser,
        in.order,
        in.wavelength
      )
    }

  implicit val arbNorthDynamic: Arbitrary[GmosModel.NorthDynamic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CommonDynamic]
        g <- arbitrary[Option[GmosModel.Grating[GmosNorthDisperser]]]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[Option[Either[GmosModel.CustomMask, GmosNorthFpu]]]
      } yield GmosModel.NorthDynamic(c, g, f, u)
    }

  implicit val cogNorthDynamic: Cogen[GmosModel.NorthDynamic] =
    Cogen[(
      GmosModel.CommonDynamic,
      Option[GmosModel.Grating[GmosNorthDisperser]],
      Option[GmosNorthFilter],
      Option[Either[GmosModel.CustomMask, GmosNorthFpu]]
    )].contramap { in =>
      (
        in.common,
        in.grating,
        in.filter,
        in.fpu
      )
    }

  implicit val arbCreateNorthDynamic: Arbitrary[GmosModel.CreateNorthDynamic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CreateCommonDynamic]
        g <- arbitrary[Option[GmosModel.CreateGrating[GmosNorthDisperser]]]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[Option[Either[GmosModel.CreateCustomMask, GmosNorthFpu]]]
      } yield GmosModel.CreateNorthDynamic(c, g, f, u)
    }

  implicit val cogCreateNorthDynamic: Cogen[GmosModel.CreateNorthDynamic] =
    Cogen[(
      GmosModel.CreateCommonDynamic,
      Option[GmosModel.CreateGrating[GmosNorthDisperser]],
      Option[GmosNorthFilter],
      Option[Either[GmosModel.CreateCustomMask, GmosNorthFpu]]
    )].contramap { in =>
      (
        in.common,
        in.grating,
        in.filter,
        in.fpu
      )
    }

  implicit val arbSouthDynamic: Arbitrary[GmosModel.SouthDynamic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CommonDynamic]
        g <- arbitrary[Option[GmosModel.Grating[GmosSouthDisperser]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[Either[GmosModel.CustomMask, GmosSouthFpu]]]
      } yield GmosModel.SouthDynamic(c, g, f, u)
    }

  implicit val cogSouthDynamic: Cogen[GmosModel.SouthDynamic] =
    Cogen[(
      GmosModel.CommonDynamic,
      Option[GmosModel.Grating[GmosSouthDisperser]],
      Option[GmosSouthFilter],
      Option[Either[GmosModel.CustomMask, GmosSouthFpu]]
    )].contramap { in =>
      (
        in.common,
        in.grating,
        in.filter,
        in.fpu
      )
    }

  implicit val arbCreateSouthDynamic: Arbitrary[GmosModel.CreateSouthDynamic] =
    Arbitrary {
      for {
        c <- arbitrary[GmosModel.CreateCommonDynamic]
        g <- arbitrary[Option[GmosModel.CreateGrating[GmosSouthDisperser]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[Either[GmosModel.CreateCustomMask, GmosSouthFpu]]]
      } yield GmosModel.CreateSouthDynamic(c, g, f, u)
    }

  implicit val cogCreateSouthDynamic: Cogen[GmosModel.CreateSouthDynamic] =
    Cogen[(
      GmosModel.CreateCommonDynamic,
      Option[GmosModel.CreateGrating[GmosSouthDisperser]],
      Option[GmosSouthFilter],
      Option[Either[GmosModel.CreateCustomMask, GmosSouthFpu]]
    )].contramap { in =>
      (
        in.common,
        in.grating,
        in.filter,
        in.fpu
      )
    }

  implicit val arbNorth: Arbitrary[GmosModel.North] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.NorthStatic]
        aq <- arbitrary[List[StepModel[GmosModel.NorthDynamic]]]
        sc <- arbitrary[List[StepModel[GmosModel.NorthDynamic]]]
      } yield GmosModel.North(st, aq, sc)
    }

  implicit val cogNorth: Cogen[GmosModel.North] =
    Cogen[(
      GmosModel.NorthStatic,
      List[StepModel[GmosModel.NorthDynamic]],
      List[StepModel[GmosModel.NorthDynamic]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit val arbCreateNorth: Arbitrary[GmosModel.CreateNorth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateNorthStatic]
        aq <- arbitrary[List[StepModel.CreateStep[GmosModel.CreateNorthDynamic]]]
        sc <- arbitrary[List[StepModel.CreateStep[GmosModel.CreateNorthDynamic]]]
      } yield GmosModel.CreateNorth(st, aq, sc)
    }

  implicit val cogCreateNorth: Cogen[GmosModel.CreateNorth] =
    Cogen[(
      GmosModel.CreateNorthStatic,
      List[StepModel.CreateStep[GmosModel.CreateNorthDynamic]],
      List[StepModel.CreateStep[GmosModel.CreateNorthDynamic]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit val arbSouth: Arbitrary[GmosModel.South] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.SouthStatic]
        aq <- arbitrary[List[StepModel[GmosModel.SouthDynamic]]]
        sc <- arbitrary[List[StepModel[GmosModel.SouthDynamic]]]
      } yield GmosModel.South(st, aq, sc)
    }

  implicit val cogSouth: Cogen[GmosModel.South] =
    Cogen[(
      GmosModel.SouthStatic,
      List[StepModel[GmosModel.SouthDynamic]],
      List[StepModel[GmosModel.SouthDynamic]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}

  implicit val arbCreateSouth: Arbitrary[GmosModel.CreateSouth] =
    Arbitrary {
      for {
        st <- arbitrary[GmosModel.CreateSouthStatic]
        aq <- arbitrary[List[StepModel.CreateStep[GmosModel.CreateSouthDynamic]]]
        sc <- arbitrary[List[StepModel.CreateStep[GmosModel.CreateSouthDynamic]]]
      } yield GmosModel.CreateSouth(st, aq, sc)
    }

  implicit val cogCreateSouth: Cogen[GmosModel.CreateSouth] =
    Cogen[(
      GmosModel.CreateSouthStatic,
      List[StepModel.CreateStep[GmosModel.CreateSouthDynamic]],
      List[StepModel.CreateStep[GmosModel.CreateSouthDynamic]]
    )].contramap { in => (
      in.static,
      in.acquisition,
      in.science
    )}


}

object ArbGmosModel extends ArbGmosModel
