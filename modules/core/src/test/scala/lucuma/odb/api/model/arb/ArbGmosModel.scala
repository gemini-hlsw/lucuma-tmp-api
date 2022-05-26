// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model
package arb

import lucuma.core.`enum`._
import lucuma.core.math.{Offset, Wavelength}
import lucuma.core.math.arb.{ArbOffset, ArbWavelength}
import lucuma.core.model.NonNegDuration
import lucuma.core.model.arb.ArbNonNegDuration
import lucuma.core.util.arb.ArbEnumerated
import clue.data.Input
import eu.timepit.refined.types.all.NonEmptyString
import eu.timepit.refined.scalacheck.string._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary


trait ArbGmosModel {

  import ArbEnumerated._
  import ArbDurationModel._
  import ArbInput._
  import ArbNonNegDuration._
  import ArbOffset._
  import ArbOffsetModel._
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


  implicit val arbNorthStatic: Arbitrary[GmosModel.NorthStatic] =
    Arbitrary {
      for {
        d <- arbitrary[GmosNorthDetector]
        m <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosModel.NodAndShuffle]]
        s <- arbitrary[GmosNorthStageMode]
      } yield GmosModel.NorthStatic(d, m, n, s)
    }

  implicit val cogNorthStatic: Cogen[GmosModel.NorthStatic] =
    Cogen[(
      GmosNorthDetector,
      MosPreImaging,
      Option[GmosModel.NodAndShuffle],
      GmosNorthStageMode
    )].contramap { in => (
      in.detector,
      in.mosPreImaging,
      in.nodAndShuffle,
      in.stageMode
    )}

  implicit val arbCreateNorthStatic: Arbitrary[GmosModel.CreateNorthStatic] =
    Arbitrary {
      for {
        d <- arbitrary[GmosNorthDetector]
        m <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosModel.CreateNodAndShuffle]]
        s <- arbitrary[GmosNorthStageMode]
      } yield GmosModel.CreateNorthStatic(d, m, n, s)
    }

  implicit val cogCreateNorthStatic: Cogen[GmosModel.CreateNorthStatic] =
    Cogen[(
      GmosNorthDetector,
      MosPreImaging,
      Option[GmosModel.CreateNodAndShuffle],
      GmosNorthStageMode
    )].contramap { in => (
      in.detector,
      in.mosPreImaging,
      in.nodAndShuffle,
      in.stageMode
    )}

  implicit val arbSouthStatic: Arbitrary[GmosModel.SouthStatic] =
    Arbitrary {
      for {
        d <- arbitrary[GmosSouthDetector]
        m <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosModel.NodAndShuffle]]
        s <- arbitrary[GmosSouthStageMode]
      } yield GmosModel.SouthStatic(d, m, n, s)
    }

  implicit val cogSouthStatic: Cogen[GmosModel.SouthStatic] =
    Cogen[(
      GmosSouthDetector,
      MosPreImaging,
      Option[GmosModel.NodAndShuffle],
      GmosSouthStageMode
    )].contramap { in => (
      in.detector,
      in.mosPreImaging,
      in.nodAndShuffle,
      in.stageMode
    )}

  implicit val arbCreateSouthStatic: Arbitrary[GmosModel.CreateSouthStatic] =
    Arbitrary {
      for {
        d <- arbitrary[GmosSouthDetector]
        m <- arbitrary[MosPreImaging]
        n <- arbitrary[Option[GmosModel.CreateNodAndShuffle]]
        s <- arbitrary[GmosSouthStageMode]
      } yield GmosModel.CreateSouthStatic(d, m, n, s)
    }

  implicit val cogCreateSouthStatic: Cogen[GmosModel.CreateSouthStatic] =
    Cogen[(
      GmosSouthDetector,
      MosPreImaging,
      Option[GmosModel.CreateNodAndShuffle],
      GmosSouthStageMode
    )].contramap { in => (
      in.detector,
      in.mosPreImaging,
      in.nodAndShuffle,
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

  implicit def arbGrating[D: Arbitrary]: Arbitrary[GmosModel.GratingConfig[D]] =
    Arbitrary {
      for {
        d <- arbitrary[D]
        o <- arbitrary[GmosGratingOrder]
        w <- arbitrary[Wavelength]
      } yield GmosModel.GratingConfig(d, o, w)
    }

  implicit def cogGrating[D: Cogen]: Cogen[GmosModel.GratingConfig[D]] =
    Cogen[(
      D,
      GmosGratingOrder,
      Wavelength
    )].contramap { in =>
      (
        in.grating,
        in.order,
        in.wavelength
      )
    }

  implicit def arbCreateGrating[D: Arbitrary]: Arbitrary[GmosModel.CreateGratingConfig[D]] =
    Arbitrary {
      for {
        d <- arbitrary[D]
        o <- arbitrary[GmosGratingOrder]
        w <- arbitrary[WavelengthModel.WavelengthInput]
      } yield GmosModel.CreateGratingConfig(d, o, w)
    }

  implicit def cogCreateGrating[D: Cogen]: Cogen[GmosModel.CreateGratingConfig[D]] =
    Cogen[(
      D,
      GmosGratingOrder,
      WavelengthModel.WavelengthInput
    )].contramap { in =>
      (
        in.grating,
        in.order,
        in.wavelength
      )
    }


  implicit def arbCreateFpu[U: Arbitrary]: Arbitrary[GmosModel.CreateFpu[U]] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[U].map(GmosModel.CreateFpu.builtin),
        arbitrary[GmosModel.CreateCustomMask].map(GmosModel.CreateFpu.customMask[U])
      )
    }

  implicit def cogCreateFpu[U: Cogen]: Cogen[GmosModel.CreateFpu[U]] =
    Cogen[(
      Option[GmosModel.CreateCustomMask],
      Option[U]
    )].contramap { in =>
      (
        in.customMask,
        in.builtin
      )
    }

  implicit val arbNorthDynamic: Arbitrary[GmosModel.NorthDynamic] =
    Arbitrary {
      for {
        e <- arbitrary[NonNegDuration]
        c <- arbitrary[GmosModel.CcdReadout]
        x <- arbitrary[GmosDtax]
        r <- arbitrary[GmosRoi]
        g <- arbitrary[Option[GmosModel.GratingConfig[GmosNorthGrating]]]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[Option[Either[GmosModel.CustomMask, GmosNorthFpu]]]
      } yield GmosModel.NorthDynamic(e, c, x, r, g, f, u)
    }

  implicit val cogNorthDynamic: Cogen[GmosModel.NorthDynamic] =
    Cogen[(
      NonNegDuration,
      GmosModel.CcdReadout,
      GmosDtax,
      GmosRoi,
      Option[GmosModel.GratingConfig[GmosNorthGrating]],
      Option[GmosNorthFilter],
      Option[Either[GmosModel.CustomMask, GmosNorthFpu]]
    )].contramap { in =>
      (
        in.exposure,
        in.readout,
        in.dtax,
        in.roi,
        in.gratingConfig,
        in.filter,
        in.fpu
      )
    }

  implicit val arbCreateNorthDynamic: Arbitrary[GmosModel.CreateNorthDynamic] =
    Arbitrary {
      for {
        e <- arbitrary[DurationModel.NonNegDurationInput]
        c <- arbitrary[GmosModel.CreateCcdReadout]
        x <- arbitrary[GmosDtax]
        r <- arbitrary[GmosRoi]
        g <- arbitrary[Option[GmosModel.CreateGratingConfig[GmosNorthGrating]]]
        f <- arbitrary[Option[GmosNorthFilter]]
        u <- arbitrary[Option[GmosModel.CreateFpu[GmosNorthFpu]]]
      } yield GmosModel.CreateNorthDynamic(e, c, x, r, g, f, u)
    }

  implicit val cogCreateNorthDynamic: Cogen[GmosModel.CreateNorthDynamic] =
    Cogen[(
      DurationModel.NonNegDurationInput,
      GmosModel.CreateCcdReadout,
      GmosDtax,
      GmosRoi,
      Option[GmosModel.CreateGratingConfig[GmosNorthGrating]],
      Option[GmosNorthFilter],
      Option[GmosModel.CreateFpu[GmosNorthFpu]]
    )].contramap { in =>
      (
        in.exposure,
        in.readout,
        in.dtax,
        in.roi,
        in.gratingConfig,
        in.filter,
        in.fpu
      )
    }

  implicit val arbSouthDynamic: Arbitrary[GmosModel.SouthDynamic] =
    Arbitrary {
      for {
        e <- arbitrary[NonNegDuration]
        c <- arbitrary[GmosModel.CcdReadout]
        x <- arbitrary[GmosDtax]
        r <- arbitrary[GmosRoi]
        g <- arbitrary[Option[GmosModel.GratingConfig[GmosSouthGrating]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[Either[GmosModel.CustomMask, GmosSouthFpu]]]
      } yield GmosModel.SouthDynamic(e, c, x, r, g, f, u)
    }

  implicit val cogSouthDynamic: Cogen[GmosModel.SouthDynamic] =
    Cogen[(
      NonNegDuration,
      GmosModel.CcdReadout,
      GmosDtax,
      GmosRoi,
      Option[GmosModel.GratingConfig[GmosSouthGrating]],
      Option[GmosSouthFilter],
      Option[Either[GmosModel.CustomMask, GmosSouthFpu]]
    )].contramap { in =>
      (
        in.exposure,
        in.readout,
        in.dtax,
        in.roi,
        in.gratingConfig,
        in.filter,
        in.fpu
      )
    }

  implicit val arbCreateSouthDynamic: Arbitrary[GmosModel.CreateSouthDynamic] =
    Arbitrary {
      for {
        e <- arbitrary[DurationModel.NonNegDurationInput]
        c <- arbitrary[GmosModel.CreateCcdReadout]
        x <- arbitrary[GmosDtax]
        r <- arbitrary[GmosRoi]
        g <- arbitrary[Option[GmosModel.CreateGratingConfig[GmosSouthGrating]]]
        f <- arbitrary[Option[GmosSouthFilter]]
        u <- arbitrary[Option[GmosModel.CreateFpu[GmosSouthFpu]]]
      } yield GmosModel.CreateSouthDynamic(e, c, x, r, g, f, u)
    }

  implicit val cogCreateSouthDynamic: Cogen[GmosModel.CreateSouthDynamic] =
    Cogen[(
      DurationModel.NonNegDurationInput,
      GmosModel.CreateCcdReadout,
      GmosDtax,
      GmosRoi,
      Option[GmosModel.CreateGratingConfig[GmosSouthGrating]],
      Option[GmosSouthFilter],
      Option[GmosModel.CreateFpu[GmosSouthFpu]]
    )].contramap { in =>
      (
        in.exposure,
        in.readout,
        in.dtax,
        in.roi,
        in.gratingConfig,
        in.filter,
        in.fpu
      )
    }

}

object ArbGmosModel extends ArbGmosModel
