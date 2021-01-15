// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.arb._
import lucuma.core.`enum`.GmosNorthDisperser
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.arb.ArbEnumerated

import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import monocle.law.discipline._
import munit.DisciplineSuite

final class GmosModelSuite extends DisciplineSuite {

  import ArbEnumerated._
  import ArbGmosModel._
  import ArbOffset._
  import ArbWavelength._

  checkAll("GmosModel.NodAndShuffle",               EqTests[GmosModel.NodAndShuffle].eqv)
  checkAll("GmosModel.NodAndShuffle.posA",          LensTests(GmosModel.NodAndShuffle.posA))
  checkAll("GmosModel.NodAndShuffle.posB",          LensTests(GmosModel.NodAndShuffle.posB))
  checkAll("GmosModel.NodAndShuffle.eOffset",       LensTests(GmosModel.NodAndShuffle.eOffset))
  checkAll("GmosModel.NodAndShuffle.shuffleOffset", LensTests(GmosModel.NodAndShuffle.shuffleOffset))
  checkAll("GmosModel.NodAndShuffle.shuffleCycles", LensTests(GmosModel.NodAndShuffle.shuffleCycles))
  checkAll("GmosModel.CreateNodAndShuffle",         EqTests[GmosModel.CreateNodAndShuffle].eqv)
  checkAll("GmosModel.EditNodAndShuffle",           EqTests[GmosModel.EditNodAndShuffle].eqv)


  checkAll("GmosModel.CommonStatic",                EqTests[GmosModel.CommonStatic].eqv)
  checkAll("GmosModel.CommonStatic.detector",       LensTests(GmosModel.CommonStatic.detector))
  checkAll("GmosModel.CommonStatic.mosPreImaging",  LensTests(GmosModel.CommonStatic.mosPreImaging))
  checkAll("GmosModel.CommonStatic.nodAndShuffle",  LensTests(GmosModel.CommonStatic.nodAndShuffle))
  checkAll("GmosModel.CreateCommonStatic",          EqTests[GmosModel.CreateCommonStatic].eqv)

  checkAll("GmosModel.NorthStatic",                 EqTests[GmosModel.NorthStatic].eqv)
  checkAll("GmosModel.NorthStatic.common",          LensTests(GmosModel.NorthStatic.common))
  checkAll("GmosModel.NorthStatic.stageMode",       LensTests(GmosModel.NorthStatic.stageMode))
  checkAll("GmosModel.CreateNorthStatic",           EqTests[GmosModel.CreateNorthStatic].eqv)

  checkAll("GmosModel.SouthStatic",                 EqTests[GmosModel.SouthStatic].eqv)
  checkAll("GmosModel.SouthStatic.common",          LensTests(GmosModel.SouthStatic.common))
  checkAll("GmosModel.SouthStatic.stageMode",       LensTests(GmosModel.SouthStatic.stageMode))
  checkAll("GmosModel.CreateSouthStatic",           EqTests[GmosModel.CreateSouthStatic].eqv)

  checkAll("GmosModel.CcdReadout",                  EqTests[GmosModel.CcdReadout].eqv)
  checkAll("GmosModel.CcdReadout.xBin",             LensTests(GmosModel.CcdReadout.xBin))
  checkAll("GmosModel.CcdReadout.yBin",             LensTests(GmosModel.CcdReadout.yBin))
  checkAll("GmosModel.CcdReadout.ampCount",         LensTests(GmosModel.CcdReadout.ampCount))
  checkAll("GmosModel.CcdReadout.ampGrain",         LensTests(GmosModel.CcdReadout.ampGain))
  checkAll("GmosModel.CcdReadout.ampRead ",         LensTests(GmosModel.CcdReadout.ampRead))
  checkAll("GmosModel.CreateCcdReadout",            EqTests[GmosModel.CreateCcdReadout].eqv)

  checkAll("GmosModel.CommonDynamic",               EqTests[GmosModel.CommonDynamic].eqv)
  checkAll("GmosModel.CommonDynamic.readout",       LensTests(GmosModel.CommonDynamic.readout))
  checkAll("GmosModel.CommonDynamic.dtax",          LensTests(GmosModel.CommonDynamic.dtax))
  checkAll("GmosModel.CommonDynamic.exposure",      LensTests(GmosModel.CommonDynamic.exposure))
  checkAll("GmosModel.CommonDynamic.roi",           LensTests(GmosModel.CommonDynamic.roi))
  checkAll("GmosModel.CreateCommonDynamic",         EqTests[GmosModel.CreateCommonDynamic].eqv)

  checkAll("GmosModel.CustomMask",                  EqTests[GmosModel.CustomMask].eqv)
  checkAll("GmosModel.CustomMask.filename",         LensTests(GmosModel.CustomMask.filename))
  checkAll("GmosModel.CustomMask.slitWidth",        LensTests(GmosModel.CustomMask.slitWidth))
  checkAll("GmosModel.CreateCustomMask",            EqTests[GmosModel.CreateCustomMask].eqv)

  checkAll("GmosModel.Grating",                     EqTests[GmosModel.Grating[GmosNorthDisperser]].eqv)
  checkAll("GmosModel.Grating.disperser",           LensTests(GmosModel.Grating.disperser[GmosNorthDisperser]))
  checkAll("GmosModel.Grating.order",               LensTests(GmosModel.Grating.order[GmosNorthDisperser]))
  checkAll("GmosModel.Grating.wavelength",          LensTests(GmosModel.Grating.wavelength[GmosNorthDisperser]))
  checkAll("GmosModel.CreateGrating",               EqTests[GmosModel.CreateGrating[GmosNorthDisperser]].eqv)

  checkAll("GmosModel.NorthDynamic",                EqTests[GmosModel.NorthDynamic].eqv)
  checkAll("GmosModel.NorthDynamic.common",         LensTests(GmosModel.NorthDynamic.common))
  checkAll("GmosModel.NorthDynamic.grating",        LensTests(GmosModel.NorthDynamic.grating))
  checkAll("GmosModel.NorthDynamic.filter",         LensTests(GmosModel.NorthDynamic.filter))
  checkAll("GmosModel.NorthDynamic.fpu",            LensTests(GmosModel.NorthDynamic.fpu))
  checkAll("GmosModel.CreateNorthDynamic",          EqTests[GmosModel.CreateNorthDynamic].eqv)

  checkAll("GmosModel.SouthDynamic",                EqTests[GmosModel.SouthDynamic].eqv)
  checkAll("GmosModel.SouthDynamic.common",         LensTests(GmosModel.SouthDynamic.common))
  checkAll("GmosModel.SouthDynamic.grating",        LensTests(GmosModel.SouthDynamic.grating))
  checkAll("GmosModel.SouthDynamic.filter",         LensTests(GmosModel.SouthDynamic.filter))
  checkAll("GmosModel.SouthDynamic.fpu",            LensTests(GmosModel.SouthDynamic.fpu))
  checkAll("GmosModel.CreateSouthDynamic",          EqTests[GmosModel.CreateSouthDynamic].eqv)

}
