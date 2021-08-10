// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.search.gmosnorth

import cats.implicits._
import lucuma.core.enum._
import lucuma.core.enum.GmosNorthFpu._
import lucuma.core.enum.GmosNorthDisperser._
import lucuma.core.enum.GmosNorthFilter._
import lucuma.core.math.Wavelength

object GmosNorthFilterSelector {

  /**
   * Given a disperser, fpu, and wavelength, find the set of appopriate blocking filter(s) for
   * spectroscopy. An empty set means there is no solution. A set containing `None` means that a
   * filter is not necessary. Filters do not overlap in practice but if a wavelength falls on the
   * exact boundary both filters will be selected.
   */
  def selectBlocking(
    disperser: GmosNorthDisperser,
    fpu:       GmosNorthFpu,
    λ:         Wavelength
  ): Set[Option[GmosNorthFilter]] =
    all.filter(_.matches(disperser, fpu, λ)).map(_.filter).toSet

  // For clarity these definitions mirror the structure of
  // https://docs.google.com/spreadsheets/d/1wCZYGoeNU230tJva89rFZYkmZUPiqEEWYvI4P-jbMlE/edit#gid=0

  private val IFU1: Set[GmosNorthFpu] =
    Set(Ifu2, Ifu3)

  private val IFU2: Set[GmosNorthFpu] =
    Set(Ifu1)

  private val Longslit: Set[GmosNorthFpu] =
    Set(
      Ns0, Ns1, Ns2, Ns3, Ns4, Ns5,
      LongSlit_0_25,
      LongSlit_0_50,
      LongSlit_0_75,
      LongSlit_1_00,
      LongSlit_1_50,
      LongSlit_2_00,
      LongSlit_5_00,
    )

  private val Longslit_MOS_IFU1: Set[GmosNorthFpu] =
    Longslit ++ IFU1

  // Conventional shorthand for the gratings.
  // Note that in the B600 and R150 cases the obsolete versions are omitted.
  private val B1200 = B1200_G5301
  private val R831  = R831_G5302
  private val B600  = B600_G5307
  private val R600  = R600_G5304
  private val R400  = R400_G5305
  private val R150  = R150_G5308

  private case class Selector(
    disperser: GmosNorthDisperser,
    fpus:      Set[GmosNorthFpu],
    minλ:      Wavelength,
    maxλ:      Wavelength,
    filter:    Option[GmosNorthFilter],
    optimalλ:  Option[Wavelength]
  ) {

    def matches(disperser: GmosNorthDisperser, fpu: GmosNorthFpu, λ: Wavelength): Boolean =
      disperser === this.disperser && fpus.contains(fpu) && minλ <= λ && λ <= maxλ

  }

  private object Selector {

    // Overloaded `apply` that taks Int anometers instead of Wavelengths
    private[GmosNorthFilterSelector] def apply(
      disperser: GmosNorthDisperser,
      fpus:      Set[GmosNorthFpu],
      minNm:     Int,
      maxNm:     Int,
      filter:    Option[GmosNorthFilter],
      optimalNm: Option[Int]
    ): Selector =
      apply(
        disperser,
        fpus,
        Wavelength.fromNanometers(minNm).get,
        Wavelength.fromNanometers(maxNm).get,
        filter,
        optimalNm.flatMap(Wavelength.fromNanometers)
      )

  }

  private val all = List(
    Selector(R831,  Longslit_MOS_IFU1, 350,  600, None,               None),
    Selector(R831,  Longslit_MOS_IFU1, 600,  660, Some(GG455),        None),
    Selector(R831,  Longslit_MOS_IFU1, 660,  910, Some(OG515),        None),  // Overfill camera warning if wav > 860
    Selector(R831,  Longslit_MOS_IFU1, 910, 1100, Some(RG610),        None),  // Overfill camera warning if wav > 860
    Selector(R400,  Longslit_MOS_IFU1, 350,  700, None,               None),
    Selector(R400,  Longslit_MOS_IFU1, 700,  760, Some(GG455),        None),
    Selector(R400,  Longslit_MOS_IFU1, 760,  850, Some(OG515),        None),
    Selector(R400,  Longslit_MOS_IFU1, 850, 1100, Some(RG610),        None),
    Selector(R600,  Longslit_MOS_IFU1, 350,  640, None,               None),
    Selector(R600,  Longslit_MOS_IFU1, 640,  710, Some(GG455),        None),
    Selector(R600,  Longslit_MOS_IFU1, 710,  870, Some(OG515),        None),
    Selector(R600,  Longslit_MOS_IFU1, 870, 1100, Some(RG610),        None),
    Selector(R150,  Longslit_MOS_IFU1, 350, 1100, Some(GG455),        None),
    Selector(B600,  Longslit_MOS_IFU1, 350,  640, None,               None),
    Selector(B600,  Longslit_MOS_IFU1, 640,  700, Some(GG455),        None),
    Selector(B600,  Longslit_MOS_IFU1, 700,  880, Some(OG515),        None),
    Selector(B600,  Longslit_MOS_IFU1, 880, 1100, Some(RG610),        None),
    Selector(B1200, Longslit_MOS_IFU1, 350,  620, None,               None), // Overfill camera warning if wavelen > 595
    Selector(B1200, Longslit_MOS_IFU1, 620, 1100, Some(OG515),        None), // Overfill camera warning if wavelen > 595
    Selector(R831,  IFU2,              490,  520, Some(GPrime_GG455), Some(505)),
    Selector(R831,  IFU2,              630,  680, Some(RPrime_RG610), Some(655)),
    Selector(R831,  IFU2,              780,  830, Some(IPrime_CaT),   Some(810)),
    Selector(R831,  IFU2,              860,  915, Some(ZPrime_CaT),   Some(885)),
    Selector(R400,  IFU2,              445,  505, Some(GPrime),       Some(475)),
    Selector(R400,  IFU2,              585,  675, Some(RPrime),       Some(630)),
    Selector(R400,  IFU2,              735,  815, Some(IPrime),       Some(780)),
    Selector(R400,  IFU2,              815,  895, Some(CaT),          Some(855)),
    Selector(R400,  IFU2,              930,  975, Some(ZPrime),       Some(940)),
    Selector(R600,  IFU2,              470,  490, Some(GPrime),       Some(475)),
    Selector(R600,  IFU2,              615,  645, Some(RPrime),       Some(630)),
    Selector(R600,  IFU2,              765,  840, Some(IPrime_CaT),   Some(800)),
    Selector(R600,  IFU2,              850,  930, Some(ZPrime_CaT),   Some(885)),
    Selector(R150,  IFU2,              735,  775, Some(GG455),        Some(755)),
    Selector(B600,  IFU2,              470,  490, Some(GPrime),       Some(475)),
    Selector(B600,  IFU2,              615,  645, Some(RPrime),       Some(630)),
    Selector(B600,  IFU2,              770,  840, Some(IPrime_CaT),   Some(800)),
    Selector(B600,  IFU2,              850,  930, Some(ZPrime_CaT),   Some(885)),
    Selector(B1200,  IFU2,             510,  555, Some(GPrime_OG515), Some(535)),
  )

}
