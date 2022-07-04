// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.h2

import cats.implicits._
import doobie._
import doobie.free.connection.delay
import doobie.implicits._
import lucuma.core.enums.{GmosAmpCount, GmosAmpGain, GmosAmpReadMode, GmosRoi, GmosXBinning, GmosYBinning, Instrument}
import lucuma.core.syntax.string._
import lucuma.core.util.Enumerated
import lucuma.odb.api.model.GmosModel
import lucuma.odb.api.model.PlannedTime.ConfigChangeType

import java.net.URL
import java.time.Duration

import scala.reflect._

object PlannedTimeDao {

  val init: ConnectionIO[Unit] =
    for {
      _ <- Statements.createEnumTables
      _ <- Statements.createReadoutOverheadTable
      _ <- Statements.createConfigChangeOverheadTable
      _ <- Statements.createDhsWriteOverheadTable
    } yield ()

  def configChangeOverhead(changeType: ConfigChangeType): ConnectionIO[Option[Duration]] =
    Statements.selectConfigChangeOverhead(changeType).option

  def gmosReadoutOverhead(ccd: GmosModel.CcdReadout, roi: GmosRoi): ConnectionIO[Option[Duration]] =
    Statements.selectGmosReadoutOverhead(
      ccd.ampCount,
      ccd.ampRead,
      roi,
      ccd.xBin,
      ccd.yBin,
      ccd.ampGain
    ).option

  def gmosReadoutOverhead(d: GmosModel.NorthDynamic): ConnectionIO[Option[Duration]] =
    gmosReadoutOverhead(d.readout, d.roi)

  def gmosReadoutOverhead(d: GmosModel.SouthDynamic): ConnectionIO[Option[Duration]] =
    gmosReadoutOverhead(d.readout, d.roi)

  def dhsWriteOverhead(instrument: Instrument): ConnectionIO[Option[Duration]] =
    Statements.selectDhsWriteOverhead(instrument).option


  object Statements {

    private type SimpleEnum = {
      def tag: String
      def shortName: String
      def longName: String
    }

    private def createEnumTable[A <: SimpleEnum : ClassTag : Enumerated]: ConnectionIO[Unit] = {
      val table = s"e_${classTag[A].toString.split('.').last.toSnakeCase}"

      val create = Fragment.const(
        s"""
          CREATE TABLE $table (
            id         VARCHAR PRIMARY KEY,
            short_name VARCHAR NOT NULL,
            long_name  VARCHAR NOT NULL
          )
        """).update.run.void

      val insert = s"insert into $table (id, short_name, long_name) values (?, ?, ?)"

      implicit val WriteA: Write[A] =
        Write[(String, String, String)].contramap { a =>
          (a.tag, a.shortName, a.longName)
        }

      for {
        _ <- create
        _ <- Update[A](insert).updateMany(Enumerated[A].all)
      } yield ()
    }

    val createEnumTables: ConnectionIO[Unit] =
      for {
        _ <- createEnumTable[GmosAmpCount]
        _ <- createEnumTable[GmosAmpReadMode]
        _ <- createEnumTable[GmosRoi]
        _ <- createEnumTable[GmosXBinning]
        _ <- createEnumTable[GmosYBinning]
        _ <- createEnumTable[GmosAmpGain]
        _ <- createEnumTable[Instrument]
      } yield ()

    def createOverheadTable(
      create:   String,
      resource: String,
      insert:   URL => String
    ): ConnectionIO[Unit] =
      for {
        _ <- Fragment.const(create).update.run.void
        u <- delay[URL](classOf[PlannedTimeDao.type].getResource(s"/$resource"))
        _ <- Fragment.const(insert(u)).update.run.void
      } yield ()

    val createReadoutOverheadTable: ConnectionIO[Unit] =
      createOverheadTable(
        """
          CREATE TABLE gmos_readout_overhead (
            amp_count     VARCHAR       NOT NULL REFERENCES e_gmos_amp_count,
            amp_read_mode VARCHAR       NOT NULL REFERENCES e_gmos_amp_read_mode,
            roi           VARCHAR       NOT NULL REFERENCES e_gmos_roi,
            x_binning     VARCHAR       NOT NULL REFERENCES e_gmos_x_binning,
            y_binning     VARCHAR       NOT NULL REFERENCES e_gmos_y_binning,
            amp_gain      VARCHAR       NOT NULL REFERENCES e_gmos_amp_gain,
            seconds       NUMERIC(7, 4) NOT NULL,
            PRIMARY KEY (amp_count, amp_read_mode, roi, x_binning, y_binning, amp_gain)
          )
        """,
        "GmosReadoutOverhead.csv",
        u => s"INSERT INTO gmos_readout_overhead ( amp_count, amp_read_mode, roi, x_binning, y_binning, amp_gain, seconds ) SELECT * FROM CSVREAD ('${u.getFile}')"
      )

    val createConfigChangeOverheadTable: ConnectionIO[Unit] =
      createOverheadTable(
        """
          CREATE TABLE config_change_overhead (
             type       VARCHAR       PRIMARY KEY,
             instrument VARCHAR       REFERENCES e_instrument,
             seconds    NUMERIC(8, 5) NOT NULL
          )
        """,
        "ConfigChangeOverhead.csv",
        u => s"INSERT INTO config_change_overhead ( type, instrument, seconds ) SELECT * FROM CSVREAD ('${u.getFile}')"
      )

    val createDhsWriteOverheadTable: ConnectionIO[Unit] =
      createOverheadTable(
        """
          CREATE TABLE dhs_write_overhead (
            instrument VARCHAR      NOT NULL REFERENCES e_instrument,
            seconds    NUMERIC(4,2) NOT NULL,
            PRIMARY KEY (instrument)
          )
        """,
        "DhsWriteOverhead.csv",
        u => s"INSERT INTO dhs_write_overhead ( instrument, seconds ) SELECT * FROM CSVREAD ('${u.getFile}')"
      )

    def selectConfigChangeOverhead(
      changeType: ConfigChangeType
    ): Query0[Duration] =
      sql"""
        SELECT seconds
          FROM config_change
         WHERE type = ${changeType.key}
      """.query[Duration]

    def selectGmosReadoutOverhead(
      ampCount: GmosAmpCount,
      ampReadMode: GmosAmpReadMode,
      roi: GmosRoi,
      xBinning: GmosXBinning,
      yBinning: GmosYBinning,
      ampGain: GmosAmpGain
    ): Query0[Duration] =
      sql"""
        SELECT seconds
          FROM gmos_readout_overhead
         WHERE amp_count     = $ampCount
           AND amp_read_mode = $ampReadMode
           AND roi           = $roi
           AND x_binning     = $xBinning
           AND y_binning     = $yBinning
           AND amp_gain      = $ampGain
      """.query[Duration]

    def selectDhsWriteOverhead(
      instrument: Instrument
    ): Query0[Duration] =
      sql"""
        SELECT seconds
          FROM dhs_write_overhead
         WHERE instrument = $instrument
      """.query[Duration]
  }


}
