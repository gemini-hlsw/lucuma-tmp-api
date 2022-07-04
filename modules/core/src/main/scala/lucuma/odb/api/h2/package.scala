// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api

import doobie.Meta
import lucuma.core.util.Enumerated

import java.time.Duration

package object h2 {

  implicit val MetaDuration: Meta[Duration] =
    Meta[BigDecimal].timap { bd =>
      val secs = bd.longValue
      val nano = (bd - secs).underlying.movePointRight(9).longValue
      Duration.ofSeconds(secs, nano)
    } { duration =>
      BigDecimal(duration.getSeconds) +
      BigDecimal(duration.getNano).underlying.movePointLeft(9)
    }

  implicit def MetaEnumerated[A: Enumerated]: Meta[A] =
    Meta[String].timap(Enumerated[A].unsafeFromTag)(Enumerated[A].tag)

}
