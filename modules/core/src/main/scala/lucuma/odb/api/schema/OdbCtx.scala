// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.itc.client.ItcClient
import lucuma.odb.api.repo.OdbRepo

/**
 *
 */
trait OdbCtx[F[_]] {

  def itcClient:  ItcClient

  def odbRepo: OdbRepo[F]

}

object OdbCtx {

  def create[F[_]](
    itc: ItcClient,
    repo: OdbRepo[F]
  ): OdbCtx[F] =

    new OdbCtx[F] {

      override def itcClient: ItcClient =
        itc

      override def odbRepo: OdbRepo[F] =
        repo
    }

}
