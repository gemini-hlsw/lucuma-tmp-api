// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.itc.client.ItcClient
import lucuma.odb.api.repo.OdbRepo

/**
 * Sangria context object passed into the resolvers. Contains the "repo" for
 * the program and observation data along with any service clients that the
 * ODB needs to resolve queries.
 */
trait OdbCtx[F[_]] {

  def itcClient:  ItcClient[F]

  def odbRepo: OdbRepo[F]

}

object OdbCtx {

  def create[F[_]](
    itc: ItcClient[F],
    repo: OdbRepo[F]
  ): OdbCtx[F] =

    new OdbCtx[F] {

      override def itcClient: ItcClient[F] =
        itc

      override def odbRepo: OdbRepo[F] =
        repo
    }

}
