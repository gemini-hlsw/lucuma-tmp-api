// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.repo

/*
import lucuma.odb.api.model.syntax.validatedinput._

import cats.Monad
import cats.data.State
import cats.effect.concurrent.Ref
import cats.syntax.all._
import lucuma.odb.api.model.{AsterismModel, Sharing, ValidatedInput}
import monocle.Lens
import monocle.state.all._
*/

trait SharingSupport {
/*
  protected def sharingUpdate[I, IM, J, JM](
    tablesRef: Ref[F, Tables],
    input:     Sharing[I, J],
    findI:     I => State[Tables, ValidatedInput[IM]],
    findJ:     J => State[Tables, ValidatedInput[JM]],
    lens:      Lens[Tables, ManyToMany[I, J]],
    update:    ManyToMany[I, J] => ManyToMany[I, J]
 ): F[AsterismModel] =
    tablesRef.modifyState {
      for {
        im  <- findI(input.one)
        jms <- input.many.traverse(findJ).map(_.sequence)
        res <- (jms, im).traverseN { (_, m) => lens.mod_(update).as(m) }
      } yield res
    }.flatMap(_.liftTo[F])
*/
}
