// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.StepConfig.CreateStepConfig
import cats.Eq
import cats.data.State
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.macros.Lenses


@Lenses final case class StepModel[A](
  id:         Step.Id,
  breakpoint: Breakpoint,
  config:     StepConfig[A]
)

object StepModel {
  implicit def EqStepModel[A: Eq]: Eq[StepModel[A]] =
    Eq.by { a => (
      a.id,
      a.breakpoint,
      a.config
    )}

  @Lenses final case class Create[A](
    id:         Option[Step.Id],
    breakpoint: Breakpoint,
    config:     CreateStepConfig[A]
  ) {

    // Presumably IdSupply turns into something more capable --
    //
    // * id lookup
    // * id creation when necessary
    // * registering newly created stuff in the item map

    def create[T, B](db: Database[T])(implicit V: InputValidator[A, B]): State[T, ValidatedInput[StepModel[B]]] =
      for {
        i <- db.step.getUnusedId(id)
        o  = (i, config.create[B]).mapN { (i, c) => StepModel(i, breakpoint, c) }
        _ <- db.step.saveValid(o)(_.id)
      } yield o

    // ^^^ this is still somehow not quite right.  what if there were other
    //     ids to lookup?  In the case of ObservationModel you have to make sure
    //     that target / asterism, constraint set, etc. really exist.

    // So the second argument, instead of just being a Validated[Id => StepModel[D]]
    // should maybe be a full fledged State[T, Validated[Id => StepModel[D]]
    // though the "simpler" form above could exist as a convenience.
    //
    // So for an observation maybe:
    //
    // store.storeObservation(
    //   id,
    //   for {
    //     a <- store.definedAsterismId(aid)  // State[T, ValidatedInput[Asterism.Id]] though aid may be None ...
    //     ....
    //   } yield (a, blah, baz).mapN { (x, y, z) => ObservationModel(...) }
    // }

    // Think this "Store[T] thing should be moved to TableState and we should
    // delete the IdSupply in Ids.


  }

  object Create {

    def stopBefore[A](s: CreateStepConfig[A]): Create[A] =
      Create(None, Breakpoint.enabled, s)

    def continueTo[A](s: CreateStepConfig[A]): Create[A] =
      Create(None, Breakpoint.disabled, s)

    implicit def EqCreate[A: Eq]: Eq[Create[A]] =
      Eq.by { a => (
        a.id,
        a.breakpoint,
        a.config
      )}

    implicit def DecoderCreate[A: Decoder]: Decoder[Create[A]] =
      deriveDecoder[Create[A]]

  }

}