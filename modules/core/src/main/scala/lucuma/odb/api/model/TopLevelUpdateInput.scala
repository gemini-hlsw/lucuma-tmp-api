// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.{NonEmptyChain, StateT}
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import eu.timepit.refined.types.all.NonNegInt
import lucuma.odb.api.model.query.{SizeLimitedResult, WherePredicate}

/**
 * An input for updating a top-level item (program, observation, target).
 */
trait TopLevelUpdateInput[I, A] {

  def typeName: String

  def editOne: StateT[EitherInput, A, Unit]

  def WHERE: Option[WherePredicate[A]]

  def LIMIT: Option[NonNegInt]

  def state: DatabaseState[I, A]

  def filteredValues: StateT[EitherInput, Database, List[A]] =
    StateT.inspect[EitherInput, Database, List[A]] { db =>
      val all = state.lens.get(db).rows.values
      WHERE.fold(all)(where => all.filter(where.matches)).toList
    }

  def editAll(as: List[A])(implicit ev: TopLevelModel[I, A]): StateT[EitherInput, Database, List[A]] =
    StateT.liftF[EitherInput, Database, List[A]] {
      as.traverse { a =>
        // If there are errors, we want to keep up with the associated ids.
        // Turn this into validated as well in order to not short-circuit
        editOne.runS(a).leftMap(_.tupleRight(ev.id(a))).toValidated
      }.leftMap { nec =>
        NonEmptyChain.fromNonEmptyList(
          nec
            .zipWithIndex // want to keep up with the order that the errors were found
            .map { case ((error, id), index) => (error.message, (id, index)) }
            .groupMap(_._1)(_._2) // make a Map keyed by error message -> Nec((id, index))
            .toNel
            .sortBy(_._2.head._2) // order by first time error appeared
            .map { case (msg, idsAndIdxs) =>
              val idList   = idsAndIdxs.toList.map(_._1) // we just want the ids
              val idString = idList.take(10).mkString("", ", ", if (idList.sizeIs >  10) " ..." else "")
              val prefix   = s"$typeName${if (idList.sizeIs == 1) "" else "s"}"
              InputError.fromMessage(s"$prefix $idString: $msg")
            }
        )
      }.toEither
    }


  def editor(implicit ev: TopLevelModel[I, A]): StateT[EitherInput, Database, SizeLimitedResult.Update[A]] =
    for {
      as  <- filteredValues
      asʹ <- editAll(as)
      _   <- asʹ.traverse(a => state.update(ev.id(a), a))
    } yield SizeLimitedResult.Update.fromAll(asʹ, LIMIT)

}
