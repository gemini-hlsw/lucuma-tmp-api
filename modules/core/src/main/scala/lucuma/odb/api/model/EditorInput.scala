// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.syntax.either._
import clue.data.Input
import lucuma.odb.api.model.syntax.prism._
import monocle.Prism

import scala.reflect.ClassTag

/**
 * Describes how to validate arguments and create a new A or else validate
 * arguments and edit an existing A.
 *
 * @tparam A type of the editing target
 */
trait EditorInput[A] { self =>

  def create: ValidatedInput[A]

  def edit: StateT[EitherInput, A, Unit]

  def imap[B](f: A => B, g: B => A): EditorInput[B] =

    new EditorInput[B] {
      override def create: ValidatedInput[B] =
        self.create.map(f)

      override def edit: StateT[EitherInput, B, Unit] =
        self.edit.transformS(g, (_, a) => f(a))
    }

}

object EditorInput {

  /**
   * Provides an editor of a sum type `S` with two mutually exclusive options
   * `A1` and `A2`.  When the existing object is an `A1`, an edit for `A1` may
   * be used (which implies only supplying arguments that need to change) but
   * to switch to an `A2` requires creation (which implies specifying all
   * required `A2` inputs).  Similarly when the existing object is an instance
   * of `A2` an editor for `A2` is used but to switch to `A1` requires
   * creation.
   *
   * @param in1 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in2 triplet containing name of the input field, the input itself, and a prism for editing
   *
   * @tparam S sum type
   * @tparam A1 type of option 1
   * @tparam A2 type of option 2
   *
   * @return editor of the sum type
   */
  def editOneOf[S, A1 <: S, A2 <: S](
    in1: (String, Input[EditorInput[A1]], Prism[S, A1]),
    in2: (String, Input[EditorInput[A2]], Prism[S, A2]),
  )(implicit ev1: ClassTag[A1], ev2: ClassTag[A2]): StateT[EitherInput, S, Unit] = {

    def fold(
      a1: StateT[EitherInput, S, Unit],
      a2: StateT[EitherInput, S, Unit]
    ): StateT[EitherInput, S, Unit] =
      StateT.get[EitherInput, S].flatMap {
        case _: A1 => a1
        case _: A2 => a2
        case _     => StateT.setF(InputError.fromMessage(s"Unexpected type, not one of $ev1 or $ev2").leftNec)
      }

    val (name1, input1, prism1) = in1
    val (name2, input2, prism2) = in2

    (input1.toOption, input2.toOption) match {
      case (Some(a1), None    ) =>
        fold(prism1.editOrIgnore(a1), prism1.create(a1))

      case (None,     Some(a2)) =>
        fold(prism2.create(a2), prism2.editOrIgnore(a2))

      case _                    =>
        StateT.setF(InputError.fromMessage(s"""exactly one of "$name1" or "$name2" must be set """).leftNec)
    }
  }

  /**
   * Provides an editor of a sum type `S` with three mutually exclusive options
   * `A1`, `A2` and `A3`.  See `editOnOf` with two arguments above.
   *
   * @param in1 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in2 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in3 triplet containing name of the input field, the input itself, and a prism for editing
   *
   * @tparam S sum type
   * @tparam A1 type of option 1
   * @tparam A2 type of option 2
   * @tparam A3 type of option 3
   *
   * @return editor of the sum type
   */
  def editOneOf[S, A1 <: S, A2 <: S, A3 <: S](
    in1: (String, Input[EditorInput[A1]], Prism[S, A1]),
    in2: (String, Input[EditorInput[A2]], Prism[S, A2]),
    in3: (String, Input[EditorInput[A3]], Prism[S, A3])
  )(implicit ev1: ClassTag[A1], ev2: ClassTag[A2], ev3: ClassTag[A3]): StateT[EitherInput, S, Unit] = {

    def fold(
      a1: StateT[EitherInput, S, Unit],
      a2: StateT[EitherInput, S, Unit],
      a3: StateT[EitherInput, S, Unit]
    ): StateT[EitherInput, S, Unit] =
      StateT.get[EitherInput, S].flatMap {
        case _: A1 => a1
        case _: A2 => a2
        case _: A3 => a3
        case _     => StateT.setF(InputError.fromMessage(s"Unexpected type, not one of $ev1, $ev2 or $ev3").leftNec)
      }

    val (name1, input1, prism1) = in1
    val (name2, input2, prism2) = in2
    val (name3, input3, prism3) = in3

    (input1.toOption, input2.toOption, input3.toOption) match {
      case (Some(a1), None,     None    ) =>
        fold(prism1.editOrIgnore(a1), prism1.create(a1), prism1.create(a1))

      case (None,     Some(a2), None    ) =>
        fold(prism2.create(a2), prism2.editOrIgnore(a2), prism2.create(a2))

      case (None,     None,     Some(a3)) =>
        fold(prism3.create(a3), prism3.create(a3), prism3.editOrIgnore(a3))

      case _                              =>
        StateT.setF(InputError.fromMessage(s"""exactly one of "$name1", "$name2" or $name3" must be set """).leftNec)
    }
  }

}
