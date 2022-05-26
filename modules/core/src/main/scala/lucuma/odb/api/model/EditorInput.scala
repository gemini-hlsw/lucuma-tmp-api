// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import cats.data.StateT
import cats.syntax.either._
import cats.syntax.functor._
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

  private def create[S, A <: S](a: EditorInput[A]): StateT[EitherInput, S, Unit] =
    StateT.setF(a.create.widen[S].toEither)

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
      a1: => StateT[EitherInput, S, Unit],
      a2: => StateT[EitherInput, S, Unit]
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
        fold(prism1.editOrIgnore(a1), create(a1))

      case (None,     Some(a2)) =>
        fold(create(a2), prism2.editOrIgnore(a2))

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
      a1: => StateT[EitherInput, S, Unit],
      a2: => StateT[EitherInput, S, Unit],
      a3: => StateT[EitherInput, S, Unit]
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
        fold(prism1.editOrIgnore(a1), create(a1), create(a1))

      case (None,     Some(a2), None    ) =>
        fold(create(a2), prism2.editOrIgnore(a2), create(a2))

      case (None,     None,     Some(a3)) =>
        fold(create(a3), create(a3), prism3.editOrIgnore(a3))

      case _                              =>
        StateT.setF(InputError.fromMessage(s"""exactly one of "$name1", "$name2" or $name3" must be set """).leftNec)
    }
  }

  /**
   * Provides an editor of a sum type `S` with ten mutually exclusive options
   * `A1`, `A2`, ..., `AA`.  See `editOnOf` with two arguments above.
   *
   * @param in1 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in2 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in3 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in4 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in5 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in6 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in7 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in8 triplet containing name of the input field, the input itself, and a prism for editing
   * @param in9 triplet containing name of the input field, the input itself, and a prism for editing
   * @param inA triplet containing name of the input field, the input itself, and a prism for editing
   *
   * @tparam S sum type
   * @tparam A1 type of option 1
   * @tparam A2 type of option 2
   * @tparam A3 type of option 3
   * @tparam A4 type of option 4
   * @tparam A5 type of option 5
   * @tparam A6 type of option 6
   * @tparam A7 type of option 7
   * @tparam A8 type of option 8
   * @tparam A9 type of option 9
   * @tparam AA type of option 10
   *
   * @return editor of the sum type
   */
  def editOneOf[S, A1 <: S, A2 <: S, A3 <: S, A4 <: S, A5 <: S, A6 <: S, A7 <: S, A8 <: S, A9 <: S, AA <: S](
    in1: (String, Input[EditorInput[A1]], Prism[S, A1]),
    in2: (String, Input[EditorInput[A2]], Prism[S, A2]),
    in3: (String, Input[EditorInput[A3]], Prism[S, A3]),
    in4: (String, Input[EditorInput[A4]], Prism[S, A4]),
    in5: (String, Input[EditorInput[A5]], Prism[S, A5]),
    in6: (String, Input[EditorInput[A6]], Prism[S, A6]),
    in7: (String, Input[EditorInput[A7]], Prism[S, A7]),
    in8: (String, Input[EditorInput[A8]], Prism[S, A8]),
    in9: (String, Input[EditorInput[A9]], Prism[S, A9]),
    inA: (String, Input[EditorInput[AA]], Prism[S, AA])
  )(implicit ev1: ClassTag[A1],
             ev2: ClassTag[A2],
             ev3: ClassTag[A3],
             ev4: ClassTag[A4],
             ev5: ClassTag[A5],
             ev6: ClassTag[A6],
             ev7: ClassTag[A7],
             ev8: ClassTag[A8],
             ev9: ClassTag[A9],
             evA: ClassTag[AA]): StateT[EitherInput, S, Unit] = {

    def fold(
      a1: => StateT[EitherInput, S, Unit],
      a2: => StateT[EitherInput, S, Unit],
      a3: => StateT[EitherInput, S, Unit],
      a4: => StateT[EitherInput, S, Unit],
      a5: => StateT[EitherInput, S, Unit],
      a6: => StateT[EitherInput, S, Unit],
      a7: => StateT[EitherInput, S, Unit],
      a8: => StateT[EitherInput, S, Unit],
      a9: => StateT[EitherInput, S, Unit],
      aA: => StateT[EitherInput, S, Unit]
    ): StateT[EitherInput, S, Unit] =
      StateT.get[EitherInput, S].flatMap {
        case _: A1 => a1
        case _: A2 => a2
        case _: A3 => a3
        case _: A4 => a4
        case _: A5 => a5
        case _: A6 => a6
        case _: A7 => a7
        case _: A8 => a8
        case _: A9 => a9
        case _: AA => aA
        case _     => StateT.setF(InputError.fromMessage(s"Unexpected type, not one of $ev1, $ev2, $ev3, $ev4, $ev5, $ev6, $ev7, $ev8, $ev9 or $evA").leftNec)
      }

    val (name1, input1, prism1) = in1
    val (name2, input2, prism2) = in2
    val (name3, input3, prism3) = in3
    val (name4, input4, prism4) = in4
    val (name5, input5, prism5) = in5
    val (name6, input6, prism6) = in6
    val (name7, input7, prism7) = in7
    val (name8, input8, prism8) = in8
    val (name9, input9, prism9) = in9
    val (nameA, inputA, prismA) = inA

    (input1.toOption, input2.toOption, input3.toOption, input4.toOption, input5.toOption, input6.toOption, input7.toOption, input8.toOption, input9.toOption, inputA.toOption) match {
      case (Some(a1), None,     None,     None,     None,     None,     None,     None,     None,     None    ) =>
        fold(prism1.editOrIgnore(a1), create(a1), create(a1), create(a1), create(a1), create(a1), create(a1), create(a1), create(a1), create(a1))

      case (None,     Some(a2), None,     None,     None,     None,     None,     None,     None,     None    ) =>
        fold(create(a2), prism2.editOrIgnore(a2), create(a2), create(a2), create(a2), create(a2), create(a2), create(a2), create(a2), create(a2))

      case (None,     None,     Some(a3), None,     None,     None,     None,     None,     None,     None    ) =>
        fold(create(a3), create(a3), prism3.editOrIgnore(a3), create(a3), create(a3), create(a3), create(a3), create(a3), create(a3), create(a3))

      case (None,     None,     None,     Some(a4), None,     None,     None,     None,     None,     None    ) =>
        fold(create(a4), create(a4), create(a4), prism4.editOrIgnore(a4), create(a4), create(a4), create(a4), create(a4), create(a4), create(a4))

      case (None,     None,     None,     None,     Some(a5), None,     None,     None,     None,     None    ) =>
        fold(create(a5), create(a5), create(a5), create(a5), prism5.editOrIgnore(a5), create(a5), create(a5), create(a5), create(a5), create(a5))

      case (None,     None,     None,     None,     None,     Some(a6), None,     None,     None,     None    ) =>
        fold(create(a6), create(a6), create(a6), create(a6), create(a6), prism6.editOrIgnore(a6), create(a6), create(a6), create(a6), create(a6))

      case (None,     None,     None,     None,     None,     None,     Some(a7), None,     None,     None    ) =>
        fold(create(a7), create(a7), create(a7), create(a7), create(a7), create(a7), prism7.editOrIgnore(a7), create(a7), create(a7), create(a7))

      case (None,     None,     None,     None,     None,     None,     None,     Some(a8), None,     None    ) =>
        fold(create(a8), create(a8), create(a8), create(a8), create(a8), create(a8), create(a8), prism8.editOrIgnore(a8), create(a8), create(a8))

      case (None,     None,     None,     None,     None,     None,     None,     None,     Some(a9), None    ) =>
        fold(create(a9), create(a9), create(a9), create(a9), create(a9), create(a9), create(a9), create(a9), prism9.editOrIgnore(a9), create(a9))

      case (None,     None,     None,     None,     None,     None,     None,     None,     None,     Some(aA)) =>
        fold(create(aA), create(aA), create(aA), create(aA), create(aA), create(aA), create(aA), create(aA), create(aA), prismA.editOrIgnore(aA))

      case _                              =>
        StateT.setF(InputError.fromMessage(s"""exactly one of "$name1", "$name2", "$name3", "$name4", "$name5", "$name6", "$name7", "$name8", "$name9" or $nameA" must be set """).leftNec)
    }
  }

}
