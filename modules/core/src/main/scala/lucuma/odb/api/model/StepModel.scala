// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Offset
import lucuma.core.`enum`.StepType
import lucuma.odb.api.model.syntax.inputvalidator._

import cats.Eq
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

// For now, just bias, dark, and science.  Pending smart-gcal and gcal.

sealed abstract class StepModel[A] extends Product with Serializable {
  def dynamicConfig: A

  def fold[B](
    biasFn:    StepModel.Bias[A]    => B,
    darkFn:    StepModel.Dark[A]    => B,
    scienceFn: StepModel.Science[A] => B
  ): B =
    this match {
      case b @ StepModel.Bias(_)       => biasFn(b)
      case d @ StepModel.Dark(_)       => darkFn(d)
      case s @ StepModel.Science(_, _) => scienceFn(s)
    }

  def bias: Option[StepModel.Bias[A]] =
    this match {
      case b @ StepModel.Bias(_) => Some(b)
      case _                     => None
    }

  def dark: Option[StepModel.Dark[A]] =
    this match {
      case d @ StepModel.Dark(_) => Some(d)
      case _                     => None
    }

  def science: Option[StepModel.Science[A]] =
    this match {
      case s @ StepModel.Science(_, _) => Some(s)
      case _                           => None
    }

  def stepType: StepType =
    fold(
      _ => StepType.Bias,
      _ => StepType.Dark,
      _ => StepType.Science
    )

}

object StepModel {

  final case class Bias     [A](dynamicConfig: A)                 extends StepModel[A]
  final case class Dark     [A](dynamicConfig: A)                 extends StepModel[A]
  final case class Science  [A](dynamicConfig: A, offset: Offset) extends StepModel[A]

  def bias[A](dynamicConfig: A): StepModel[A] =
    Bias(dynamicConfig)

  def dark[A](dynamicConfig: A): StepModel[A] =
    Dark(dynamicConfig)

  def science[A](dynamicConfig: A, offset: Offset): StepModel[A] =
    Science(dynamicConfig, offset)


  implicit def EqStepModel[A: Eq]: Eq[StepModel[A]] =
    Eq.instance {
      case (Bias(a), Bias(b))               => a === b
      case (Dark(a), Dark(b))               => a === b
      case (Science(a, oa), Science(b, ob)) => (a === b) && (oa === ob)
      case _                                => false
    }

  final case class CreateScience[A](
    config: A,
    offset: OffsetModel.Input
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[Science[B]] =
      (config.validateAndCreate, offset.create).mapN { (c, o) => Science(c, o) }

  }

  object CreateScience {

    implicit def EqCreateScience[A: Eq]: Eq[CreateScience[A]] =
      Eq.by { a => (
        a.config,
        a.offset
      )}

    implicit def DecoderCreateScience[A: Decoder]: Decoder[CreateScience[A]] =
      deriveDecoder[CreateScience[A]]

    implicit def ValidatorCreateScience[A, B](implicit V: InputValidator[A, B]): InputValidator[CreateScience[A], Science[B]] =
      (csa: CreateScience[A]) => csa.create[B]

  }

  final case class CreateBias[A](config: A) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[Bias[B]] =
      config.validateAndCreate.map(Bias(_))

  }

  object CreateBias {

    implicit def EqCreateBias[A: Eq]: Eq[CreateBias[A]] =
      Eq.by(_.config)

    implicit def DecoderCreateBias[A: Decoder]: Decoder[CreateBias[A]] =
      deriveDecoder[CreateBias[A]]

    implicit def ValidateCreateBias[A, B](implicit V: InputValidator[A, B]): InputValidator[CreateBias[A], Bias[B]] =
      (cba: CreateBias[A]) => cba.create[B]

  }

  final case class CreateDark[A](config: A) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[Dark[B]] =
      config.validateAndCreate.map(Dark(_))

  }

  object CreateDark {

    implicit def EqCreateDark[A: Eq]: Eq[CreateDark[A]] =
      Eq.by(_.config)

    implicit def DecoderCreateDark[A: Decoder]: Decoder[CreateDark[A]] =
      deriveDecoder[CreateDark[A]]

    implicit def ValidateCreateDark[A, B](implicit ev: InputValidator[A, B]): InputValidator[CreateDark[A], Dark[B]] =
      (cda: CreateDark[A]) => cda.create[B]

  }

  final case class CreateStep[A](
    bias:    Option[CreateBias[A]],
    dark:    Option[CreateDark[A]],
    science: Option[CreateScience[A]]
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[StepModel[B]] =
      ValidatedInput.requireOne(
        "step",
        bias.map    { _.validateAndCreate },
        dark.map    { _.validateAndCreate },
        science.map { _.validateAndCreate }
      )

  }

  object CreateStep {

    implicit def EqCreateStep[A: Eq]: Eq[CreateStep[A]] =
      Eq.by { a => (
        a.bias,
        a.dark,
        a.science
      )}

    implicit def DecoderCreateStep[A: Decoder]: Decoder[CreateStep[A]] =
      deriveDecoder[CreateStep[A]]

    implicit def ValidateCreateStep[A, B](implicit ev: InputValidator[A, B]): InputValidator[CreateStep[A], StepModel[B]] =
      (csa: CreateStep[A]) => csa.create[B]

  }

  /*
  val x =
    """
      |"gmos": {
      |  "static": ...
      |  "acquisition": [
      |
      |  ],
      |  "sequence": [
      |    {
      |      "bias": {
      |        "filter" : ...
      |      }
      |    }
      |  ]
      |
      |}
      |""".stripMargin
   */
}