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
import monocle.{Lens, Optional, Setter}
import monocle.macros.Lenses

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

  @Lenses final case class Bias     [A](dynamicConfig: A)                 extends StepModel[A]
  @Lenses final case class Dark     [A](dynamicConfig: A)                 extends StepModel[A]
  @Lenses final case class Science  [A](dynamicConfig: A, offset: Offset) extends StepModel[A]

  def dynamicConfig[A]: Lens[StepModel[A], A] =
    Lens[StepModel[A], A](_.dynamicConfig) { a =>
      _.fold(
        _.copy(dynamicConfig = a),
        _.copy(dynamicConfig = a),
        _.copy(dynamicConfig = a)
      )
    }

  def offset[A]: Optional[StepModel[A], Offset] =
    Optional[StepModel[A], Offset](_.science.map(_.offset)) { a =>
      _.fold(
        identity,
        identity,
        _.copy(offset = a)
      )
    }

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

  @Lenses final case class CreateScience[A](
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

  @Lenses final case class CreateBias[A](config: A) {

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

  @Lenses final case class CreateDark[A](config: A) {

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

  @Lenses final case class CreateStep[A](
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

    def Empty[A]: CreateStep[A] =
      CreateStep[A](
        None,
        None,
        None
      )

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

    def bias[A](b: CreateBias[A]): CreateStep[A] =
      Empty[A].copy(bias = Some(b))

    def dark[A](d: CreateDark[A]): CreateStep[A] =
      Empty[A].copy(dark = Some(d))

    def science[A](s: CreateScience[A]): CreateStep[A] =
      Empty[A].copy(science = Some(s))

    /**
     * Sets the instrument configuration in the step, whether it is a bias,
     * dark, etc.
     */
    def config[A]: Setter[CreateStep[A], A] =
      Setter[CreateStep[A], A] { f => c =>
        CreateStep(
          c.bias.map(CreateBias.config.modify(f)),
          c.dark.map(CreateDark.config.modify(f)),
          c.science.map(CreateScience.config.modify(f))
        )
      }

    /**
     * Optional for science step offset.
     */
    def offset[A]: Optional[CreateStep[A], OffsetModel.Input] =
      Optional[CreateStep[A], OffsetModel.Input](_.science.map(_.offset)) { a => c =>
        c.copy(science = c.science.map(CreateScience.offset[A].set(a)))
      }

    def p[A]: Optional[CreateStep[A], OffsetModel.ComponentInput] =
      offset[A] ^|-> OffsetModel.Input.p

    def q[A]: Optional[CreateStep[A], OffsetModel.ComponentInput] =
      offset[A] ^|-> OffsetModel.Input.q
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