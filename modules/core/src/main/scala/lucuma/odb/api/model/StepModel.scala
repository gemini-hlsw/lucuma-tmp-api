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
import monocle.{Lens, Optional}
import monocle.macros.Lenses

// For now, just bias, dark, gcal and science.  Pending smart-gcal.

sealed abstract class StepModel[A] extends Product with Serializable {
  def instrumentConfig: A

  def fold[B](
    biasFn:    StepModel.Bias[A]    => B,
    darkFn:    StepModel.Dark[A]    => B,
    gcalFn:    StepModel.Gcal[A]    => B,
    scienceFn: StepModel.Science[A] => B
  ): B =
    this match {
      case b @ StepModel.Bias(_)       => biasFn(b)
      case d @ StepModel.Dark(_)       => darkFn(d)
      case g @ StepModel.Gcal(_, _)    => gcalFn(g)
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

  def gcal: Option[StepModel.Gcal[A]] =
    this match {
      case g @ StepModel.Gcal(_, _) => Some(g)
      case _                        => None
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
      _ => StepType.Gcal,
      _ => StepType.Science
    )

}

object StepModel {

  @Lenses final case class Bias   [A](instrumentConfig: A)                        extends StepModel[A]
  @Lenses final case class Dark   [A](instrumentConfig: A)                        extends StepModel[A]
  @Lenses final case class Gcal   [A](instrumentConfig: A, gcalConfig: GcalModel) extends StepModel[A]
  @Lenses final case class Science[A](instrumentConfig: A, offset: Offset)        extends StepModel[A]

  def instrumentConfig[A]: Lens[StepModel[A], A] =
    Lens[StepModel[A], A](_.instrumentConfig) { a =>
      _.fold(
        _.copy(instrumentConfig = a),
        _.copy(instrumentConfig = a),
        _.copy(instrumentConfig = a),
        _.copy(instrumentConfig = a)
      )
    }

  def gcalConfig[A]: Optional[StepModel[A], GcalModel] =
    Optional[StepModel[A], GcalModel](_.gcal.map(_.gcalConfig)) { a =>
      _.fold(
        identity,
        identity,
        _.copy(gcalConfig = a),
        identity
      )
    }

  def offset[A]: Optional[StepModel[A], Offset] =
    Optional[StepModel[A], Offset](_.science.map(_.offset)) { a =>
      _.fold(
        identity,
        identity,
        identity,
        _.copy(offset = a)
      )
    }

  def bias[A](instrumentConfig: A): StepModel[A] =
    Bias(instrumentConfig)

  def dark[A](instrumentConfig: A): StepModel[A] =
    Dark(instrumentConfig)

  def gcal[A](instrumentConfig: A, gcalConfig: GcalModel): StepModel[A] =
    Gcal(instrumentConfig, gcalConfig)

  def science[A](instrumentConfig: A, offset: Offset): StepModel[A] =
    Science(instrumentConfig, offset)


  implicit def EqStepModel[A: Eq]: Eq[StepModel[A]] =
    Eq.instance {
      case (Bias(a), Bias(b))               => a === b
      case (Dark(a), Dark(b))               => a === b
      case (Gcal(a, ga), Gcal(b, gb))       => (a === b) && (ga === gb)
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

  @Lenses final case class CreateGcal[A](
    config: A,
    gcalConfig: GcalModel.Create
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[Gcal[B]] =
      (config.validateAndCreate, gcalConfig.create).mapN { (b, g) => Gcal(b, g) }
  }

  object CreateGcal {

    implicit def EqCreateGcal[A: Eq]: Eq[CreateGcal[A]] =
      Eq.by { a => (
        a.config,
        a.gcalConfig
      )}

    implicit def DecoderCreateGcal[A: Decoder]: Decoder[CreateGcal[A]] =
      deriveDecoder[CreateGcal[A]]

    implicit def ValidatorCreateScience[A, B](implicit V: InputValidator[A, B]): InputValidator[CreateGcal[A], Gcal[B]] =
      (cga: CreateGcal[A]) => cga.create[B]

  }


  @Lenses final case class CreateStep[A](
    bias:    Option[CreateBias[A]],
    dark:    Option[CreateDark[A]],
    gcal:    Option[CreateGcal[A]],
    science: Option[CreateScience[A]]
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[StepModel[B]] =
      ValidatedInput.requireOne(
        "step",
        bias.map    { _.validateAndCreate },
        dark.map    { _.validateAndCreate },
        gcal.map    { _.validateAndCreate },
        science.map { _.validateAndCreate }
      )

  }

  object CreateStep {

    def Empty[A]: CreateStep[A] =
      CreateStep[A](
        None,
        None,
        None,
        None
      )

    implicit def EqCreateStep[A: Eq]: Eq[CreateStep[A]] =
      Eq.by { a => (
        a.bias,
        a.dark,
        a.gcal,
        a.science
      )}

    implicit def DecoderCreateStep[A: Decoder]: Decoder[CreateStep[A]] =
      deriveDecoder[CreateStep[A]]

    implicit def ValidateCreateStep[A, B](implicit ev: InputValidator[A, B]): InputValidator[CreateStep[A], StepModel[B]] =
      (csa: CreateStep[A]) => csa.create[B]

    def bias[A](a: A): CreateStep[A] =
      Empty[A].copy(bias = Some(CreateBias(a)))

    def dark[A](a: A): CreateStep[A] =
      Empty[A].copy(dark = Some(CreateDark(a)))

    def gcal[A](a: A, g: GcalModel.Create): CreateStep[A] =
      Empty[A].copy(gcal = Some(CreateGcal(a, g)))

    def science[A](a: A, o: OffsetModel.Input): CreateStep[A] =
      Empty[A].copy(science = Some(CreateScience(a, o)))

    def instrumentConfig[A]: Optional[CreateStep[A], A] =
      Optional[CreateStep[A], A] {
        case CreateStep(Some(CreateBias(a)), None, None, None)       => a.some
        case CreateStep(None, Some(CreateDark(a)), None, None)       => a.some
        case CreateStep(None, None, Some(CreateGcal(a, _)), None)    => a.some
        case CreateStep(None, None, None, Some(CreateScience(a, _))) => a.some
        case _                                                       => None
      }{ a => {
        case CreateStep(Some(CreateBias(_)), None, None, None)       => CreateStep.bias(a)
        case CreateStep(None, Some(CreateDark(_)), None, None)       => CreateStep.dark(a)
        case CreateStep(None, None, Some(CreateGcal(_, g)), None)    => CreateStep.gcal(a, g)
        case CreateStep(None, None, None, Some(CreateScience(_, o))) => CreateStep.science(a, o)
        case other                                                   => other
      }}

    /**
     * Optional for gcal configuration.
     */
    def gcalConfig[A]: Optional[CreateStep[A], GcalModel.Create] =
      Optional[CreateStep[A], GcalModel.Create](_.gcal.map(_.gcalConfig)) { a => c =>
        c.copy(gcal = c.gcal.map(CreateGcal.gcalConfig[A].set(a)))
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

}