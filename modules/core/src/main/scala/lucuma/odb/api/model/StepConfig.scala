// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.core.math.Offset
import lucuma.core.`enum`.StepType
import lucuma.odb.api.model.syntax.inputvalidator._
import cats.{Applicative, Eq, Eval, Traverse}
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import monocle.{Lens, Optional}
import monocle.macros.GenLens

// For now, just bias, dark, gcal and science.  Pending smart-gcal.

sealed abstract class StepConfig[A] extends Product with Serializable {
  def instrumentConfig: A

  def fold[B](
    biasFn:    StepConfig.Bias[A]    => B,
    darkFn:    StepConfig.Dark[A]    => B,
    gcalFn:    StepConfig.Gcal[A]    => B,
    scienceFn: StepConfig.Science[A] => B
  ): B =
    this match {
      case b @ StepConfig.Bias(_)       => biasFn(b)
      case d @ StepConfig.Dark(_)       => darkFn(d)
      case g @ StepConfig.Gcal(_, _)    => gcalFn(g)
      case s @ StepConfig.Science(_, _) => scienceFn(s)
    }

  def bias: Option[StepConfig.Bias[A]] =
    this match {
      case b @ StepConfig.Bias(_) => b.some
      case _                      => none
    }

  def dark: Option[StepConfig.Dark[A]] =
    this match {
      case d @ StepConfig.Dark(_) => d.some
      case _                      => none
    }

  def gcal: Option[StepConfig.Gcal[A]] =
    this match {
      case g @ StepConfig.Gcal(_, _) => g.some
      case _                         => none
    }

  def science: Option[StepConfig.Science[A]] =
    this match {
      case s @ StepConfig.Science(_, _) => s.some
      case _                            => none
    }

  def stepType: StepType =
    fold(
      _ => StepType.Bias,
      _ => StepType.Dark,
      _ => StepType.Gcal,
      _ => StepType.Science
    )

  def gmosNorth: Option[GmosModel.NorthDynamic] =
    instrumentConfig match {
      case gn: GmosModel.NorthDynamic => gn.some
      case _                          => none
    }

  def gmosSouth: Option[GmosModel.SouthDynamic] =
    instrumentConfig match {
      case gs: GmosModel.SouthDynamic => gs.some
      case _                          => none
    }

}

object StepConfig {

  final case class Bias   [A](instrumentConfig: A)                        extends StepConfig[A]
  final case class Dark   [A](instrumentConfig: A)                        extends StepConfig[A]
  final case class Gcal   [A](instrumentConfig: A, gcalConfig: GcalModel) extends StepConfig[A]
  final case class Science[A](instrumentConfig: A, offset: Offset)        extends StepConfig[A]

  implicit val TraverseStepConfig: Traverse[StepConfig] =
    new Traverse[StepConfig] {
      override def traverse[G[_], A, B](fa: StepConfig[A])(f: A => G[B])(implicit ev: Applicative[G]): G[StepConfig[B]] =
        f(fa.instrumentConfig).map { b =>
          fa.fold(
            _    => Bias(b),
            _    => Dark(b),
            gcal => Gcal(b, gcal.gcalConfig),
            sci  => Science(b, sci.offset)
          )
        }

      override def foldLeft[A, B](fa: StepConfig[A], b: B)(f: (B, A) => B): B =
        f(b, fa.instrumentConfig)

      override def foldRight[A, B](fa: StepConfig[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(fa.instrumentConfig, lb)

    }

  def instrumentConfig[A]: Lens[StepConfig[A], A] =
    Lens[StepConfig[A], A](_.instrumentConfig) { a => b => b.as(a) }

  def gcalConfig[A]: Optional[StepConfig[A], GcalModel] =
    Optional[StepConfig[A], GcalModel](_.gcal.map(_.gcalConfig)) { a =>
      _.fold(
        identity,
        identity,
        _.copy(gcalConfig = a),
        identity
      )
    }

  def offset[A]: Optional[StepConfig[A], Offset] =
    Optional[StepConfig[A], Offset](_.science.map(_.offset)) { a =>
      _.fold(
        identity,
        identity,
        identity,
        _.copy(offset = a)
      )
    }

  def bias[A](instrumentConfig: A): StepConfig[A] =
    Bias(instrumentConfig)

  def dark[A](instrumentConfig: A): StepConfig[A] =
    Dark(instrumentConfig)

  def gcal[A](instrumentConfig: A, gcalConfig: GcalModel): StepConfig[A] =
    Gcal(instrumentConfig, gcalConfig)

  def science[A](instrumentConfig: A, offset: Offset): StepConfig[A] =
    Science(instrumentConfig, offset)


  implicit def EqStepModel[A: Eq]: Eq[StepConfig[A]] =
    Eq.instance {
      case (Bias(a), Bias(b))               => a === b
      case (Dark(a), Dark(b))               => a === b
      case (Gcal(a, ga), Gcal(b, gb))       => (a === b) && (ga === gb)
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
    def offset[A]: Lens[CreateScience[A], OffsetModel.Input] = GenLens[CreateScience[A]](_.offset)

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

  final case class CreateGcal[A](
    config: A,
    gcalConfig: GcalModel.Create
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[Gcal[B]] =
      (config.validateAndCreate, gcalConfig.create).mapN { (b, g) => Gcal(b, g) }
  }

  object CreateGcal {
    def gcalConfig[A]: Lens[CreateGcal[A], GcalModel.Create] = GenLens[CreateGcal[A]](_.gcalConfig)

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


  final case class CreateStepConfig[A](
    bias:    Option[CreateBias[A]],
    dark:    Option[CreateDark[A]],
    gcal:    Option[CreateGcal[A]],
    science: Option[CreateScience[A]]
  ) {

    def create[B](implicit V: InputValidator[A, B]): ValidatedInput[StepConfig[B]] =
      ValidatedInput.requireOne(
        "step",
        bias.map    { _.validateAndCreate },
        dark.map    { _.validateAndCreate },
        gcal.map    { _.validateAndCreate },
        science.map { _.validateAndCreate }
      )

  }

  object CreateStepConfig {

    def Empty[A]: CreateStepConfig[A] =
      CreateStepConfig[A](
        None,
        None,
        None,
        None
      )

    implicit def EqCreateStep[A: Eq]: Eq[CreateStepConfig[A]] =
      Eq.by { a => (
        a.bias,
        a.dark,
        a.gcal,
        a.science
      )}

    implicit def DecoderCreateStep[A: Decoder]: Decoder[CreateStepConfig[A]] =
      deriveDecoder[CreateStepConfig[A]]

    implicit def ValidateCreateStep[A, B](implicit ev: InputValidator[A, B]): InputValidator[CreateStepConfig[A], StepConfig[B]] =
      (csa: CreateStepConfig[A]) => csa.create[B]

    def bias[A](a: A): CreateStepConfig[A] =
      Empty[A].copy(bias = Some(CreateBias(a)))

    def dark[A](a: A): CreateStepConfig[A] =
      Empty[A].copy(dark = Some(CreateDark(a)))

    def gcal[A](a: A, g: GcalModel.Create): CreateStepConfig[A] =
      Empty[A].copy(gcal = Some(CreateGcal(a, g)))

    def science[A](a: A, o: OffsetModel.Input): CreateStepConfig[A] =
      Empty[A].copy(science = Some(CreateScience(a, o)))

    def instrumentConfig[A]: Optional[CreateStepConfig[A], A] =
      Optional[CreateStepConfig[A], A] {
        case CreateStepConfig(Some(CreateBias(a)), None, None, None)       => a.some
        case CreateStepConfig(None, Some(CreateDark(a)), None, None)       => a.some
        case CreateStepConfig(None, None, Some(CreateGcal(a, _)), None)    => a.some
        case CreateStepConfig(None, None, None, Some(CreateScience(a, _))) => a.some
        case _                                                             => None
      }{ a => {
        case CreateStepConfig(Some(CreateBias(_)), None, None, None)       => CreateStepConfig.bias(a)
        case CreateStepConfig(None, Some(CreateDark(_)), None, None)       => CreateStepConfig.dark(a)
        case CreateStepConfig(None, None, Some(CreateGcal(_, g)), None)    => CreateStepConfig.gcal(a, g)
        case CreateStepConfig(None, None, None, Some(CreateScience(_, o))) => CreateStepConfig.science(a, o)
        case other                                                         => other
      }}

    /**
     * Optional for gcal configuration.
     */
    def gcalConfig[A]: Optional[CreateStepConfig[A], GcalModel.Create] =
      Optional[CreateStepConfig[A], GcalModel.Create](_.gcal.map(_.gcalConfig)) { a => c =>
        c.copy(gcal = c.gcal.map(CreateGcal.gcalConfig[A].replace(a)))
      }

    /**
     * Optional for science step offset.
     */
    def offset[A]: Optional[CreateStepConfig[A], OffsetModel.Input] =
      Optional[CreateStepConfig[A], OffsetModel.Input](_.science.map(_.offset)) { a => c =>
        c.copy(science = c.science.map(CreateScience.offset[A].replace(a)))
      }

    def p[A]: Optional[CreateStepConfig[A], OffsetModel.ComponentInput] =
      offset[A].andThen(OffsetModel.Input.p)

    def q[A]: Optional[CreateStepConfig[A], OffsetModel.ComponentInput] =
      offset[A].andThen(OffsetModel.Input.q)
  }

}
