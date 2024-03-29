// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric._
import io.circe.{Decoder, DecodingFailure, HCursor}
import lucuma.core.model.NonNegDuration
import lucuma.core.syntax.time._

import java.math.MathContext

sealed trait ItcResult {

  def resultType: String =
    this match {
      case ItcResult.Error(_)         => ItcResult.Error.ResultType
      case ItcResult.Success(_, _, _) => ItcResult.Success.ResultType
    }

  def toEither: Either[ItcResult.Error, ItcResult.Success] =
    this match {
      case e: ItcResult.Error   => e.asLeft[ItcResult.Success]
      case s: ItcResult.Success => s.asRight[ItcResult.Error]
    }

  def error: Option[ItcResult.Error] =
    toEither.swap.toOption

  def success: Option[ItcResult.Success] =
    toEither.toOption

}

object ItcResult {

  implicit val DecoderItcResult: Decoder[ItcResult] =
    (c: HCursor) =>
      c.downField("resultType").as[String].flatMap {
        case "Success" => c.as[Success].widen[ItcResult]
        case "Error"   => c.as[Error].widen[ItcResult]
        case rt        => DecodingFailure(s"Couldn't parse ItcResult as success or error: $rt", c.history).asLeft
      }

  implicit val EqItcResult: Eq[ItcResult] =
    Eq.instance {
      case (s0: Success, s1: Success) => s0 === s1
      case (e0: Error, e1: Error)     => e0 === e1
      case _                          => false
    }

  final case class Error(
    msg: String
  ) extends ItcResult

  object Error {

    val ResultType: String =
      "Error"

    implicit val DecoderError: Decoder[Error] =
      (c: HCursor) =>
        for {
          m <- c.downField("msg").as[String]
        } yield Error(m)

    implicit val EqError: Eq[Error] =
      Eq.by(_.msg)
  }

  final case class Success(
    exposureTime:  NonNegDuration,
    exposures:     NonNegInt,
    signalToNoise: PosBigDecimal
  ) extends ItcResult {

    def stepSignalToNoise: PosBigDecimal =
      PosBigDecimal.unsafeFrom(
        (signalToNoise.value * signalToNoise.value / exposures.value)
          .underlying()
          .sqrt(MathContext.DECIMAL128)
      )

  }

  object Success {

    val ResultType: String =
      "Success"

    implicit val DecoderSuccess: Decoder[Success] =
      (c: HCursor) =>
        for {
          t <- c.downField("exposureTime").downField("microseconds").as[Long].flatMap(l => NonNegDuration.from(l.microseconds).leftMap(m => DecodingFailure(m, c.history)))
          n <- c.downField("exposures").as[Int].flatMap(n => NonNegInt.from(n).leftMap(m => DecodingFailure(m, c.history)))
          s <- c.downField("signalToNoise").as[BigDecimal].flatMap(d => PosBigDecimal.from(d).leftMap(m => DecodingFailure(m, c.history)))
        } yield Success(t, n, s)

    implicit val EqSuccess: Eq[Success] =
      Eq.by { a => (
        a.exposureTime.value.toNanos,
        a.exposures,
        a.signalToNoise.value
      )}

  }

  def error(msg: String): ItcResult =
    Error(msg)

  def success(exposureTime: NonNegDuration, exposures: NonNegInt, signalToNoise: PosBigDecimal): ItcResult =
    Success(exposureTime, exposures, signalToNoise)

}
