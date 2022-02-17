// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model.targetModel

import lucuma.core.math.Coordinates
import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.StateT
import cats.syntax.all._
import clue.data.Input
import clue.data.syntax._
import io.circe.Decoder
import lucuma.core.model.{Program, Target}
import lucuma.core.util.Gid
import lucuma.odb.api.model.{CoordinatesModel, Database, EditorInput, EitherInput, InputError, ValidatedInput}
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import monocle.Lens

import scala.collection.immutable.SortedSet

final case class TargetEnvironmentModel(
  asterism:     SortedSet[Target.Id],
  explicitBase: Option[Coordinates]
) {

  def validate2(
    pid: Program.Id
  ): StateT[EitherInput, Database, List[TargetModel]] =
    Database
      .target
      .lookupAll(asterism.toList)
      .flatMapF { ts =>
        if (ts.forall(_.programId === pid)) ts.rightNec[InputError]
        else InputError.fromMessage(s"Cannot assign targets from programs other than ${Gid[Program.Id].show(pid)}").leftNec
      }

}

object TargetEnvironmentModel extends TargetEnvironmentModelOptics {

  val empty: TargetEnvironmentModel =
    TargetEnvironmentModel(SortedSet.empty[Target.Id], None)

  implicit val EqTargetEnvironmentModel: Eq[TargetEnvironmentModel] =
    Eq.by { tem =>
      (
        tem.asterism,
        tem.explicitBase
      )
    }

}


trait TargetEnvironmentModelOptics { self: TargetEnvironmentModel.type =>

  val asterism: Lens[TargetEnvironmentModel, SortedSet[Target.Id]] =
    Lens[TargetEnvironmentModel, SortedSet[Target.Id]](_.asterism)(a => _.copy(asterism = a))

  val explicitBase: Lens[TargetEnvironmentModel, Option[Coordinates]] =
    Lens[TargetEnvironmentModel, Option[Coordinates]](_.explicitBase)(a => _.copy(explicitBase = a))

}

final case class TargetEnvironmentInput(
  asterism:     Input[List[Target.Id]]        = Input.ignore,
  explicitBase: Input[CoordinatesModel.Input] = Input.ignore
) extends EditorInput[TargetEnvironmentModel] {

  override val create: ValidatedInput[TargetEnvironmentModel] =
    explicitBase.toOption.traverse(_.toCoordinates).map { b =>
      TargetEnvironmentModel(SortedSet.from(asterism.toOption.toList.flatten), b)
    }

  override val edit: StateT[EitherInput, TargetEnvironmentModel, Unit] =
    for {
      b <- explicitBase.validateNullable(_.toCoordinates).liftState
      _ <- TargetEnvironmentModel.explicitBase := b
      _ <- TargetEnvironmentModel.asterism     := asterism.toOption.map(SortedSet.from(_))
    } yield ()

}

object TargetEnvironmentInput {

  val Empty: TargetEnvironmentInput =
    TargetEnvironmentInput(Input.ignore, Input.ignore)

  def explicitBase(c: CoordinatesModel.Input): TargetEnvironmentInput =
    TargetEnvironmentInput(Input.ignore, c.assign)

  def asterism(tids: Target.Id*): TargetEnvironmentInput =
    asterism(tids.toList)

  def asterism(tids: List[Target.Id]): TargetEnvironmentInput =
    TargetEnvironmentInput(tids.assign, Input.ignore)

  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.Configuration
  implicit val customConfig: Configuration = Configuration.default.withDefaults

  implicit val DecoderTargetEnvironmentInput: Decoder[TargetEnvironmentInput] =
    deriveConfiguredDecoder[TargetEnvironmentInput]

  implicit val EqTargetEnvironmentInput: Eq[TargetEnvironmentInput] =
    Eq.by { a => (
      a.explicitBase,
      a.asterism
    )}

}

