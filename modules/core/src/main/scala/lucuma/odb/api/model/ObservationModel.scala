// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.input._
import lucuma.core.`enum`.ObsStatus
import lucuma.core.optics.syntax.lens._
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats.Eq
import cats.data.State
import cats.syntax.all._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Lens
import monocle.state.all._


final case class ObservationModel(
  id:                 Observation.Id,
  existence:          Existence,
  programId:          Program.Id,
  name:               Option[NonEmptyString],
  status:             ObsStatus,
  targets:            Option[Either[Asterism.Id, Target.Id]],
  plannedTimeSummary: PlannedTimeSummaryModel,
  config:             Option[ConfigModel]
)

object ObservationModel extends ObservationOptics {

  implicit val TopLevelObservation: TopLevelModel[Observation.Id, ObservationModel] =
    TopLevelModel.instance(_.id, ObservationModel.existence)

  implicit val EqObservation: Eq[ObservationModel] =
    Eq.by(o => (o.id, o.existence, o.programId, o.name, o.status, o.targets, o.plannedTimeSummary, o.config))


  final case class Create(
    observationId: Option[Observation.Id],
    programId:     Program.Id,
    name:          Option[String],
    asterismId:    Option[Asterism.Id],
    targetId:      Option[Target.Id],
    status:        Option[ObsStatus],
    config:        Option[ConfigModel.Create]
  ) {

    def withId(s: PlannedTimeSummaryModel): ValidatedInput[Observation.Id => ObservationModel] =
      (
        name.traverse(ValidatedInput.nonEmptyString("name", _)),
        ValidatedInput.optionEither("asterismId", "targetId", asterismId.map(_.validNec), targetId.map(_.validNec)),
        config.traverse(_.create)
      ).mapN { (n, t, c) => oid =>
        ObservationModel(
          oid,
          Present,
          programId,
          n,
          status.getOrElse(ObsStatus.New),
          t,
          s,
          c
        )
      }

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.observationId,
        a.programId,
        a.name,
        a.asterismId,
        a.targetId,
        a.status
      )}

  }

  final case class Edit(
    observationId: Observation.Id,
    existence:     Input[Existence]   = Input.ignore,
    name:          Input[String]      = Input.ignore,
    status:        Input[ObsStatus]   = Input.ignore,
    asterismId:    Input[Asterism.Id] = Input.ignore,
    targetId:      Input[Target.Id]   = Input.ignore
  ) extends Editor[Observation.Id, ObservationModel] {

    override def id: Observation.Id =
      observationId

    override def editor: ValidatedInput[State[ObservationModel, Unit]] =
      (existence.validateIsNotNull("existence"),
       name     .validateNullable(n => ValidatedInput.nonEmptyString("name", n)),
       status   .validateIsNotNull("status"),
       Input.validateNullableEither("asterismId", "targetId", asterismId, targetId)
      ).mapN { (e, n, s, f) =>
        for {
          _ <- ObservationModel.existence  := e
          _ <- ObservationModel.name       := n
          _ <- ObservationModel.status     := s
          _ <- ObservationModel.targets.mod_(f)
        } yield ()
      }

  }

  object Edit {

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderEdit: Decoder[Edit] =
      deriveConfiguredDecoder[Edit]

    implicit val EqEdit: Eq[Edit] =
      Eq.by{ a => (
        a.observationId,
        a.existence,
        a.name,
        a.status,
        a.asterismId
      )}

  }

  final case class ObservationEvent (
    id:       Long,
    editType: Event.EditType,
    value:    ObservationModel,
  ) extends Event.Edit[ObservationModel]

  object ObservationEvent {
    def apply(editType: Event.EditType, value: ObservationModel)(id: Long): ObservationEvent =
      ObservationEvent(id, editType, value)
  }

}

trait ObservationOptics { self: ObservationModel.type =>

  val id: Lens[ObservationModel, Observation.Id] =
    Lens[ObservationModel, Observation.Id](_.id)(a => _.copy(id = a))

  val existence: Lens[ObservationModel, Existence] =
    Lens[ObservationModel, Existence](_.existence)(a => _.copy(existence = a))

  val name: Lens[ObservationModel, Option[NonEmptyString]] =
    Lens[ObservationModel, Option[NonEmptyString]](_.name)(a => _.copy(name = a))

  val status: Lens[ObservationModel, ObsStatus] =
    Lens[ObservationModel, ObsStatus](_.status)(a => _.copy(status = a))

  val targets: Lens[ObservationModel, Option[Either[Asterism.Id, Target.Id]]] =
    Lens[ObservationModel, Option[Either[Asterism.Id, Target.Id]]](_.targets)(a => _.copy(targets = a))

  val config: Lens[ObservationModel, Option[ConfigModel]] =
    Lens[ObservationModel, Option[ConfigModel]](_.config)(a => _.copy(config = a))

}
