// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.core.`enum`.ObsStatus
import lucuma.core.optics.syntax.lens._
import lucuma.core.model.{Asterism, Observation, Program}
import cats.data.State
import cats.syntax.validated._
import io.circe.Decoder
import io.circe.generic.semiauto._
import monocle.Lens


final case class ObservationModel(
  id:                 Observation.Id,
  existence:          Existence,
  programId:          Program.Id,
  name:               Option[String],
  status:             ObsStatus,
  asterismId:         Option[Asterism.Id],
  plannedTimeSummary: PlannedTimeSummaryModel
)

object ObservationModel extends ObservationOptics {

  implicit val TopLevelObservation: TopLevelModel[Observation.Id, ObservationModel] =
    TopLevelModel.instance(_.id, ObservationModel.existence)

  final case class Create(
    observationId: Option[Observation.Id],
    programId:     Program.Id,
    name:          Option[String],
    asterismId:    Option[Asterism.Id],
    status:        Option[ObsStatus]
  ) {

    def withId(id: Observation.Id, s: PlannedTimeSummaryModel): ObservationModel =
      ObservationModel(
        id,
        Present,
        programId,
        name,
        status.getOrElse(ObsStatus.New),
        asterismId,
        s
      )

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

  }

  final case class Edit(
    observationId: Observation.Id,
    existence:     Option[Existence],
    name:          Option[Option[String]],
    status:        Option[ObsStatus],
    asterismId:    Option[Option[Asterism.Id]]
  ) extends Editor[Observation.Id, ObservationModel] {

    override def id: Observation.Id =
      observationId

    override def editor: ValidatedInput[State[ObservationModel, Unit]] =
      (for {
        _ <- ObservationModel.existence  := existence
        _ <- ObservationModel.name       := name
        _ <- ObservationModel.status     := status
        _ <- ObservationModel.asterismId := asterismId
      } yield ()).validNec

  }

  object Edit {

    implicit val DecoderEdit: Decoder[Edit] =
      deriveDecoder[Edit]

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
    Lens[ObservationModel, Observation.Id](_.id)(a => b => b.copy(id = a))

  val existence: Lens[ObservationModel, Existence] =
    Lens[ObservationModel, Existence](_.existence)(a => b => b.copy(existence = a))

  val name: Lens[ObservationModel, Option[String]] =
    Lens[ObservationModel, Option[String]](_.name)(a => b => b.copy(name = a))

  val status: Lens[ObservationModel, ObsStatus] =
    Lens[ObservationModel, ObsStatus](_.status)(a => b => b.copy(status = a))

  val asterismId: Lens[ObservationModel, Option[Asterism.Id]] =
    Lens[ObservationModel, Option[Asterism.Id]](_.asterismId)(a => b => b.copy(asterismId = a))

}
