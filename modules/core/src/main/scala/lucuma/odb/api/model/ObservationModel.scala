// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.input._
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.optics.syntax.lens._
import lucuma.core.model.{Asterism, Observation, Program, Target}
import cats.{Eq, Monad}
import cats.data.{Nested, State}
import cats.mtl.Stateful
import cats.syntax.all._
import clue.data.{Assign, Input}
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import io.circe.{Decoder, HCursor}
import io.circe.generic.semiauto._
import io.circe.refined._
import monocle.{Focus, Lens, Optional}

import scala.collection.immutable.SortedSet

final case class ObservationModel(
  id:                   Observation.Id,
  existence:            Existence,
  programId:            Program.Id,
  name:                 Option[NonEmptyString],
  status:               ObsStatus,
  activeStatus:         ObsActiveStatus,
  pointing:             Option[Either[Asterism.Id, Target.Id]],
  constraintSet:        ConstraintSetModel,
  scienceRequirements:  ScienceRequirements,
  plannedTimeSummary:   PlannedTimeSummaryModel,
  config:               Option[InstrumentConfigModel.Reference]
) {

  def asterismId: Option[Asterism.Id] =
    pointing.flatMap(_.swap.toOption)

  def targetId: Option[Target.Id] =
    pointing.flatMap(_.toOption)

}

object ObservationModel extends ObservationOptics {

  implicit val TopLevelObservation: TopLevelModel[Observation.Id, ObservationModel] =
    TopLevelModel.instance(_.id, ObservationModel.existence)

  implicit val EqObservation: Eq[ObservationModel] =
    Eq.by { o => (
      o.id,
      o.existence,
      o.programId,
      o.name,
      o.status,
      o.activeStatus,
      o.pointing,
      o.constraintSet,
      o.scienceRequirements,
      o.plannedTimeSummary,
      o.config
    )}


  final case class Create(
    observationId:       Option[Observation.Id],
    programId:           Program.Id,
    name:                Option[NonEmptyString],
    status:              Option[ObsStatus],
    activeStatus:        Option[ObsActiveStatus],
    asterismId:          Option[Asterism.Id],
    targetId:            Option[Target.Id],
    constraintSet:       Option[ConstraintSetModel.Create],
    scienceRequirements: Option[ScienceRequirementsModel.Create],
    config:              Option[InstrumentConfigModel.Create]
  ) {

    def create[F[_]: Monad, T](db: DatabaseState[T], s: PlannedTimeSummaryModel)(implicit S: Stateful[F, T]): F[ValidatedInput[ObservationModel]] =
      for {
        i <- db.observation.getUnusedId(observationId)
        p <- db.program.lookupValidated(programId)
        a <- asterismId.traverse(db.asterism.lookupValidated[F]).map(_.sequence)
        t <- targetId.traverse(db.target.lookupValidated[F]).map(_.sequence)

        pointing = ValidatedInput.optionEither(
          "asterismId",
          "targetId",
          Nested(a).map(_.id).value,
          Nested(t).map(_.id).value
        )
        c  = constraintSet.traverse(_.create)
        q  = scienceRequirements.traverse(_.create)

        g <- config.traverse(_.create(db)).map(_.sequence)

        o  = (i, p, pointing, c, q, g).mapN { (iʹ, _, pointingʹ, cʹ, qʹ, gʹ) =>
          ObservationModel(
            iʹ,
            Present,
            programId,
            name,
            status.getOrElse(ObsStatus.New),
            activeStatus.getOrElse(ObsActiveStatus.Active),
            pointingʹ,
            cʹ.getOrElse(ConstraintSetModel.AnyConstraints),
            qʹ.getOrElse(ScienceRequirements.Default),
            s,
            gʹ.map(_.toReference)
          )
        }

        _ <- db.observation.saveIfValid(o)(_.id)
      } yield o

  }

  object Create {

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.observationId,
        a.programId,
        a.name,
        a.status,
        a.activeStatus,
        a.asterismId,
        a.targetId,
        a.constraintSet,
        a.scienceRequirements,
        a.config
      )}

  }

  final case class Edit(
    observationId:       Observation.Id,
    existence:           Input[Existence]                      = Input.ignore,
    name:                Input[NonEmptyString]                 = Input.ignore,
    status:              Input[ObsStatus]                      = Input.ignore,
    activeStatus:        Input[ObsActiveStatus]                = Input.ignore,
    asterismId:          Input[Asterism.Id]                    = Input.ignore,
    targetId:            Input[Target.Id]                      = Input.ignore,
    constraintSet:       Option[ConstraintSetModel.Edit]       = None,
    scienceRequirements: Option[ScienceRequirementsModel.Edit] = None
  ) {

    def id: Observation.Id =
      observationId

    def pointing: ValidatedInput[(Input[Asterism.Id], Input[Target.Id])] =
      (asterismId, targetId) match {
        case (Assign(_), Assign(_)) => InputError.fromMessage(s"Cannot assign both an asterism and a target to the observation").invalidNec
        case _                      => (asterismId, targetId).validNec
      }

    def editor: ValidatedInput[State[ObservationModel, Unit]] =
      (existence   .validateIsNotNull("existence"),
       status      .validateIsNotNull("status"),
       activeStatus.validateIsNotNull("active"),
       constraintSet.traverse(_.editor),
       scienceRequirements.traverse(_.editor)
      ).mapN { (e, s, a, c, r) =>
        for {
          _ <- ObservationModel.existence    := e
          _ <- ObservationModel.name         := name.toOptionOption
          _ <- ObservationModel.status       := s
          _ <- ObservationModel.activeStatus := a
          _ <- State.modify[ObservationModel] { o =>
            c.fold(o) { ed => ObservationModel.constraintSet.modify(ed.runS(_).value)(o) }
          }
          _ <- State.modify[ObservationModel] { o =>
            r.fold(o) { ed => ObservationModel.scienceRequirements.modify(ed.runS(_).value)(o) }
          }
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
        a.activeStatus,
        a.asterismId,
        a.targetId,
        a.constraintSet
      )}

  }

  final case class EditPointing(
    observationIds: List[Observation.Id],
    asterismId:     Option[Asterism.Id],
    targetId:       Option[Target.Id]
  ) {

    def pointing: ValidatedInput[Option[Either[Asterism.Id, Target.Id]]] =
      (asterismId, targetId) match {
        case (Some(_), Some(_)) => InputError.fromMessage("Cannot assign both an asterism and a target to the observation").invalidNec[Option[Either[Asterism.Id, Target.Id]]]
        case (Some(a), None)    => a.asLeft[Target.Id].some.validNec[InputError]
        case (None,    Some(t)) => t.asRight[Asterism.Id].some.validNec[InputError]
        case (None,    None)    => Option.empty[Either[Asterism.Id, Target.Id]].validNec
      }

  }

  object EditPointing {

    def unassign(observationIds: List[Observation.Id]): EditPointing =
      EditPointing(observationIds, None, None)

    def assignAsterism(observationIds: List[Observation.Id], asterismId: Asterism.Id): EditPointing =
      unassign(observationIds).copy(asterismId = Some(asterismId))

    def assignTarget(observationIds: List[Observation.Id], targetId: Target.Id): EditPointing =
      unassign(observationIds).copy(targetId = Some(targetId))

    implicit val DecoderEditPointing: Decoder[EditPointing] =
      deriveDecoder[EditPointing]

    implicit val EqEditPointing: Eq[EditPointing] =
      Eq.by { a => (
        a.observationIds,
        a.asterismId,
        a.targetId
      )}

  }

  final case class ObservationEvent (
    id:       Long,
    editType: Event.EditType,
    value:    ObservationModel,
  ) extends Event.Edit[ObservationModel]

  object ObservationEvent {
    def created(value: ObservationModel)(id: Long): ObservationEvent =
      ObservationEvent(id, Event.EditType.Created, value)

    def updated(value: ObservationModel)(id: Long): ObservationEvent =
      ObservationEvent(id, Event.EditType.Updated, value)
  }

  /**
   * A grouping of observations according to some commonly held value.
   *
   * @param value value held in common
   * @param observationIds observations sharing the same value
   */
  final case class Group[A](
    value:          A,
    observationIds: SortedSet[Observation.Id]
  )

  object Group {

    implicit def EqGroup[A: Eq]: Eq[Group[A]] =
      Eq.by { a => (
        a.value,
        a.observationIds
      )}

  }

  final case class BulkEdit[A](
    input:          A,
    observationIds: List[Observation.Id]
  )

  object BulkEdit {

    def decoder[A: Decoder](
      editorName: String
    ): Decoder[BulkEdit[A]] =
      (c: HCursor) =>
        for {
          input <- c.downField(editorName).as[A]
          oids  <- c.downField("observationIds").as[List[Observation.Id]]
        } yield BulkEdit(input, oids)

    implicit def EqBulkEdit[A: Eq]: Eq[BulkEdit[A]] =
      Eq.by { a => (
        a.input,
        a.observationIds
      )}

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

  val activeStatus: Lens[ObservationModel, ObsActiveStatus] =
    Lens[ObservationModel, ObsActiveStatus](_.activeStatus)(a => _.copy(activeStatus = a))

  val pointing: Lens[ObservationModel, Option[Either[Asterism.Id, Target.Id]]] =
    Lens[ObservationModel, Option[Either[Asterism.Id, Target.Id]]](_.pointing)(a => _.copy(pointing = a))

  val asterism: Optional[ObservationModel, Asterism.Id] =
    Optional[ObservationModel, Asterism.Id](_.pointing.flatMap(_.swap.toOption)) { a =>
      _.copy(pointing = a.asLeft[Target.Id].some)
    }

  val target: Optional[ObservationModel, Target.Id] =
    Optional[ObservationModel, Target.Id](_.pointing.flatMap(_.toOption)) { t =>
      _.copy(pointing = t.asRight[Asterism.Id].some)
    }

  val scienceRequirements: Lens[ObservationModel, ScienceRequirements] =
    Focus[ObservationModel](_.scienceRequirements)

  val constraintSet: Lens[ObservationModel, ConstraintSetModel] =
    Lens[ObservationModel, ConstraintSetModel](_.constraintSet)(a => _.copy(constraintSet = a))

  val config: Lens[ObservationModel, Option[InstrumentConfigModel.Reference]] =
    Lens[ObservationModel, Option[InstrumentConfigModel.Reference]](_.config)(a => _.copy(config = a))

}
