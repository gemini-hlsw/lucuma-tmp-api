// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.targetModel.{TargetEnvironmentInput, TargetEnvironmentModel}
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{Observation, Program}
import cats.{Eq, Functor}
import cats.data.StateT
import cats.effect.Sync
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import monocle.{Focus, Lens}

import scala.collection.immutable.SortedSet


final case class ObservationModel(
  id:                   Observation.Id,
  existence:            Existence,
  programId:            Program.Id,
  name:                 Option[NonEmptyString],
  status:               ObsStatus,
  activeStatus:         ObsActiveStatus,
  targetEnvironment:    TargetEnvironmentModel,
  constraintSet:        ConstraintSetModel,
  scienceRequirements:  ScienceRequirements,
  scienceConfiguration: Option[ScienceConfigurationModel],
  config:               Option[InstrumentConfigModel],
  plannedTimeSummary:   PlannedTimeSummaryModel
) {

  val validate: StateT[EitherInput, Database, ObservationModel] =
    targetEnvironment.validate(programId).as(this)

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
      o.targetEnvironment,
      o.constraintSet,
      o.scienceRequirements,
      o.scienceConfiguration,
      o.config,
      o.plannedTimeSummary
    )}


  final case class Create(
    observationId:        Option[Observation.Id],
    programId:            Program.Id,
    name:                 Option[NonEmptyString],
    status:               Option[ObsStatus],
    activeStatus:         Option[ObsActiveStatus],
    targetEnvironment:    Option[TargetEnvironmentInput],
    constraintSet:        Option[ConstraintSetInput],
    scienceRequirements:  Option[ScienceRequirementsInput],
    scienceConfiguration: Option[ScienceConfigurationInput],
    config:               Option[InstrumentConfigModel.Create]
  ) {

    def create[F[_]: Sync](
      s: PlannedTimeSummaryModel
    ): F[StateT[EitherInput, Database, ObservationModel]] =
      config.traverse(_.create).map { g =>
        for {
          i <- Database.observation.getUnusedKey(observationId)
          _ <- Database.program.lookup(programId)
          t  = targetEnvironment.getOrElse(TargetEnvironmentInput.Empty).create
          c  = constraintSet.traverse(_.create)
          q  = scienceRequirements.traverse(_.create)
          u  = scienceConfiguration.traverse(_.create)
          o  = (t, c, q, u, g.sequence).mapN { (tʹ, cʹ, qʹ, uʹ, gʹ) =>
            ObservationModel(
              id                   = i,
              existence            = Present,
              programId            = programId,
              name                 = name,
              status               = status.getOrElse(ObsStatus.New),
              activeStatus         = activeStatus.getOrElse(ObsActiveStatus.Active),
              targetEnvironment    = tʹ,
              constraintSet        = cʹ.getOrElse(ConstraintSetModel.Default),
              scienceRequirements  = qʹ.getOrElse(ScienceRequirements.Default),
              scienceConfiguration = uʹ,
              config               = gʹ,
              plannedTimeSummary   = s
            )
          }
          oʹ <- o.traverse(_.validate)
          _  <- Database.observation.saveNewIfValid(oʹ)(_.id)
          v  <- Database.observation.lookup(i)
        } yield v
      }
  }

  object Create {

    def empty(programId: Program.Id): Create =
      Create(
        observationId        = None,
        programId            = programId,
        name                 = None,
        status               = None,
        activeStatus         = None,
        targetEnvironment    = None,
        constraintSet        = None,
        scienceRequirements  = None,
        scienceConfiguration = None,
        config               = None
      )

    implicit val DecoderCreate: Decoder[Create] =
      deriveDecoder[Create]

    implicit val EqCreate: Eq[Create] =
      Eq.by { a => (
        a.observationId,
        a.programId,
        a.name,
        a.status,
        a.activeStatus,
        a.targetEnvironment,
        a.constraintSet,
        a.scienceRequirements,
        a.scienceConfiguration,
        a.config
      )}

  }

  final case class Edit(
    observationId:        Observation.Id,
    existence:            Input[Existence]                 = Input.ignore,
    name:                 Input[NonEmptyString]            = Input.ignore,
    status:               Input[ObsStatus]                 = Input.ignore,
    activeStatus:         Input[ObsActiveStatus]           = Input.ignore,
    targetEnvironment:    Input[TargetEnvironmentInput]    = Input.ignore,
    constraintSet:        Input[ConstraintSetInput]        = Input.ignore,
    scienceRequirements:  Input[ScienceRequirementsInput]  = Input.ignore,
    scienceConfiguration: Input[ScienceConfigurationInput] = Input.ignore
  ) {

    val edit: StateT[EitherInput, ObservationModel, Unit] = {
      val validArgs =
        (existence   .validateIsNotNull("existence"),
         status      .validateIsNotNull("status"),
         activeStatus.validateIsNotNull("active")
        ).tupled

      for {
        args <- validArgs.liftState
        (e, s, a) = args
        _ <- ObservationModel.existence            := e
        _ <- ObservationModel.name                 := name.toOptionOption
        _ <- ObservationModel.status               := s
        _ <- ObservationModel.activeStatus         := a
        _ <- ObservationModel.targetEnvironment    :! targetEnvironment
        _ <- ObservationModel.constraintSet        :! constraintSet
        _ <- ObservationModel.scienceRequirements  :! scienceRequirements
        _ <- ObservationModel.scienceConfiguration :? scienceConfiguration
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
      Eq.by { a => (
        a.observationId,
        a.existence,
        a.name,
        a.status,
        a.activeStatus,
        a.targetEnvironment,
        a.constraintSet,
        a.scienceRequirements,
        a.scienceConfiguration
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

    implicit def FunctorGroup: Functor[Group] =
      new Functor[Group] {
        override def map[A, B](fa: Group[A])(f: A => B): Group[B] =
          Group(f(fa.value), fa.observationIds)
      }

    def from[A](
      value:          A,
      observationIds: IterableOnce[Observation.Id]
    ): Group[A] =
      Group(value, SortedSet.from(observationIds))

  }

  final case class BulkEdit[A](
    selectObservations: Option[List[Observation.Id]],
    selectProgram:      Option[Program.Id],
    edit:               A
  )

  object BulkEdit {

    def observations[A](oids: List[Observation.Id], edit: A): BulkEdit[A] =
      BulkEdit(oids.some, None, edit)

    def program[A](pid: Program.Id, edit: A): BulkEdit[A] =
      BulkEdit(None, pid.some, edit)

    implicit def DecoderBulkEdit[A: Decoder]: Decoder[BulkEdit[A]] =
      deriveDecoder[BulkEdit[A]]

    implicit def EqBulkEdit[A: Eq]: Eq[BulkEdit[A]] =
      Eq.by { a => (
        a.selectObservations,
        a.selectProgram,
        a.edit
      )}

  }
}

trait ObservationOptics { self: ObservationModel.type =>

  val id: Lens[ObservationModel, Observation.Id] =
    Focus[ObservationModel](_.id)

  val existence: Lens[ObservationModel, Existence] =
    Focus[ObservationModel](_.existence)

  val name: Lens[ObservationModel, Option[NonEmptyString]] =
    Focus[ObservationModel](_.name)

  val status: Lens[ObservationModel, ObsStatus] =
    Focus[ObservationModel](_.status)

  val activeStatus: Lens[ObservationModel, ObsActiveStatus] =
    Focus[ObservationModel](_.activeStatus)

  val targetEnvironment: Lens[ObservationModel, TargetEnvironmentModel] =
    Focus[ObservationModel](_.targetEnvironment)

  val constraintSet: Lens[ObservationModel, ConstraintSetModel] =
    Focus[ObservationModel](_.constraintSet)

  val scienceRequirements: Lens[ObservationModel, ScienceRequirements] =
    Focus[ObservationModel](_.scienceRequirements)

  val scienceConfiguration: Lens[ObservationModel, Option[ScienceConfigurationModel]] =
    Focus[ObservationModel](_.scienceConfiguration)

  val config: Lens[ObservationModel, Option[InstrumentConfigModel]] =
    Focus[ObservationModel](_.config)

}
