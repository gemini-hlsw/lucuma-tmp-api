// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

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
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import org.typelevel.cats.time._
import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.targetModel.{TargetEnvironmentInput, TargetEnvironmentModel}
import lucuma.core.enums.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{ConstraintSet, Observation, Program}
import monocle.{Focus, Lens, Optional}

import java.time.Instant

import scala.collection.immutable.SortedSet


final case class ObservationModel(
  id:                  Observation.Id,
  programId:           Program.Id,
  existence:           Existence,
  subtitle:            Option[NonEmptyString],
  status:              ObsStatus,
  activeStatus:        ObsActiveStatus,
  visualizationTime:   Option[Instant],
  posAngleConstraint:  Option[PosAngleConstraint],
  targetEnvironment:   TargetEnvironmentModel,
  constraintSet:       ConstraintSet,
  scienceRequirements: ScienceRequirements,
  scienceMode:         Option[ScienceMode],
  manualConfig:        Option[ExecutionModel]
) {

  val validate: StateT[EitherInput, Database, ObservationModel] =
    targetEnvironment.validate(programId).as(this)

  def clone(newId: Observation.Id): ObservationModel =
    copy(
      id         = newId,
      existence  = Existence.Present,
      status     = ObsStatus.New
    )
}


object ObservationModel extends ObservationOptics {

  implicit val TopLevelObservation: TopLevelModel[Observation.Id, ObservationModel] =
    TopLevelModel.instance(_.id, ObservationModel.existence)

  implicit val EqObservation: Eq[ObservationModel] =
    Eq.by { o => (
      o.id,
      o.existence,
      o.programId,
      o.subtitle,
      o.status,
      o.activeStatus,
      o.visualizationTime,
      o.posAngleConstraint,
      o.targetEnvironment,
      o.constraintSet,
      o.scienceRequirements,
      o.scienceMode,
      o.manualConfig
    )}

  final case class PropertiesInput(
    subtitle:            Input[NonEmptyString]            = Input.ignore,
    status:              Input[ObsStatus]                 = Input.ignore,
    activeStatus:        Input[ObsActiveStatus]           = Input.ignore,
    visualizationTime:   Input[Instant]                   = Input.ignore,
    posAngleConstraint:  Input[PosAngleConstraintInput]   = Input.ignore,
    targetEnvironment:   Input[TargetEnvironmentInput]    = Input.ignore,
    constraintSet:       Input[ConstraintSetInput]        = Input.ignore,
    scienceRequirements: Input[ScienceRequirementsInput]  = Input.ignore,
    scienceMode:         Input[ScienceModeInput]          = Input.ignore,
    manualConfig:        Input[ExecutionModel.Create]     = Input.ignore,
    existence:           Input[Existence]                 = Input.ignore
  ) {

    def create(
      observationId: Observation.Id,
      programId:     Program.Id,
      config:        ValidatedInput[Option[ExecutionModel]]
    ): ValidatedInput[ObservationModel] = {
      val p = posAngleConstraint.toOption.traverse(_.create)
      val t = targetEnvironment.toOption.getOrElse(TargetEnvironmentInput.Empty).create
      val c = constraintSet.toOption.traverse(_.create)
      val q = scienceRequirements.toOption.traverse(_.create)
      val u = scienceMode.toOption.traverse(_.create)
      (p, t, c, q, u, config).mapN { (pʹ, tʹ, cʹ, qʹ, uʹ, gʹ) =>
        ObservationModel(
          id                  = observationId,
          existence           = existence.toOption.getOrElse(Existence.Present),
          programId           = programId,
          subtitle            = subtitle.toOption,
          status              = status.toOption.getOrElse(ObsStatus.New),
          activeStatus        = activeStatus.toOption.getOrElse(ObsActiveStatus.Active),
          visualizationTime   = visualizationTime.toOption,
          posAngleConstraint  = pʹ,
          targetEnvironment   = tʹ,
          constraintSet       = cʹ.getOrElse(ConstraintSetModel.Default),
          scienceRequirements = qʹ.getOrElse(ScienceRequirements.Default),
          scienceMode         = uʹ,
          manualConfig        = gʹ
        )
      }
    }

    val edit: StateT[EitherInput, ObservationModel, Unit] = {
      val validArgs =
        (existence.validateIsNotNull("existence"),
         status.validateIsNotNull("status"),
         activeStatus.validateIsNotNull("active")
        ).tupled

      for {
        args <- validArgs.liftState
        (e, s, a) = args
        _ <- ObservationModel.existence           := e
        _ <- ObservationModel.subtitle            := subtitle.toOptionOption
        _ <- ObservationModel.status              := s
        _ <- ObservationModel.activeStatus        := a
        _ <- ObservationModel.visualizationTime   := visualizationTime.toOptionOption
        _ <- ObservationModel.posAngleConstraint  :? posAngleConstraint
        _ <- ObservationModel.targetEnvironment   :! targetEnvironment
        _ <- ObservationModel.constraintSet       :! constraintSet
        _ <- ObservationModel.scienceRequirements :! scienceRequirements
        _ <- ObservationModel.scienceMode         :? scienceMode
      } yield ()
    }

  }

  object PropertiesInput {

    val Empty: PropertiesInput =
      PropertiesInput()

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderPropertiesInput: Decoder[PropertiesInput] =
      deriveConfiguredDecoder[PropertiesInput]

    implicit val EqPropertiesInput: Eq[PropertiesInput] =
      Eq.by { a => (
        a.subtitle,
        a.status,
        a.activeStatus,
        a.visualizationTime,
        a.posAngleConstraint,
        a.targetEnvironment,
        a.constraintSet,
        a.scienceRequirements,
        a.scienceMode,
        a.manualConfig,
        a.existence
      )}

    val scienceMode: Lens[PropertiesInput, Input[ScienceModeInput]] =
      Focus[PropertiesInput](_.scienceMode)

    val manualConfig: Lens[PropertiesInput, Input[ExecutionModel.Create]] =
      Focus[PropertiesInput](_.manualConfig)

  }

  final case class CreateInput(
    programId: Program.Id,
    SET:       Option[PropertiesInput]
  ) {

    def create[F[_]: Sync]: F[StateT[EitherInput, Database, ObservationModel]] =
      SET.flatMap(_.manualConfig.toOption).traverse(_.create[F]).map { c =>
        for {
          i <- Database.observation.cycleNextUnused
          _ <- Database.program.lookup(programId)

          o  = SET.getOrElse(PropertiesInput.Empty).create(i, programId, c.sequence)
          oʹ <- o.traverse(_.validate)
          _  <- Database.observation.saveNewIfValid(oʹ)(_.id)
          v  <- Database.observation.lookup(i)
        } yield v
      }

  }

  object CreateInput {

    def empty(programId: Program.Id): CreateInput =
      CreateInput(
        programId = programId,
        SET       = None
      )

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderCreateInput: Decoder[CreateInput] =
      deriveConfiguredDecoder[CreateInput]

    implicit val EqCreateInput: Eq[CreateInput] =
      Eq.by { a => (
        a.programId,
        a.SET
      )}

    val properties: Lens[CreateInput, Option[PropertiesInput]] =
      Focus[CreateInput](_.SET)

    val scienceMode: Optional[CreateInput, Input[ScienceModeInput]] =
      properties.some.andThen(PropertiesInput.scienceMode)

    val manualConfig: Optional[CreateInput, Input[ExecutionModel.Create]] =
      properties.some.andThen(PropertiesInput.manualConfig)
  }

  final case class CreateResult(
    observation: ObservationModel
  )

  object CreateResult {

    implicit val EqCreateResult: Eq[CreateResult] =
      Eq.by(_.observation)
  }

  final case class UpdateInput(
    SET:   PropertiesInput,
    WHERE: Option[WhereObservationInput],
    LIMIT: Option[NonNegInt],
    includeDeleted: Boolean = false
  ) extends TopLevelUpdateInput[Observation.Id, ObservationModel] {

    override def typeName: String =
      "observation"

    override def editOne: StateT[EitherInput, ObservationModel, Unit] =
      SET.edit

    override def state: DatabaseState[Observation.Id, ObservationModel] =
      Database.observation
  }

  object UpdateInput {

    implicit val DecoderUpdateInput: Decoder[UpdateInput] =
      deriveDecoder[UpdateInput]

    implicit val EqUpdateInput: Eq[UpdateInput] =
      Eq.by { a => (
        a.SET,
        a.WHERE,
        a.LIMIT,
        a.includeDeleted
      )}

  }

  final case class CloneResult(
    originalObservation: ObservationModel,
    newObservation:      ObservationModel
  )

  object CloneResult {

    implicit val EqCloneResult: Eq[CloneResult] =
      Eq.by { a => (
        a.originalObservation,
        a.newObservation
      )}

  }

  final case class CloneInput(
    observationId: Observation.Id,
    SET:           Option[PropertiesInput]
  ) {

    // At the moment, manual config (if present in patch) is ignored
    val go: StateT[EitherInput, Database, CloneResult] =
      for {
        o  <- Database.observation.lookup(observationId)
        i  <- Database.observation.cycleNextUnused
        c   = o.clone(i)
        _  <- Database.observation.saveNew(i, c)
        cʹ <- SET.fold(StateT.pure[EitherInput, Database, ObservationModel](c)) { p =>
          ObservationModel.UpdateInput(
            p,
            WhereObservationInput.MatchAll.withId(i).some,
            None
          ).editor.map(_.limitedValues.head)
        }
      } yield CloneResult(originalObservation = o, newObservation = cʹ)

  }

  object CloneInput {

    implicit val DecoderCloneInput: Decoder[CloneInput] =
      deriveDecoder[CloneInput]

    implicit val EqCloneInput: Eq[CloneInput] =
      Eq.by { a => (
        a.observationId,
        a.SET
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
    SET:   A,
    WHERE: Option[WhereObservationInput],
    LIMIT: Option[NonNegInt]
  )

  object BulkEdit {

    def observations[A](set: A, oids: List[Observation.Id]): BulkEdit[A] =
      BulkEdit(set, WhereObservationInput.MatchAll.withIds(oids).some, None)

    def program[A](set: A, pid: Program.Id): BulkEdit[A] =
      BulkEdit(set, WhereObservationInput.MatchAll.withProgramId(pid).some, None)

    implicit def DecoderBulkEdit[A: Decoder]: Decoder[BulkEdit[A]] =
      deriveDecoder[BulkEdit[A]]

    implicit def EqBulkEdit[A: Eq]: Eq[BulkEdit[A]] =
      Eq.by { a => (
        a.SET,
        a.WHERE
      )}

  }
}

trait ObservationOptics { self: ObservationModel.type =>

  val id: Lens[ObservationModel, Observation.Id] =
    Focus[ObservationModel](_.id)

  val existence: Lens[ObservationModel, Existence] =
    Focus[ObservationModel](_.existence)

  val subtitle: Lens[ObservationModel, Option[NonEmptyString]] =
    Focus[ObservationModel](_.subtitle)

  val status: Lens[ObservationModel, ObsStatus] =
    Focus[ObservationModel](_.status)

  val activeStatus: Lens[ObservationModel, ObsActiveStatus] =
    Focus[ObservationModel](_.activeStatus)

  val visualizationTime: Lens[ObservationModel, Option[Instant]] =
    Focus[ObservationModel](_.visualizationTime)

  val posAngleConstraint: Lens[ObservationModel, Option[PosAngleConstraint]] =
    Focus[ObservationModel](_.posAngleConstraint)

  val targetEnvironment: Lens[ObservationModel, TargetEnvironmentModel] =
    Focus[ObservationModel](_.targetEnvironment)

  val constraintSet: Lens[ObservationModel, ConstraintSet] =
    Focus[ObservationModel](_.constraintSet)

  val scienceRequirements: Lens[ObservationModel, ScienceRequirements] =
    Focus[ObservationModel](_.scienceRequirements)

  val scienceMode: Lens[ObservationModel, Option[ScienceMode]] =
    Focus[ObservationModel](_.scienceMode)

  val manualConfig: Lens[ObservationModel, Option[ExecutionModel]] =
    Focus[ObservationModel](_.manualConfig)

}
