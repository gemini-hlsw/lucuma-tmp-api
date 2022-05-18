// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.syntax.input._
import lucuma.odb.api.model.syntax.lens._
import lucuma.odb.api.model.syntax.validatedinput._
import lucuma.odb.api.model.targetModel.{TargetEnvironmentInput, TargetEnvironmentModel}
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.model.{ConstraintSet, Observation, Program}
import cats.{Eq, Functor}
import cats.data.StateT
import cats.effect.Sync
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import clue.data.Input
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.refined._
import monocle.{Focus, Lens, Optional}

import scala.collection.immutable.SortedSet


final case class ObservationModel(
  id:                  Observation.Id,
  programId:           Program.Id,
  existence:           Existence,
  subtitle:            Option[NonEmptyString],
  status:              ObsStatus,
  activeStatus:        ObsActiveStatus,
  targetEnvironment:   TargetEnvironmentModel,
  constraintSet:       ConstraintSet,
  scienceRequirements: ScienceRequirements,
  scienceMode:         Option[ScienceMode],
  config:              Option[ExecutionModel]
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
      o.targetEnvironment,
      o.constraintSet,
      o.scienceRequirements,
      o.scienceMode,
      o.config
    )}

  final case class PropertiesInput(
    subtitle:             Input[NonEmptyString]            = Input.ignore,
    status:               Input[ObsStatus]                 = Input.ignore,
    activeStatus:         Input[ObsActiveStatus]           = Input.ignore,
    targetEnvironment:    Input[TargetEnvironmentInput]    = Input.ignore,
    constraintSet:        Input[ConstraintSetInput]        = Input.ignore,
    scienceRequirements:  Input[ScienceRequirementsInput]  = Input.ignore,
    scienceMode:          Input[ScienceModeInput]          = Input.ignore,
    config:               Input[ExecutionModel.Create]     = Input.ignore,
    existence:            Input[Existence]                 = Input.ignore
  ) {

    def create(
      observationId: Observation.Id,
      programId:     Program.Id,
      config:        ValidatedInput[Option[ExecutionModel]]
    ): ValidatedInput[ObservationModel] = {
      val t = targetEnvironment.toOption.getOrElse(TargetEnvironmentInput.Empty).create
      val c = constraintSet.toOption.traverse(_.create)
      val q = scienceRequirements.toOption.traverse(_.create)
      val u = scienceMode.toOption.traverse(_.create)
      (t, c, q, u, config).mapN { (tʹ, cʹ, qʹ, uʹ, gʹ) =>
        ObservationModel(
          id = observationId,
          existence = existence.toOption.getOrElse(Existence.Present),
          programId = programId,
          subtitle = subtitle.toOption,
          status = status.toOption.getOrElse(ObsStatus.New),
          activeStatus = activeStatus.toOption.getOrElse(ObsActiveStatus.Active),
          targetEnvironment = tʹ,
          constraintSet = cʹ.getOrElse(ConstraintSetModel.Default),
          scienceRequirements = qʹ.getOrElse(ScienceRequirements.Default),
          scienceMode = uʹ,
          config = gʹ
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
        _ <- ObservationModel.targetEnvironment   :! targetEnvironment
        _ <- ObservationModel.constraintSet       :! constraintSet
        _ <- ObservationModel.scienceRequirements :! scienceRequirements
        _ <- ObservationModel.scienceMode         :? scienceMode
      } yield ()
    }

  }

  object PropertiesInput {

    val Empty: PropertiesInput =
      PropertiesInput(
        subtitle            = Input.ignore,
        status              = Input.ignore,
        activeStatus        = Input.ignore,
        targetEnvironment   = Input.ignore,
        constraintSet       = Input.ignore,
        scienceRequirements = Input.ignore,
        scienceMode         = Input.ignore,
        config              = Input.ignore,
        existence           = Input.ignore
      )

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
        a.targetEnvironment,
        a.constraintSet,
        a.scienceRequirements,
        a.scienceMode,
        a.existence
      )}

    val scienceMode: Lens[PropertiesInput, Input[ScienceModeInput]] =
      Focus[PropertiesInput](_.scienceMode)

    val config: Lens[PropertiesInput, Input[ExecutionModel.Create]] =
      Focus[PropertiesInput](_.config)

  }

  final case class CreateInput(
    programId:     Program.Id,
    properties:    Option[PropertiesInput]
  ) {

    def create[F[_]: Sync]: F[StateT[EitherInput, Database, ObservationModel]] =
      properties.flatMap(_.config.toOption).traverse(_.create[F]).map { c =>
        for {
          i <- Database.observation.cycleNextUnused
          _ <- Database.program.lookup(programId)

          o  = properties.getOrElse(PropertiesInput.Empty).create(i, programId, c.sequence)
          oʹ <- o.traverse(_.validate)
          _  <- Database.observation.saveNewIfValid(oʹ)(_.id)
          v  <- Database.observation.lookup(i)
        } yield v
      }

  }

  object CreateInput {

    def empty(programId: Program.Id): CreateInput =
      CreateInput(
        programId   = programId,
        properties  = None
      )

    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderCreateInput: Decoder[CreateInput] =
      deriveConfiguredDecoder[CreateInput]

    implicit val EqCreateInput: Eq[CreateInput] =
      Eq.by { a => (
        a.programId,
        a.properties
      )}

    val properties: Lens[CreateInput, Option[PropertiesInput]] =
      Focus[CreateInput](_.properties)

    val scienceMode: Optional[CreateInput, Input[ScienceModeInput]] =
      properties.some.andThen(PropertiesInput.scienceMode)

    val config: Optional[CreateInput, Input[ExecutionModel.Create]] =
      properties.some.andThen(PropertiesInput.config)
  }

  final case class SelectInput(
    programId:      Option[Program.Id],
    observationId:  Option[Observation.Id],
    observationIds: Option[List[Observation.Id]]
  ) {

    val go: StateT[EitherInput, Database, List[ObservationModel]] =
      for {
        p   <- programId.traverse(Database.program.lookup)
        os0 <- p.traverse { pm =>
          StateT.inspect[EitherInput, Database, List[ObservationModel]](_.observations.rows.values.filter(_.programId === pm.id).toList)
        }.map(_.toList.flatten)
        os1 <- observationId.traverse(Database.observation.lookup).map(_.toList)
        os2 <- observationIds.traverse(Database.observation.lookupAll).map(_.toList.flatten)
      } yield (os0 ::: os1 ::: os2).distinctBy(_.id).sortBy(_.id)

  }

  object SelectInput {

    val Empty: SelectInput =
      SelectInput(None, None, None)

    def observationId(oid: Observation.Id): SelectInput =
      Empty.copy(observationId = oid.some)

    def observationIds(oids: List[Observation.Id]): SelectInput =
      Empty.copy(observationIds = oids.some)

    implicit val DecoderSelect: Decoder[SelectInput] =
      deriveDecoder[SelectInput]

    implicit val EqSelect: Eq[SelectInput] =
      Eq.by { a => (
        a.programId,
        a.observationId,
        a.observationIds
      )}

  }

  final case class EditInput(
    select: SelectInput,
    patch:  PropertiesInput
  ) {

    // At the moment, manual config (if present in patch) is ignored
    val editor: StateT[EitherInput, Database, List[ObservationModel]] =
      for {
        os  <- select.go
        osʹ <- StateT.liftF[EitherInput, Database, List[ObservationModel]](os.traverse(patch.edit.runS))
        _   <- osʹ.traverse(o => Database.observation.update(o.id, o))
      } yield osʹ

  }

  object EditInput {

    implicit val DecoderEditInput: Decoder[EditInput] =
      deriveDecoder[EditInput]

    implicit val EqEditInput: Eq[EditInput] =
      Eq.by { a => (
        a.select,
        a.patch
      )}

  }

  final case class CloneInput(
    observationId: Observation.Id,
    patch:         Option[PropertiesInput]
  ) {

    // At the moment, manual config (if present in patch) is ignored
    val go: StateT[EitherInput, Database, ObservationModel] =
      for {
        o  <- Database.observation.lookup(observationId)
        i  <- Database.observation.cycleNextUnused
        c   = o.clone(i)
        _  <- Database.observation.saveNew(i, c)
        cʹ <- patch.fold(StateT.pure[EitherInput, Database, ObservationModel](c)) { p =>
          ObservationModel.EditInput(
            ObservationModel.SelectInput.observationId(i),
            p
          ).editor.map(_.head)
        }
      } yield cʹ

  }

  object CloneInput {

    implicit val DecoderCloneInput: Decoder[CloneInput] =
      deriveDecoder[CloneInput]

    implicit val EqCloneInput: Eq[CloneInput] =
      Eq.by { a => (
        a.observationId,
        a.patch
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
    select: BulkEdit.Select,
    patch:  A
  )

  object BulkEdit {

    final case class Select(
      programId:      Option[Program.Id],
      observationIds: Option[List[Observation.Id]]
    )

    object Select {

      implicit val DecoderSelect: Decoder[Select] =
        deriveDecoder[Select]

      implicit val EqSelect: Eq[Select] =
        Eq.by { a => (
          a.programId,
          a.observationIds
        )}

      def observations(oids: List[Observation.Id]): Select =
        Select(None, oids.some)

      def program(pid: Program.Id): Select =
        Select(pid.some, None)

    }

    def observations[A](oids: List[Observation.Id], edit: A): BulkEdit[A] =
      BulkEdit(Select.observations(oids), edit)

    def program[A](pid: Program.Id, edit: A): BulkEdit[A] =
      BulkEdit(Select.program(pid), edit)

    implicit def DecoderBulkEdit[A: Decoder]: Decoder[BulkEdit[A]] =
      deriveDecoder[BulkEdit[A]]

    implicit def EqBulkEdit[A: Eq]: Eq[BulkEdit[A]] =
      Eq.by { a => (
        a.select,
        a.patch
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

  val targetEnvironment: Lens[ObservationModel, TargetEnvironmentModel] =
    Focus[ObservationModel](_.targetEnvironment)

  val constraintSet: Lens[ObservationModel, ConstraintSet] =
    Focus[ObservationModel](_.constraintSet)

  val scienceRequirements: Lens[ObservationModel, ScienceRequirements] =
    Focus[ObservationModel](_.scienceRequirements)

  val scienceMode: Lens[ObservationModel, Option[ScienceMode]] =
    Focus[ObservationModel](_.scienceMode)

  val config: Lens[ObservationModel, Option[ExecutionModel]] =
    Focus[ObservationModel](_.config)

}
