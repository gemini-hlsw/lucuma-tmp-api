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
  id:         Observation.Id,
  existence:  Existence,
  programId:  Program.Id,
  properties: ObservationModel.Properties
) {

  val validate: StateT[EitherInput, Database, ObservationModel] =
    properties.validate(programId).as(this)

  def clone(newId: Observation.Id): ObservationModel =
    copy(
      id         = newId,
      existence  = Existence.Present,
      properties = properties.copy(status = ObsStatus.New)
    )
}


object ObservationModel extends ObservationOptics {

  final case class Properties(
    subtitle:            Option[NonEmptyString],
    status:              ObsStatus,
    activeStatus:        ObsActiveStatus,
    targetEnvironment:   TargetEnvironmentModel,
    constraintSet:       ConstraintSet,
    scienceRequirements: ScienceRequirements,
    scienceMode:         Option[ScienceMode],
    config:              Option[ExecutionModel]
  ) {

    def validate(programId: Program.Id): StateT[EitherInput, Database, Properties] =
      targetEnvironment.validate(programId).as(this)

  }

  object Properties {
    implicit val EqProperties: Eq[Properties] =
      Eq.by { a => (
        a.subtitle,
        a.status,
        a.activeStatus,
        a.targetEnvironment,
        a.constraintSet,
        a.scienceRequirements,
        a.scienceMode,
        a.config
      )}

    val subtitle: Lens[Properties, Option[NonEmptyString]] =
      Focus[Properties](_.subtitle)

    val status: Lens[Properties, ObsStatus] =
      Focus[Properties](_.status)

    val activeStatus: Lens[Properties, ObsActiveStatus] =
      Focus[Properties](_.activeStatus)

    val targetEnvironment: Lens[Properties, TargetEnvironmentModel] =
      Focus[Properties](_.targetEnvironment)

    val constraintSet: Lens[Properties, ConstraintSet] =
      Focus[Properties](_.constraintSet)

    val scienceRequirements: Lens[Properties, ScienceRequirements] =
      Focus[Properties](_.scienceRequirements)

    val scienceMode: Lens[Properties, Option[ScienceMode]] =
      Focus[Properties](_.scienceMode)

    val config: Lens[Properties, Option[ExecutionModel]] =
      Focus[Properties](_.config)

  }

  implicit val TopLevelObservation: TopLevelModel[Observation.Id, ObservationModel] =
    TopLevelModel.instance(_.id, ObservationModel.existence)

  implicit val EqObservation: Eq[ObservationModel] =
    Eq.by { o => (
      o.id,
      o.existence,
      o.programId,
      o.properties
    )}

  final case class PropertiesInput(
    subtitle:             Input[NonEmptyString]            = Input.ignore,
    status:               Input[ObsStatus]                 = Input.ignore,
    activeStatus:         Input[ObsActiveStatus]           = Input.ignore,
    targetEnvironment:    Input[TargetEnvironmentInput]    = Input.ignore,
    constraintSet:        Input[ConstraintSetInput]        = Input.ignore,
    scienceRequirements:  Input[ScienceRequirementsInput]  = Input.ignore,
    scienceMode:          Input[ScienceModeInput]          = Input.ignore,
    config:               Input[ExecutionModel.Create]     = Input.ignore
  ) {

    def create[F[_]: Sync]: F[ValidatedInput[Properties]] =
      config.traverse(_.create).map { g =>
        val t = targetEnvironment.toOption.getOrElse(TargetEnvironmentInput.Empty).create
        val c = constraintSet.toOption.traverse(_.create)
        val q = scienceRequirements.toOption.traverse(_.create)
        val u = scienceMode.toOption.traverse(_.create)
        (t, c, q, u, g.toOption.sequence).mapN { (tʹ, cʹ, qʹ, uʹ, gʹ) =>
          Properties(
            subtitle            = subtitle.toOption,
            status              = status.toOption.getOrElse(ObsStatus.New),
            activeStatus        = activeStatus.toOption.getOrElse(ObsActiveStatus.Active),
            targetEnvironment   = tʹ,
            constraintSet       = cʹ.getOrElse(ConstraintSetModel.Default),
            scienceRequirements = qʹ.getOrElse(ScienceRequirements.Default),
            scienceMode         = uʹ,
            config              = gʹ
          )
        }
      }

    val edit: StateT[EitherInput, Properties, Unit] = {
      val validArgs =
        (status.validateIsNotNull("status"),
         activeStatus.validateIsNotNull("active")
        ).tupled

      for {
        args <- validArgs.liftState
        (s, a) = args
        _ <- Properties.subtitle            := subtitle.toOptionOption
        _ <- Properties.status              := s
        _ <- Properties.activeStatus        := a
        _ <- Properties.targetEnvironment   :! targetEnvironment
        _ <- Properties.constraintSet       :! constraintSet
        _ <- Properties.scienceRequirements :! scienceRequirements
        _ <- Properties.scienceMode         :? scienceMode
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
        config              = Input.ignore
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
        a.scienceMode
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
      properties.getOrElse(PropertiesInput.Empty).create.map { p =>
        for {
          i <- Database.observation.cycleNextUnused
          _ <- Database.program.lookup(programId)
          o  = p.map(pʹ => ObservationModel(i, Existence.Present, programId, pʹ))
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

  final case class PatchInput(
    properties: Input[PropertiesInput] = Input.ignore,
    existence:  Input[Existence]       = Input.ignore
  ) {

    val editor: StateT[EitherInput, ObservationModel, Unit] = {
      val validArgs =
        (existence.validateIsNotNull("existence"),
         properties.validateIsNotNull("properties")
        ).tupled

      for {
        args <- validArgs.liftState
        (e, _) = args
        _ <- ObservationModel.existence := e
        _ <- ObservationModel.properties :< properties.toOption.map(_.edit)
      } yield ()
    }

  }

  object PatchInput {
    import io.circe.generic.extras.semiauto._
    import io.circe.generic.extras.Configuration
    implicit val customConfig: Configuration = Configuration.default.withDefaults

    implicit val DecoderPatchInput: Decoder[PatchInput] =
      deriveConfiguredDecoder[PatchInput]

    implicit val EqPatchInput: Eq[PatchInput] =
      Eq.by { a => (
        a.properties,
        a.existence
      )}

  }



  final case class EditInput(
    select: SelectInput,
    patch:  PatchInput
  ) {

    // At the moment, manual config (if present in patch) is ignored
    val editor: StateT[EitherInput, Database, List[ObservationModel]] =
      for {
        os  <- select.go
        osʹ <- StateT.liftF[EitherInput, Database, List[ObservationModel]](os.traverse(patch.editor.runS))
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
    patch:         Option[PatchInput]
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
    edit:   A
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
        a.edit
      )}

  }
}

trait ObservationOptics { self: ObservationModel.type =>

  val id: Lens[ObservationModel, Observation.Id] =
    Focus[ObservationModel](_.id)

  val existence: Lens[ObservationModel, Existence] =
    Focus[ObservationModel](_.existence)

  val properties: Lens[ObservationModel, ObservationModel.Properties] =
    Focus[ObservationModel](_.properties)

  val targetEnvironment: Lens[ObservationModel, TargetEnvironmentModel] =
    properties.andThen(Properties.targetEnvironment)

  val constraintSet: Lens[ObservationModel, ConstraintSet] =
    properties.andThen(Properties.constraintSet)

  val scienceRequirements: Lens[ObservationModel, ScienceRequirements] =
    properties.andThen(Properties.scienceRequirements)

  val scienceMode: Lens[ObservationModel, Option[ScienceMode]] =
    properties.andThen(Properties.scienceMode)

  val config: Lens[ObservationModel, Option[ExecutionModel]] =
    properties.andThen(Properties.config)

}
