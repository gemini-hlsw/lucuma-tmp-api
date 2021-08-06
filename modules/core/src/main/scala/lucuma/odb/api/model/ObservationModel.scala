// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.model

import lucuma.odb.api.model.Existence._
import lucuma.odb.api.model.ScienceConfigurationModel.ScienceConfigurationModelEdit
import lucuma.odb.api.model.syntax.input._
import lucuma.core.`enum`.{ObsActiveStatus, ObsStatus}
import lucuma.core.math.Coordinates
import lucuma.core.model.{Observation, Program, Target}
import lucuma.core.optics.state.all._
import lucuma.core.optics.syntax.lens._

import cats.{Eq, Functor, Monad}
import cats.data.State
import cats.implicits.catsKernelOrderingForOrder
import cats.mtl.Stateful
import cats.syntax.apply._
import cats.syntax.flatMap._
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

import scala.collection.immutable.{SortedMap, SortedSet}


final case class ObservationModel(
  id:                   Observation.Id,
  existence:            Existence,
  programId:            Program.Id,
  name:                 Option[NonEmptyString],
  status:               ObsStatus,
  activeStatus:         ObsActiveStatus,
  targets:              TargetEnvironmentModel,
  constraintSet:        ConstraintSetModel,
  scienceRequirements:  ScienceRequirements,
  scienceConfiguration: Option[ScienceConfigurationModel],
  config:               Option[InstrumentConfigModel.Reference],
  plannedTimeSummary:   PlannedTimeSummaryModel
)


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
      o.targets,
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
    targets:              Option[TargetEnvironmentModel.Create],
    constraintSet:        Option[ConstraintSetModel.Create],
    scienceRequirements:  Option[ScienceRequirementsModel.Create],
    scienceConfiguration: Option[ScienceConfigurationModel.Create],
    config:               Option[InstrumentConfigModel.Create]
  ) {

    def create[F[_]: Monad, T](
      db: DatabaseState[T],
      s:  PlannedTimeSummaryModel
    )(implicit S: Stateful[F, T]): F[ValidatedInput[ObservationModel]] =

      for {
        i <- db.observation.getUnusedId(observationId)
        p <- db.program.lookupValidated(programId)
        t  = targets.traverse(_.create)
        c  = constraintSet.traverse(_.create)
        q  = scienceRequirements.traverse(_.create)
        u  = scienceConfiguration.traverse(_.create)
        g <- config.traverse(_.create(db)).map(_.sequence)
        o  = (i, p, t, c, q, u, g).mapN { (iʹ, _, tʹ, cʹ, qʹ, uʹ,  gʹ) =>
          ObservationModel(
            iʹ,
            Present,
            programId,
            name,
            status.getOrElse(ObsStatus.New),
            activeStatus.getOrElse(ObsActiveStatus.Active),
            tʹ.getOrElse(TargetEnvironmentModel.Empty),
            cʹ.getOrElse(ConstraintSetModel.AnyConstraints),
            qʹ.getOrElse(ScienceRequirements.Default),
            uʹ,
            gʹ.map(_.toReference),
            s
          )
        }
        _ <- db.observation.saveIfValid(o)(_.id)
      } yield o

  }

  object Create {

    def empty(programId: Program.Id): Create =
      Create(
        observationId        = None,
        programId            = programId,
        name                 = None,
        status               = None,
        activeStatus         = None,
        targets              = None,
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
        a.targets,
        a.constraintSet,
        a.scienceRequirements,
        a.scienceConfiguration,
        a.config
      )}

  }

  final case class Edit(
    observationId:        Observation.Id,
    existence:            Input[Existence]                      = Input.ignore,
    name:                 Input[NonEmptyString]                 = Input.ignore,
    status:               Input[ObsStatus]                      = Input.ignore,
    activeStatus:         Input[ObsActiveStatus]                = Input.ignore,
    targets:              Option[TargetEnvironmentModel.Edit]   = None,
    constraintSet:        Option[ConstraintSetModel.Edit]       = None,
    scienceRequirements:  Option[ScienceRequirementsModel.Edit] = None,
    scienceConfiguration: Input[ScienceConfigurationModelEdit]  = Input.ignore
  ) {

    val id: Observation.Id =
      observationId

    def editOrCreateSciConfig(
      ed: Option[Option[Either[ScienceConfigurationModel, State[ScienceConfigurationModel, Unit]]]]
    )(
      m: Option[ScienceConfigurationModel]
    ): Option[ScienceConfigurationModel] =
      ed.flatMap {
        case Some(Left(s))  => s.some
        case Some(Right(s)) => m.map(s.runS(_).value)
        case _              => None
      }

    def edit(o: ObservationModel): ValidatedInput[ObservationModel] =
      editor(
        targets.traverse(_.edit(o.targets, o.id))
      ).map(_.runS(o).value)


    private def editor(
      targets: ValidatedInput[Option[TargetEnvironmentModel]]
    ): ValidatedInput[State[ObservationModel, Unit]] =
      (existence   .validateIsNotNull("existence"),
       status      .validateIsNotNull("status"),
       activeStatus.validateIsNotNull("active"),
       targets,
       constraintSet.traverse(_.editor),
       scienceRequirements.traverse(_.editor),
       scienceConfiguration.validateNullable(_.editor)
      ).mapN { (e, s, a, t, c, sr, sc) =>
        for {
          _ <- ObservationModel.existence    := e
          _ <- ObservationModel.name         := name.toOptionOption
          _ <- ObservationModel.status       := s
          _ <- ObservationModel.activeStatus := a
          _ <- ObservationModel.targets      := t
          _ <- c.fold(State.get[ObservationModel].void) { ed =>
            ObservationModel.constraintSet.mod_(ed.runS(_).value)
          }
          _ <- sr.fold(State.get[ObservationModel].void) { ed =>
            ObservationModel.scienceRequirements.mod_(ed.runS(_).value)
          }
          _ <- sc.fold(State.get[ObservationModel].void) { ed =>
            ObservationModel.scienceConfiguration.mod_(editOrCreateSciConfig(ed))
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
        a.targets,
        a.constraintSet
      )}

  }


  final case class EditTargetEnvironment(
    observationIds: List[Observation.Id],
    targets:        TargetEnvironmentModel.Edit
  )

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

  final case class BulkEdit[S, E](
    select: S,
    edit:   E
  )

  object BulkEdit {

    implicit def DecoderBulkEdit[S: Decoder, E: Decoder]: Decoder[BulkEdit[S, E]] =
      deriveDecoder[BulkEdit[S, E]]

    implicit def EqBulkEdit[S: Eq, E: Eq]: Eq[BulkEdit[S, E]] =
      Eq.by { a => (
        a.select,
        a.edit
      )}

  }

  final case class ObservationSelector(
    programId:      Option[Program.Id],
    observationIds: Option[List[Observation.Id]]
  )

  object ObservationSelector {

    implicit val DecoderObsSelectorInput: Decoder[ObservationSelector] =
      deriveDecoder[ObservationSelector]

    implicit val EqObsSelectorInput: Eq[ObservationSelector] =
      Eq.by { a => (
        a.programId,
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

  val targets: Lens[ObservationModel, TargetEnvironmentModel] =
    Lens[ObservationModel, TargetEnvironmentModel](_.targets)(a => _.copy(targets = a))

  val explicitBase: Lens[ObservationModel, Option[Coordinates]] =
    targets.andThen(TargetEnvironmentModel.explicitBase)

  val scienceTargets: Lens[ObservationModel, SortedMap[NonEmptyString, Target]] =
    targets.andThen(TargetEnvironmentModel.science)

  def scienceTarget(name: NonEmptyString): Optional[ObservationModel, Target] =
    scienceTargets.index(name)

  def nonsiderealScienceTarget(name: NonEmptyString): Optional[ObservationModel, Target] =
    scienceTarget(name).andThen(TargetModel.nonsiderealTarget)

  def siderealScienceTarget(name: NonEmptyString): Optional[ObservationModel, Target] =
    scienceTarget(name).andThen(TargetModel.siderealTarget)

  val constraintSet: Lens[ObservationModel, ConstraintSetModel] =
    Lens[ObservationModel, ConstraintSetModel](_.constraintSet)(a => _.copy(constraintSet = a))

  val scienceRequirements: Lens[ObservationModel, ScienceRequirements] =
    Focus[ObservationModel](_.scienceRequirements)

  val scienceConfiguration: Lens[ObservationModel, Option[ScienceConfigurationModel]] =
    Focus[ObservationModel](_.scienceConfiguration)

  val config: Lens[ObservationModel, Option[InstrumentConfigModel.Reference]] =
    Lens[ObservationModel, Option[InstrumentConfigModel.Reference]](_.config)(a => _.copy(config = a))

}
