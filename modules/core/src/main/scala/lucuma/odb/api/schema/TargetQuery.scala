// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import cats.MonadError
import cats.effect.std.Dispatcher
import cats.syntax.all._
import lucuma.odb.api.repo.{OdbRepo, ResultPage}
import lucuma.odb.api.model.targetModel.TargetModel
import sangria.schema._


trait TargetQuery {
  import context._

  import GeneralSchema.ArgumentIncludeDeleted
  import ObservationSchema.{ ObservationIdArgument, OptionalListObservationIdArgument }
  import Paging._
  import ProgramSchema.{ OptionalProgramIdArgument, ProgramIdArgument }
  import TargetSchema.{TargetEnvironmentType, TargetConnectionType, TargetType}

  def referencedScienceTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "referencedScienceTargets",
      fieldType   = TargetConnectionType[F],
      description = "All the science targets that are used by one or more observations in the given program".some,
      arguments   = List(
        ProgramIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve = c =>
        unsafeSelectTopLevelPageFuture(c.pagingTargetId) { gid =>
          c.ctx.target.selectReferencedPageForProgram(c.programId, c.pagingFirst, gid, c.includeDeleted)
        }
    )

  def allScienceTargets[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "allScienceTargets",
      fieldType   = TargetConnectionType[F],
      description = "All the science targets (used or not) associated with a given program or specific observations".some,
      arguments   = List(
        OptionalProgramIdArgument,
        OptionalListObservationIdArgument,
        ArgumentPagingFirst,
        ArgumentPagingCursor,
        ArgumentIncludeDeleted
      ),
      resolve = c =>
        unsafeSelectTopLevelPageFuture(c.pagingTargetId) { gid =>
          (c.optionalProgramId, c.arg(OptionalListObservationIdArgument)) match {
            case (_, Some(oids)) => c.ctx.target.selectPageForObservations(oids.toSet, c.pagingFirst, gid, c.includeDeleted)
            case (Some(pid), _)  => c.ctx.target.selectPageForProgram(pid, c.pagingFirst, gid, c.includeDeleted)
            case _               => ResultPage.empty[TargetModel].pure[F]
          }
        }
    )

  def firstScienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "firstScienceTarget",
      fieldType   = OptionType(TargetType[F]),
      description = "The first (or only) science target (if any) for the given observation".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationFirstTarget(c.observationId))
    )

  def asterism[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "asterism",
      fieldType   = ListType(TargetType[F]),
      description = "All science targets (if any) for the given observation (or environment)".some,
      arguments   = List(ObservationIdArgument, ArgumentIncludeDeleted),
      resolve     = c => c.target(_.selectObservationAsterism(c.observationId, c.includeDeleted).map(_.toList))
    )

  def targetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
    Field(
      name        = "targetEnvironment",
      fieldType   = OptionType(TargetEnvironmentType[F]),
      description = "Target environment for the given observation (or environment id)".some,
      arguments   = List(ObservationIdArgument),
      resolve     = c => c.target(_.selectObservationTargetEnvironment(c.observationId))
    )

//  def TargetEnvironmentGroupType[F[_]: Dispatcher, A](
//    name:      String,
//    valueName: String,
//    outType:   OutputType[A]
//  )(implicit ev: MonadError[F, Throwable]): ObjectType[OdbRepo[F], TargetEnvironmentGroup[A]] =
//    ObjectType(
//      name     = name,
//      fieldsFn = () => fields(
//
//        Field(
//          name        = "targetEnvironmentIds",
//          fieldType   = ListType(TargetEnvironmentIdType),
//          description = "IDs of target environments that share the common value".some,
//          resolve     = _.value.targetEnvironmentIds.toList
//        ),
//
//        Field(
//          name        = "observationIds",
//          fieldType   = ListType(ObservationIdType),
//          description = "IDs of observations that share the common value".some,
//          resolve     = c => c.target { repo =>
//            Nested(c.value.targetEnvironmentIds.toList.traverse(repo.unsafeSelectTargetEnvironment))
//              .map(_.observationId.toList)
//              .value
//              .map(_.flatten)
//          }
//        ),
//
//        Field(
//          name        = "targetEnvironments",
//          fieldType   = ListType(TargetEnvironmentModelType[F]),
//          description = "Target environments that share the common value".some,
//          resolve     = c => c.target { repo =>
//            c.value.targetEnvironmentIds.toList.traverse(repo.unsafeSelectTargetEnvironment)
//          }
//        ),
//
//        Field(
//          name        = valueName,
//          fieldType   = outType,
//          description = "Commonly held value across the target environments".some,
//          resolve     = _.value.value
//        )
//      )
//    )

//  def groupByScienceTarget[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
//    Field(
//      name        = "scienceTargetGroup",
//      fieldType   = ListType(TargetEnvironmentGroupType[F, CommonTarget]("GroupByTarget", "commonTarget", CommonTargetType[F])),
//      description = "Target environments grouped by those that share the same target".some,
//      arguments   = List(
//        ProgramIdArgument,
//        ArgumentIncludeDeleted
//      ),
//      resolve     = c => c.target(_.groupBySingleScienceTarget(c.programId, c.includeDeleted))
//    )
//
//  def groupByScienceTargetList[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
//    Field(
//      name        = "scienceTargetListGroup",
//      fieldType   = ListType(TargetEnvironmentGroupType[F, Seq[CommonTarget]]("GroupByTargetList", "commonTargetList", ListType(CommonTargetType[F]))),
//      description = "Target environments grouped by those that share the same collection of targets".some,
//      arguments   = List(
//        ProgramIdArgument,
//        ArgumentIncludeDeleted
//      ),
//      resolve     = c => c.target { repo =>
//        Nested(repo.groupByScienceTargetList(c.programId, c.includeDeleted))
//          .map(_.map(Seq.from))
//          .value
//      }
//    )
//
//  def groupByTargetEnvironment[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): Field[OdbRepo[F], Unit] =
//    Field(
//      name        = "targetEnvironmentGroup",
//      fieldType   = ListType(TargetEnvironmentGroupType[F, CommonTargetEnvironment]("GroupByTargetEnvironment", "commonTargetEnvironment", CommonTargetEnvironmentType[F])),
//      description = "Target environments grouped by those that share the same properties and targets".some,
//      arguments   = List(
//        ProgramIdArgument,
//        ArgumentIncludeDeleted
//      ),
//      resolve     = c => c.target(_.groupByTargetEnvironment(c.programId, c.includeDeleted))
//    )
//
  def allFields[F[_]: Dispatcher](implicit ev: MonadError[F, Throwable]): List[Field[OdbRepo[F], Unit]] =
    List(
      referencedScienceTargets[F],
      firstScienceTarget[F],
      asterism[F],
      targetEnvironment[F]
//      groupByScienceTarget[F],
//      groupByScienceTargetList[F],
//      groupByTargetEnvironment[F]
    )
}

object TargetQuery extends TargetQuery
