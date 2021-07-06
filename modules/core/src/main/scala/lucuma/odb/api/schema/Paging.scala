// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.schema

import lucuma.odb.api.repo.{OdbRepo, ResultPage}
import lucuma.core.util.Gid
import lucuma.odb.api.model.{InputError, TopLevelModel}

import cats.{ApplicativeError, Eq, Order}
import cats.effect.std.Dispatcher
import cats.syntax.all._
import io.circe.Decoder
import monocle.Prism

import java.nio.charset.Charset
import java.util.Base64
import sangria.schema._
import sangria.validation.ValueCoercionViolation

import scala.concurrent.Future
import scala.util.Try

object Paging {

  /**
   * Giving a type to an opaque String.
   */
  final class Cursor(override val toString: String) extends AnyVal {

    def toBase64: String =
      Base64.getEncoder.encodeToString(toString.getBytes(Cursor.CharacterSet))

  }

  object Cursor {
    val CharacterSet: Charset =
      Charset.forName("UTF-8")

    def tryDecode(s: String): Try[Cursor] =
      Try {
        new Cursor(new String(Base64.getDecoder.decode(s), CharacterSet))
      }

    def decode(s: String): Option[Cursor] =
      tryDecode(s).toOption

    implicit val OrderCursor: Order[Cursor] =
      Order.by(_.toString)

    implicit val DecoderCursor: Decoder[Cursor] =
      Decoder[String].emapTry(tryDecode)

    def gid[A: Gid]: Prism[Cursor, A] =
      Prism.apply[Cursor, A](c => Gid[A].fromString.getOption(c.toString))(a => new Cursor(Gid[A].show(a)))

  }

  case object CursorViolation extends ValueCoercionViolation("Expected a base-64 encoded value")

  implicit val CursorType: ScalarType[Cursor] =
    ScalarType[Cursor](
      name          = "Cursor",
      description   = Some("Opaque object cursor"),
      coerceUserInput = {
        case s: String => Cursor.tryDecode(s).toEither.left.map(_ => CursorViolation)
        case _         => Left(CursorViolation)
      },
      coerceOutput     = (a, _) => a.toBase64,
      coerceInput      = {
        case sangria.ast.StringValue(s, _, _, _, _) => Cursor.tryDecode(s).toEither.left.map(_ => CursorViolation)
        case _                                      => Left(CursorViolation)
      }
    )

  val ArgumentPagingFirst: Argument[Option[Int]] =
    Argument(
      name         = "first",
      argumentType = OptionInputType(IntType),
      description  = "Retrieve `first` values after the given cursor"
    )

  val ArgumentPagingCursor: Argument[Option[Cursor]] =
    Argument(
      name         = "after",
      argumentType = OptionInputType(CursorType),
      description  = "Retrieve values after the one associated with this cursor"
    )

  final case class PageInfo(
    startCursor:     Option[Cursor],
    endCursor:       Option[Cursor],
    hasNextPage:     Boolean
  )

  object PageInfo {

    val Empty: PageInfo =
      PageInfo(
        startCursor     = None,
        endCursor       = None,
        hasNextPage     = false
      )

    implicit val EqPageInfo: Eq[PageInfo] =
      Eq.by { a => (
        a.startCursor,
        a.endCursor,
        a.hasNextPage
      )}

  }

  def PageInfoType[F[_]]: ObjectType[OdbRepo[F], PageInfo] =
    ObjectType(
      name        = "PageInfo",
      description = "Information that supports paging through a list of elements",
      fieldsFn    = () => fields(

        Field(
          name        = "startCursor",
          fieldType   = OptionType(CursorType),
          description = Some("Cursor pointing to the first element in the result set, if any"),
          resolve     = _.value.startCursor
        ),

        Field(
          name        = "endCursor",
          fieldType   = OptionType(CursorType),
          description = Some("Cursor pointing to the last element in the result set, if any"),
          resolve     = _.value.endCursor
        ),

        Field(
          name        = "hasNextPage",
          fieldType   = BooleanType,
          description = Some("Whether there are any pages left to retrieve"),
          resolve     = _.value.hasNextPage
        )

      )
    )

  final case class Edge[A](
    node:   A,
    cursor: Cursor
  )

  object Edge {

    implicit def EqEdge[A: Eq]: Eq[Edge[A]] =
      Eq.by { a => (
        a.node,
        a.cursor
      )}

  }

  /**
   * Simple edge type consisting of a node and a cursor.
   */
  def EdgeType[F[_], A](
    name:        String,
    description: String,
    nodeType:    OutputType[A]
  ): ObjectType[OdbRepo[F], Edge[A]] =

    ObjectType(
      name        = name,
      description = description,
      fieldsFn    = () => fields(

        Field(
          name        = "node",
          fieldType   = nodeType,
          description = Some(s"$name element"),
          resolve     = _.value.node
        ),

        Field(
          name        = "cursor",
          fieldType   = CursorType,
          description = Some(s"$name element cursor"),
          resolve     = _.value.cursor
        )
      )
    )

  final case class Connection[A](
    edges:      List[Edge[A]],
    totalCount: Int,
    pageInfo:   PageInfo
  )

  object Connection {

    def empty[A]: Connection[A] =
      Connection(Nil, 0, PageInfo.Empty)

    implicit def EqConnection[A](implicit ev: Eq[Edge[A]]): Eq[Connection[A]] =
      Eq.by { a => (
        a.edges,
        a.pageInfo
      )}

    def page[A](page: ResultPage[A])(cursorFor: A => Cursor): Connection[A] =
      Connection(
        page.nodes.map(a => Edge(a, cursorFor(a))),
        page.totalCount,
        PageInfo(
          startCursor     = page.nodes.headOption.map(cursorFor),
          endCursor       = page.nodes.lastOption.map(cursorFor),
          hasNextPage     = page.hasNextPage
        )
      )

  }

  def ConnectionType[F[_], A](
    name:        String,
    description: String,
    nodeType:    ObjectLikeType[OdbRepo[F], A],
    edgeType:    ObjectLikeType[OdbRepo[F], Edge[A]]
  ): ObjectType[OdbRepo[F], Connection[A]] =

    ObjectType(
      name        = name,
      description = description,
      fieldsFn    = () => fields(

        // A convenience field that drills down into the edges and extracts
        // just the nodes.
        Field(
          name        = "nodes",
          fieldType   = ListType(nodeType),
          description = Some("The nodes in all the edges from the current page"),
          resolve     = _.value.edges.map(_.node)
        ),

        Field(
          name        = "edges",
          fieldType   = ListType(edgeType),
          description = Some("Edges in the current page"),
          resolve     = _.value.edges
        ),

        Field(
          name        = "totalCount",
          fieldType   = IntType,
          description = Some("Count of all nodes in all pages"),
          resolve     = _.value.totalCount
        ),

        Field(
          name        = "pageInfo",
          fieldType   = PageInfoType[F],
          description = Some("Paging information"),
          resolve     = _.value.pageInfo
        )

      )
    )

  def selectPage[F[_], I, T](
    afterId:   Either[InputError, Option[I]],
    getCursor: T => Cursor,
    select:    Option[I] => F[ResultPage[T]]
  )(implicit E: ApplicativeError[F, Throwable]): F[Connection[T]] =
    afterId.fold(
      e => E.raiseError[Connection[T]](e.toException),
      g => select(g).map { page => Connection.page(page)(getCursor) }
    )

  def unsafeSelectPageFuture[F[_]: Dispatcher, I, T](
    afterId:    Either[InputError, Option[I]],
    getCursor:  T => Cursor,
    select:     Option[I] => F[ResultPage[T]]
  )(implicit ev: ApplicativeError[F, Throwable]): Future[Connection[T]] =
    implicitly[Dispatcher[F]].unsafeToFuture(selectPage[F, I, T](afterId, getCursor, select))

  def unsafeSelectTopLevelPageFuture[F[_]: Dispatcher, I: Gid, T](
    afterGid:   Either[InputError, Option[I]]
  )(
    select: Option[I] => F[ResultPage[T]]
  )(implicit ev: TopLevelModel[I, T], ev2: ApplicativeError[F, Throwable]): Future[Connection[T]] =
    unsafeSelectPageFuture[F, I, T](
      afterGid,
      t => Cursor.gid.reverseGet(TopLevelModel[I, T].id(t)),
      select
    )

}
