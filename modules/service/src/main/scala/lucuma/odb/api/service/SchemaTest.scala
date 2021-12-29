// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.api.service

import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.effect.std.Dispatcher
import lucuma.odb.api.schema.OdbSchema

// Does nothing but generate the schema and dump its type names.  The 'gracia'
// is getting the error message when the schema doesn't validate.
object SchemaTest extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    Dispatcher
      .apply[IO]
      .use(d => IO(OdbSchema[IO](d, Async.apply[IO])))
      .map { s =>
        println(s.types.keys.toList.sorted.mkString("\n"))
        ExitCode.Success
      }

}
