// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class TargetRenameMutationSuite extends OdbSuite {

  // Rename target "NGC 3312" to "NGC 3312*".
  queryTest(
    query ="""
      mutation UpdateTarget($renameEdit: EditTargetInput!) {
        updateTarget(input: $renameEdit) {
          id
          name
        }
      }
    """,
    expected = json"""
      {
        "updateTarget": {
          "id": "t-4",
          "name": "NGC 3312*"
        }
      }
    """,
    variables = json"""
      {
        "renameEdit": {
          "targetId": "t-4",
          "sidereal" : {
            "name": "NGC 3312*"
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state
  )

}
