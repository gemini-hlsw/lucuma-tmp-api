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
      mutation UpdateTargets($renameUpdate: UpdateTargetsInput!) {
        updateTargets(input: $renameUpdate) {
          targets {
            id
            name
          }
        }
      }
    """,
    expected = json"""
      {
        "updateTargets": {
          "targets": [
            {
              "id": "t-4",
              "name": "NGC 3312*"
            }
          ]
        }
      }
    """,
    variables = json"""
      {
        "renameUpdate": {
          "SET": {
            "name": "NGC 3312*"
          },
          "WHERE": {
            "id": { "EQ":  "t-4" }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state
  )

}
