// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class TargetRenameMutationSuite extends OdbSuite {

  //
  // Observations and their targets:
  //
  // o-2: NGC 5949
  // o-3: NGC 3312
  // o-4: NGC 3312
  // o-5: NGC 3312 (explicit base)
  // o-6: NGC 5949, NGC 3269, NGC 3312
  // o-7: <none>
  //

  // Rename target "NGC 3312" to "NGC 3312*" in o-3.
  queryTest(
    query ="""
      mutation UpdateScienceTarget($renameEdit: BulkEditTargetInput!) {
        updateScienceTarget(input: $renameEdit) {
          observation {
            id
          }
          edits {
            op
            target {
              name
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "updateScienceTarget": [
          {
            "observation": {
              "id": "o-3"
            },
            "edits": [
              {
                "op": "EDIT",
                "target": {
                  "name": "NGC 3312*"
                }
              }
            ]
          }
        ]
      }
    """,
    variables = json"""
      {
        "renameEdit": {
          "select": {
            "observations": [ "o-3" ]
          },
          "editSidereal": {
            "select": {
              "names": [ "NGC 3312" ]
            },
            "name": "NGC 3312*"
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state
  )

}
