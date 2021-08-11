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

  // Rename target NGC 3312 in o-3
  queryTest(
    query ="""
      mutation UpdateScienceTarget($renameEdit: BulkEditScienceTargetInput!) {
        updateScienceTarget(input: $renameEdit) {
          id
          targets {
            science {
              name
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "updateScienceTarget": [
          {
            "id": "o-3",
            "targets": {
              "science": [
                {
                  "name": "NGC 3312*"
                }
              ]
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "renameEdit": {
          "selectObservations": [ "o-3" ],
          "edit": {
            "selectTarget": "NGC 3312",
            "sidereal": {
              "name": "NGC 3312*"
            }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state
  )

  // Renaming disallowed since it would replace an existing target.
  queryTestFailure(
    query ="""
      mutation UpdateScienceTarget($renameEdit: BulkEditScienceTargetInput!) {
        updateScienceTarget(input: $renameEdit) {
          id
          targets {
            science {
              name
            }
          }
        }
      }
    """,
    errors = List(
      "Cannot rename 'NGC 3312' to 'NGC 5949' because there is already a science target named 'NGC 5949' in observation o-6"
    ),
    variables =json"""
      {
        "renameEdit": {
          "selectObservations": [ "o-6" ],
          "edit": {
            "selectTarget": "NGC 3312",
            "sidereal": {
              "name": "NGC 5949"
            }
          }
        }
      }
    """.some
  )

  // Rename

}
