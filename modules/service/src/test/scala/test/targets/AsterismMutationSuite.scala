// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class AsterismMutationSuite extends OdbSuite {


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

  // Replace NGC 3312 (t-4) in o-3 and o-4 with NGC 5949 (t-2)
  queryTest(
    query ="""
      mutation ReplaceTargets($listEdit: BulkEditAsterismInput!) {
        bulkEditAsterism(input: $listEdit) {
          id
          targetEnvironment {
            asterism {
              name
            }
          }
        }
      }
    """,
    expected =json"""
      {
        "bulkEditAsterism": [
          {
            "id": "o-3",
            "targetEnvironment": {
              "asterism": [
                {
                  "name": "NGC 5949"
                }
              ]
            }
          },
          {
            "id": "o-4",
            "targetEnvironment": {
              "asterism": [
                {
                  "name": "NGC 5949"
                }
              ]
            }
          }
        ]
      }
    """,
    variables =json"""
      {
        "listEdit": {
          "select": {
            "observationIds": [ "o-3", "o-4" ]
          },
          "edit": [
            {
              "delete": "t-4"
            },
            {
              "add": "t-2"
            }
          ]
        }
      }
    """.some,
    clients = List(ClientOption.Http)  // cannot run this test twice since it changes required state

  )

}
