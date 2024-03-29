// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class TargetCloneSuite extends OdbSuite {

  // Clone an existing deleted, target.
  queryTest(
    query ="""
      mutation CloneTarget($cloneInput: CloneTargetInput!) {
        cloneTarget(input: $cloneInput) {
          newTarget {
            name
            existence
          }
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "newTarget": {
            "name": "Biff",
            "existence": "PRESENT"
          }
        }
      }
    """,
    variables = json"""
      {
        "cloneInput": {
          "targetId": "t-4",
          "SET": {
            "name": "Biff"
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Clone an existing target and replace it in observations 3 and 4
  queryTest(
    query ="""
      mutation CloneAndReplaceTarget($cloneInput: CloneTargetInput!) {
        cloneTarget(input: $cloneInput) {
          newTarget {
            name
          }
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "newTarget": {
            "name": "NGC 3312 (2)"
          }
        }
      }
    """,
    variables = json"""
      {
        "cloneInput": {
          "targetId": "t-4",
          "SET": {
            "name": "NGC 3312 (2)"
          },
          "REPLACE_IN": [ "o-3", "o-4" ]
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Group by asterism.
  //
  // NGC 5949                     => o-2
  // NGC 3312                     => o-3, o-4, o-5
  // NGC 3269, NGC 3312, NGC 5949 => o-6
  // <nothing>                    => o-7
  // NGC 4749                     => <none>
  queryTest(
    query ="""
      query GroupByAsterism {
        asterismGroup(programId: "p-2") {
          matches {
            observationIds
            asterism {
              name
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "asterismGroup": {
          "matches": [
            {
              "observationIds": [
                "o-2"
              ],
              "asterism": [
                {
                  "name": "NGC 5949"
                }
              ]
            },
            {
              "observationIds": [
                "o-3",
                "o-4"
              ],
              "asterism": [
                {
                  "name": "NGC 3312 (2)"
                }
              ]
            },
            {
              "observationIds": [
                "o-5"
              ],
              "asterism": [
                {
                  "name": "NGC 3312"
                }
              ]
            },
            {
              "observationIds": [
                "o-6"
              ],
              "asterism": [
                {
                  "name": "NGC 5949"
                },
                {
                  "name": "NGC 3269"
                },
                {
                  "name": "NGC 3312"
                }
              ]
            },
            {
              "observationIds": [
                "o-7"
              ],
              "asterism": [
              ]
            }
          ]
        }
      }
    """
  )
}
