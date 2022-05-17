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
          properties {
            name
          }
          existence
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "properties": {
            "name": "Biff"
          },
          "existence": "PRESENT"
        }
      }
    """,
    variables = json"""
      {
        "cloneInput": {
          "targetId": "t-4",
          "patch": {
            "properties": {
              "name": "Biff"
            }
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  //clexistingTargetId: "t-4", suggestedCloneId: "t-a", observationIds: [ "o-3", "o-4" ]) {
  // Clone an existing target and replace it in observations 3 and 4
  queryTest(
    query ="""
      mutation CloneAndReplaceTarget($cloneInput: CloneTargetInput!) {
        cloneTarget(input: $cloneInput) {
          properties {
            name
          }
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "properties": {
            "name": "NGC 3312 (2)"
          }
        }
      }
    """,
    variables = json"""
      {
        "cloneInput": {
          "targetId": "t-4",
          "patch": {
            "properties": {
              "name": "NGC 3312 (2)"
            }
          },
          "replaceIn": [ "o-3", "o-4" ]
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
          nodes {
            observationIds
            asterism {
              properties {
                name
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "asterismGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2"
              ],
              "asterism": [
                {
                  "properties": {
                    "name": "NGC 5949"
                  }
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
                  "properties": {
                    "name": "NGC 3312 (2)"
                  }
                }
              ]
            },
            {
              "observationIds": [
                "o-5"
              ],
              "asterism": [
                {
                  "properties": {
                    "name": "NGC 3312"
                  }
                }
              ]
            },
            {
              "observationIds": [
                "o-6"
              ],
              "asterism": [
                {
                  "properties": {
                    "name": "NGC 5949"
                  }
                },
                {
                  "properties": {
                    "name": "NGC 3269"
                  }
                },
                {
                  "properties": {
                    "name": "NGC 3312"
                  }
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
