// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import io.circe.literal._

class TargetCloneSuite extends OdbSuite {

  // Clone an existing deleted, target.
  queryTest(
    query ="""
      mutation CloneTarget {
        cloneTarget(existingTargetId: "t-4", suggestedCloneId: "t-abc") {
          id
          name
          existence
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "id": "t-abc",
          "name": "NGC 3312",
          "existence": "PRESENT"
        }
      }
    """,
    None,
    clients = List(ClientOption.Http)
  )

  // Clone an existing target and replace it in observations 3 and 4
  queryTest(
    query ="""
      mutation CloneAndReplaceTarget {
        cloneTarget(existingTargetId: "t-4", suggestedCloneId: "t-a", observationIds: [ "o-3", "o-4" ]) {
          id
          name
        }
      }
    """,
    expected =json"""
      {
        "cloneTarget": {
          "id": "t-a",
          "name": "NGC 3312"
        }
      }
    """,
    None,
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
              id
              name
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
                  "id": "t-2",
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
                  "id": "t-a",
                  "name": "NGC 3312"
                }
              ]
            },
            {
              "observationIds": [
                "o-5"
              ],
              "asterism": [
                {
                  "id": "t-4",
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
                  "id": "t-2",
                  "name": "NGC 5949"
                },
                {
                  "id": "t-3",
                  "name": "NGC 3269"
                },
                {
                  "id": "t-4",
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
