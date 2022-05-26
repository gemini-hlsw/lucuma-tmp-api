// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import cats.syntax.option._
import io.circe.literal._

class TargetGroupIncludeDeletedSuite extends OdbSuite {

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

  // Delete NGC 3269 (t-3)
  queryTest(
    query ="""
      mutation DeleteTarget($deleteTargetInput: DeleteTargetInput!) {
        deleteTarget(input: $deleteTargetInput) {
          id
        }
      }
    """,
    expected = json"""
      {
        "deleteTarget": [
          {
            "id": "t-3"
          }
        ]
      }
    """,
    variables = json"""
      {
        "deleteTargetInput": {
          "select": {
            "targetIds": [ "t-3" ]
          }
        }
      }
    """.some,
    clients = List(ClientOption.Http)
  )

  // Group by individual science target.
  //
  // NGC 5949 => o-2, o-6
  // NGC 3312 => o-3, o-4, o-5, o-6
  // NGC 4749 => <none>
  // XXX NGC 3269 => o-6
  queryTest(
    query ="""
      query GroupByScienceTarget {
        targetGroup(programId: "p-2") {
          nodes {
            observationIds
            target {
              name
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "targetGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2",
                "o-6"
              ],
              "target": {
                "name": "NGC 5949"
              }
            },
            {
              "observationIds": [
                "o-3",
                "o-4",
                "o-5",
                "o-6"
              ],
              "target": {
                "name": "NGC 3312"
              }
            },
            {
              "observationIds": [
              ],
              "target": {
                "name": "NGC 4749"
              }
            }
          ]
        }
      }
    """,
    clients = List(ClientOption.Http)
  )

  // Group by individual science target, includeDeleted = true
  //
  // NGC 5949 => o-2, o-6
  // NGC 3312 => o-3, o-4, o-5, o-6
  // NGC 4749 => <none>
  // XXX NGC 3269 => o-6
  queryTest(
    query ="""
      query GroupByScienceTargetIncludeDeleted {
        targetGroup(programId: "p-2", includeDeleted: true) {
          nodes {
            observationIds
            target {
              name
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "targetGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2",
                "o-6"
              ],
              "target": {
                "name": "NGC 5949"
              }
            },
            {
              "observationIds": [
                "o-6"
              ],
              "target": {
                "name": "NGC 3269"
              }
            },
            {
              "observationIds": [
                "o-3",
                "o-4",
                "o-5",
                "o-6"
              ],
              "target": {
                "name": "NGC 3312"
              }
            },
            {
              "observationIds": [
              ],
              "target": {
                "name": "NGC 4749"
              }
            }
          ]
        }
      }
    """,
    clients = List(ClientOption.Http)
  )

  // Group by asterism.
  //
  // NGC 5949                           => o-2
  // NGC 3312                           => o-3, o-4, o-5
  // (XXX NGC 3269), NGC 3312, NGC 5949 => o-6
  // <nothing>                          => o-7
  // NGC 4749                           => <none>
  queryTest(
    query ="""
      query GroupByAsterism {
        asterismGroup(programId: "p-2") {
          nodes {
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
          "nodes": [
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
                "o-4",
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
    """,
    clients = List(ClientOption.Http)
  )


  // Group by target environment, including all properties (e.g., explicit base).
  // Here o-5 is not in the same group and o-3 and o-4 even though it has the
  // same single science target because its target environment includes an
  // explicit base position.
  queryTest(
    query ="""
      query GroupByTargetEnvironment {
        targetEnvironmentGroup(programId: "p-2") {
          nodes {
            observationIds
            targetEnvironment {
              explicitBase {
                ra { hms }
                dec { dms }
              }
              asterism {
                name
              }
            }
          }
        }
      }
    """,
    expected = json"""
      {
        "targetEnvironmentGroup": {
          "nodes": [
            {
              "observationIds" : [
                "o-2"
              ],
              "targetEnvironment" : {
                "explicitBase" : null,
                "asterism" : [
                  {
                    "name" : "NGC 5949"
                  }
                ]
              }
            },
            {
              "observationIds" : [
                "o-3",
                "o-4"
              ],
              "targetEnvironment" : {
                "explicitBase" : null,
                "asterism" : [
                  {
                    "name" : "NGC 3312"
                  }
                ]
              }
            },
            {
              "observationIds" : [
                "o-5"
              ],
              "targetEnvironment" : {
                "explicitBase" : {
                  "ra" : {
                    "hms" : "10:37:01.992000"
                  },
                  "dec" : {
                    "dms" : "-27:33:54.000000"
                  }
                },
                "asterism" : [
                  {
                    "name" : "NGC 3312"
                  }
                ]
              }
            },
            {
              "observationIds" : [
                "o-6"
              ],
              "targetEnvironment" : {
                "explicitBase" : null,
                "asterism" : [
                  {
                    "name" : "NGC 5949"
                  },
                  {
                    "name" : "NGC 3312"
                  }
                ]
              }
            },
            {
              "observationIds" : [
                "o-7"
              ],
              "targetEnvironment" : {
                "explicitBase" : null,
                "asterism" : [
                ]
              }
            }
          ]
        }
      }
    """,
    clients = List(ClientOption.Http)
  )

}
