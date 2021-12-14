// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package test
package targets

import io.circe.literal._

class TargetQuerySuite extends OdbSuite {

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

  // Pick the first (or only in this case) science target for o-2.
  queryTest(
    query ="""
      query ScienceTargetForObservation {
        firstScienceTarget(observationId: "o-2") {
          name
        }
      }
    """,
    expected =json"""
      {
        "firstScienceTarget": {
          "name": "NGC 5949"
        }
      }
    """
  )

  // Fetch all science targets for o-6.
  queryTest(
    query ="""
      query AsterismForObservation {
        asterism(observationId: "o-6") {
          name
        }
      }
    """,
    expected =json"""
      {
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
      }
    """
  )

  // Looks up the whole target environment for o-5, including explicit base position.
  queryTest(
    query ="""
      query TargetEnvironmentForObservation {
        targetEnvironment(observationId: "o-5") {
          explicitBase {
            ra { hms }
            dec { dms }
          }
          firstScienceTarget {
            name
          }
        }
      }
    """,
    expected =json"""
      {
        "targetEnvironment": {
          "explicitBase": {
            "ra": {
              "hms": "10:37:01.992000"
            },
            "dec": {
              "dms": "-27:33:54.000000"
            }
          },
          "firstScienceTarget": {
            "name": "NGC 3312"
          }
        }
      }
    """
  )


  // Group by individual science target.
  //
  // NGC 5949 => o-2, o-6
  // NGC 3312 => o-3, o-4, o-5, o-6
  // NGC 3269 => o-6
  // NGC 4749 => <none>
/* Disabled at the moment
  queryTest(
    query ="""
      query GroupByScienceTarget {
        scienceTargetGroup(programId: "p-2") {
           observationIds
           commonTarget {
             name
           }
        }
      }
    """,
    expected = json"""
      {
        "scienceTargetGroup": [
          {
            "observationIds": [
              "o-2",
              "o-6"
            ],
            "commonTarget": {
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
            "commonTarget": {
              "name": "NGC 3312"
            }
          },
          {
            "observationIds": [
              "o-6"
            ],
            "commonTarget": {
              "name": "NGC 3269"
            }
          },
          {
            "observationIds": [
            ],
            "commonTarget": {
              "name": "NGC 4749"
            }
          }
        ]
      }
    """
  )
 */

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
                    "name" : "NGC 3269"
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
    """
  )

}
