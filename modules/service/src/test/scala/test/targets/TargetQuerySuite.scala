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

  // Pick the first alphabetically by name (or only in this case) science target
  // for o-2.
  queryTest(
    query ="""
      query ScienceTargetForObservation {
        scienceTarget(observationId: "o-2") {
          name
        }
      }
    """,
    expected =json"""
      {
        "scienceTarget": {
          "name": "NGC 5949"
        }
      }
    """
  )

  // Fetch all science targets for o-6.
  queryTest(
    query ="""
      query ScienceTargetsForObservation {
        scienceTargets(observationId: "o-6") {
          name
        }
      }
    """,
    expected =json"""
      {
        "scienceTargets": [
          {
            "name": "NGC 3269"
          },
          {
            "name": "NGC 3312"
          },
          {
            "name": "NGC 5949"
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
          science {
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
          "science": [
            {
              "name": "NGC 3312"
            }
          ]
        }
      }
    """
  )

  // Group by individual science target.
  //
  // NGC 5949 => o-2, o-6
  // NGC 3312 => o-3, o-4, o-5, o-6
  // NGC 3269 => o-6
  queryTest(
    query ="""
      query GroupByScienceTarget {
        scienceTargetGroup(programId: "p-2") {
          nodes {
             observationIds
             scienceTarget {
               name
             }
          }
        }
      }
    """,
    expected = json"""
      {
        "scienceTargetGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2",
                "o-6"
              ],
              "scienceTarget": {
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
              "scienceTarget": {
                "name": "NGC 3312"
              }
            },
            {
              "observationIds": [
                "o-6"
              ],
              "scienceTarget": {
                "name": "NGC 3269"
              }
            }
          ]
        }
      }
    """
  )

  // Group by all science targets.
  //
  // NGC 5949                     => o-2
  // NGC 3312                     => o-3, o-4, o-5
  // NGC 3269, NGC 3312, NGC 5949 => o-6
  // <nothing>                    => o-7
  queryTest(
    query ="""
      query GroupByAllScienceTargets {
        allScienceTargetsGroup(programId: "p-2") {
          nodes {
             observationIds
             allScienceTargets {
               name
             }
          }
        }
      }
    """,
    expected = json"""
      {
        "allScienceTargetsGroup": {
          "nodes": [
            {
              "observationIds": [
                "o-2"
              ],
              "allScienceTargets": [
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
              "allScienceTargets": [
                {
                  "name": "NGC 3312"
                }
              ]
            },
            {
              "observationIds": [
                "o-6"
              ],
              "allScienceTargets": [
                {
                  "name": "NGC 3269"
                },
                {
                  "name": "NGC 3312"
                },
                {
                  "name": "NGC 5949"
                }
              ]
            },
            {
              "observationIds": [
                "o-7"
              ],
              "allScienceTargets": []
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
               science {
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
              "observationIds": [
                "o-2"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": [
                  {
                    "name": "NGC 5949"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-3",
                "o-4"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": [
                  {
                    "name": "NGC 3312"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-5"
              ],
              "targetEnvironment": {
                "explicitBase": {
                  "ra": {
                    "hms": "10:37:01.992000"
                  },
                  "dec": {
                    "dms": "-27:33:54.000000"
                  }
                },
                "science": [
                  {
                    "name": "NGC 3312"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-6"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": [
                  {
                    "name": "NGC 3269"
                  },
                  {
                    "name": "NGC 3312"
                  },
                  {
                    "name": "NGC 5949"
                  }
                ]
              }
            },
            {
              "observationIds": [
                "o-7"
              ],
              "targetEnvironment": {
                "explicitBase": null,
                "science": []
              }
            }
          ]
        }
      }
    """
  )
}
